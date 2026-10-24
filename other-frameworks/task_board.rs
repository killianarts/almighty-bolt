//! task_board.rs — GPUI version of task-board.lisp and task_board.dart, as
//! an API sketch.
//!
//! Not built or run. GPUI's API changes often, so some names may differ in
//! whichever version you'd pin. In a real project this would be src/main.rs
//! with `gpui` as a git dependency on the Zed repo.

use gpui::{
    actions, anchored, deferred, div, prelude::*, rgb, Action, App, Application, ClickEvent,
    Context, Entity, FocusHandle, FontWeight, KeyBinding, MouseButton, MouseDownEvent, Pixels,
    Point, SharedString, Subscription, Window, WindowOptions,
};

// Actions: named commands, separate from any widget. Like CLIM commands,
// the same action can come from a key binding or a menu (in Zed, also the
// command palette). These carry no data and act on the selected task, so
// "selection" stands in for CLIM's command argument.

actions!(
    task_board,
    [SelectNext, SelectPrevious, ToggleDone, Escalate, Downgrade, DeleteTask, AddTask]
);

// Model

#[derive(Clone)]
struct Task {
    id: usize,
    title: SharedString,
    done: bool,
    urgent: bool,
}

// The board view. The App owns this struct; everything else reaches it
// through an Entity<TaskBoard> handle.

struct TaskBoard {
    tasks: Vec<Task>,
    next_id: usize,
    // Ids, not indices, so they stay valid when tasks are deleted.
    selected: Option<usize>,
    // Where the context menu is open, if it is.
    menu: Option<Point<Pixels>>,
    focus_handle: FocusHandle,
}

impl TaskBoard {
    fn new(window: &mut Window, cx: &mut Context<Self>) -> Self {
        // Actions are dispatched from the focused element upward, so the
        // board must hold focus for its key bindings to fire.
        let focus_handle = cx.focus_handle();
        window.focus(&focus_handle);

        let mut board = Self {
            tasks: Vec::new(),
            next_id: 0,
            selected: None,
            menu: None,
            focus_handle,
        };
        board.push("Write docs", false);
        board.push("Fix prod bug", true);
        board.push("Lunch", false);
        board
    }

    fn push(&mut self, title: &str, urgent: bool) {
        self.tasks.push(Task {
            id: self.next_id,
            title: SharedString::from(title.to_string()),
            done: false,
            urgent,
        });
        self.next_id += 1;
    }

    fn selected_index(&self) -> Option<usize> {
        let id = self.selected?;
        self.tasks.iter().position(|t| t.id == id)
    }

    fn select_index(&mut self, ix: usize, cx: &mut Context<Self>) {
        if let Some(task) = self.tasks.get(ix) {
            self.selected = Some(task.id);
            cx.notify();
        }
    }

    fn update_selected(&mut self, cx: &mut Context<Self>, change: impl FnOnce(&mut Task)) {
        if let Some(ix) = self.selected_index() {
            change(&mut self.tasks[ix]);
            self.menu = None;
            // Marks this view dirty: its render runs again, and anything
            // observing the board (the status bar) is told.
            cx.notify();
        }
    }

    // Action handlers. Each receives the action value, even though these
    // actions carry no data.

    fn select_next(&mut self, _: &SelectNext, _: &mut Window, cx: &mut Context<Self>) {
        let last = self.tasks.len().saturating_sub(1);
        let ix = self.selected_index().map_or(0, |ix| (ix + 1).min(last));
        self.select_index(ix, cx);
    }

    fn select_previous(&mut self, _: &SelectPrevious, _: &mut Window, cx: &mut Context<Self>) {
        let ix = self.selected_index().map_or(0, |ix| ix.saturating_sub(1));
        self.select_index(ix, cx);
    }

    fn toggle_done(&mut self, _: &ToggleDone, _: &mut Window, cx: &mut Context<Self>) {
        self.update_selected(cx, |t| t.done = !t.done);
    }

    fn escalate(&mut self, _: &Escalate, _: &mut Window, cx: &mut Context<Self>) {
        self.update_selected(cx, |t| t.urgent = true);
    }

    fn downgrade(&mut self, _: &Downgrade, _: &mut Window, cx: &mut Context<Self>) {
        self.update_selected(cx, |t| t.urgent = false);
    }

    fn delete_task(&mut self, _: &DeleteTask, _: &mut Window, cx: &mut Context<Self>) {
        if let Some(ix) = self.selected_index() {
            self.tasks.remove(ix);
            // Select the task that moved into its place, or the new last one.
            self.selected = self.tasks.get(ix).or(self.tasks.last()).map(|t| t.id);
            self.menu = None;
            cx.notify();
        }
    }

    fn add_task(&mut self, _: &AddTask, _: &mut Window, cx: &mut Context<Self>) {
        // GPUI has no built-in text field (Zed builds its own Editor), so
        // new tasks get a placeholder title.
        let title = format!("Task {}", self.next_id + 1);
        self.push(&title, false);
        self.selected = Some(self.next_id - 1);
        cx.notify();
    }

    fn render_menu(
        &self,
        position: Point<Pixels>,
        task: &Task,
        cx: &mut Context<Self>,
    ) -> impl IntoElement {
        // Menu items dispatch the same actions as the key bindings, so the
        // menu has no logic of its own. Which items appear depends on the
        // task, like the :TESTER on the McCLIM escalate translator.
        let item = |label: &'static str, action: Box<dyn Action>| {
            div()
                .id(label)
                .px_2()
                .py_1()
                .hover(|style| style.bg(rgb(0xe0e0e0)))
                .child(label)
                .on_click(move |_, window, cx| window.dispatch_action(action.boxed_clone(), cx))
        };

        // DEFERRED paints the menu after everything else, and ANCHORED
        // positions it at the click and keeps it inside the window.
        deferred(
            anchored().position(position).child(
                div()
                    .flex()
                    .flex_col()
                    .bg(rgb(0xffffff))
                    .border_1()
                    .border_color(rgb(0xcccccc))
                    .rounded_md()
                    .shadow_md()
                    .occlude()
                    .when(!task.urgent, |menu| menu.child(item("Escalate", Box::new(Escalate))))
                    .when(task.urgent, |menu| menu.child(item("Downgrade", Box::new(Downgrade))))
                    .child(item("Delete", Box::new(DeleteTask)))
                    .on_mouse_down_out(cx.listener(|this, _, _, cx| {
                        this.menu = None;
                        cx.notify();
                    })),
            ),
        )
    }
}

impl Render for TaskBoard {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let menu = self
            .menu
            .zip(self.selected_index())
            .map(|(position, ix)| self.render_menu(position, &self.tasks[ix], cx));

        div()
            // Key bindings registered for "TaskBoard" apply only while focus
            // is inside this element.
            .key_context("TaskBoard")
            .track_focus(&self.focus_handle)
            .on_action(cx.listener(Self::select_next))
            .on_action(cx.listener(Self::select_previous))
            .on_action(cx.listener(Self::toggle_done))
            .on_action(cx.listener(Self::escalate))
            .on_action(cx.listener(Self::downgrade))
            .on_action(cx.listener(Self::delete_task))
            .on_action(cx.listener(Self::add_task))
            .flex_1()
            .flex()
            .flex_col()
            .gap_1()
            .p_4()
            .children(self.tasks.iter().map(|task| {
                let id = task.id;
                TaskRow {
                    task: task.clone(),
                    selected: self.selected == Some(id),
                    // Left click selects and toggles. Action handlers are
                    // ordinary methods, so they can be called directly.
                    on_click: Box::new(cx.listener(move |this, _, window, cx| {
                        this.selected = Some(id);
                        this.toggle_done(&ToggleDone, window, cx);
                    })),
                    on_right_click: Box::new(cx.listener(
                        move |this, event: &MouseDownEvent, _, cx| {
                            this.selected = Some(id);
                            this.menu = Some(event.position);
                            cx.notify();
                        },
                    )),
                }
            }))
            .children(menu)
    }
}

// One task row: a stateless component, consumed when rendered. Like the
// Flutter TaskRow, it gets its behaviour as callbacks and knows nothing
// about TaskBoard.

#[derive(IntoElement)]
struct TaskRow {
    task: Task,
    selected: bool,
    on_click: Box<dyn Fn(&ClickEvent, &mut Window, &mut App)>,
    on_right_click: Box<dyn Fn(&MouseDownEvent, &mut Window, &mut App)>,
}

impl RenderOnce for TaskRow {
    fn render(self, _window: &mut Window, _cx: &mut App) -> impl IntoElement {
        let task = self.task;
        div()
            // Anything clickable needs a stable id, so GPUI can track hover
            // and press state for it between frames.
            .id(("task", task.id))
            .px_2()
            .py_1()
            .rounded_sm()
            .cursor_pointer()
            .when(self.selected, |row| row.bg(rgb(0xdde8ff)))
            .when(task.urgent, |row| {
                row.text_color(rgb(0xcc0000)).font_weight(FontWeight::BOLD)
            })
            .when(task.done, |row| row.line_through())
            .child(format!("[{}] {}", if task.done { "x" } else { " " }, task.title))
            .on_click(self.on_click)
            .on_mouse_down(MouseButton::Right, self.on_right_click)
    }
}

// A second view that depends on the board. It re-renders whenever the board
// notifies, without the board knowing it exists.

struct StatusBar {
    board: Entity<TaskBoard>,
    // Dropping the subscription stops the observation, so it lives as long
    // as the StatusBar does.
    _observe_board: Subscription,
}

impl StatusBar {
    fn new(board: Entity<TaskBoard>, cx: &mut Context<Self>) -> Self {
        let _observe_board = cx.observe(&board, |_, _, cx| cx.notify());
        Self { board, _observe_board }
    }
}

impl Render for StatusBar {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let board = self.board.read(cx);
        let done = board.tasks.iter().filter(|t| t.done).count();
        div()
            .px_4()
            .py_1()
            .border_t_1()
            .border_color(rgb(0xcccccc))
            .text_sm()
            .child(format!("{done} of {} done", board.tasks.len()))
    }
}

// Root view. It holds handles to its child views and places them in its
// tree; each child still re-renders on its own.

struct TaskBoardWindow {
    board: Entity<TaskBoard>,
    status: Entity<StatusBar>,
}

impl Render for TaskBoardWindow {
    fn render(&mut self, _window: &mut Window, _cx: &mut Context<Self>) -> impl IntoElement {
        div()
            .size_full()
            .flex()
            .flex_col()
            .bg(rgb(0xfafafa))
            .text_color(rgb(0x202020))
            .child(self.board.clone())
            .child(self.status.clone())
    }
}

fn main() {
    Application::new().run(|cx: &mut App| {
        cx.bind_keys([
            KeyBinding::new("down", SelectNext, Some("TaskBoard")),
            KeyBinding::new("up", SelectPrevious, Some("TaskBoard")),
            KeyBinding::new("space", ToggleDone, Some("TaskBoard")),
            KeyBinding::new("cmd-up", Escalate, Some("TaskBoard")),
            KeyBinding::new("cmd-down", Downgrade, Some("TaskBoard")),
            KeyBinding::new("cmd-backspace", DeleteTask, Some("TaskBoard")),
            KeyBinding::new("cmd-n", AddTask, Some("TaskBoard")),
        ]);

        cx.open_window(WindowOptions::default(), |window, cx| {
            let board = cx.new(|cx| TaskBoard::new(window, cx));
            let status = cx.new(|cx| StatusBar::new(board.clone(), cx));
            cx.new(|_| TaskBoardWindow { board, status })
        })
        .unwrap();

        cx.activate(true);
    });
}
