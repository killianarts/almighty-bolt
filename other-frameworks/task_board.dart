// task_board.dart — Flutter version of task-board.lisp, as an API sketch.
//
// Not built or run. In a real project this would be lib/main.dart in an
// app created with `flutter create`.

import 'package:flutter/material.dart';

void main() => runApp(const TaskBoardApp());

// Model. Tasks are immutable values; "changing" one swaps in a copy with
// the same id. The id is what keeps a row's identity across rebuilds,
// like :UNIQUE-ID in the McCLIM version.

@immutable
class Task {
  const Task({
    required this.id,
    required this.title,
    this.done = false,
    this.urgent = false,
  });

  final int id;
  final String title;
  final bool done;
  // Dart has no CHANGE-CLASS, so "urgent" is a flag, not a subclass.
  final bool urgent;

  Task copyWith({bool? done, bool? urgent}) => Task(
        id: id,
        title: title,
        done: done ?? this.done,
        urgent: urgent ?? this.urgent,
      );
}

// The board's state. NOTIFYLISTENERS plays the part of CLIM's command loop
// calling the display function again after each command.

class TaskBoard extends ChangeNotifier {
  int _nextId = 0;

  late final List<Task> _tasks = [
    _make('Write docs'),
    _make('Fix prod bug', urgent: true),
    _make('Lunch'),
  ];

  List<Task> get tasks => List.unmodifiable(_tasks);

  Task _make(String title, {bool urgent = false}) =>
      Task(id: _nextId++, title: title, urgent: urgent);

  void add(String title) {
    _tasks.add(_make(title));
    notifyListeners();
  }

  void toggle(Task task) => _update(task, (t) => t.copyWith(done: !t.done));

  void setUrgent(Task task, bool urgent) =>
      _update(task, (t) => t.copyWith(urgent: urgent));

  void delete(Task task) {
    _tasks.removeWhere((t) => t.id == task.id);
    notifyListeners();
  }

  void _update(Task task, Task Function(Task) change) {
    final i = _tasks.indexWhere((t) => t.id == task.id);
    if (i == -1) return;
    _tasks[i] = change(_tasks[i]);
    notifyListeners();
  }
}

// Makes the board available to every widget below it, and rebuilds the
// widgets that read it whenever it notifies. Roughly *APPLICATION-FRAME*,
// but scoped to a subtree and change-aware.

class TaskBoardScope extends InheritedNotifier<TaskBoard> {
  const TaskBoardScope({
    super.key,
    required TaskBoard board,
    required super.child,
  }) : super(notifier: board);

  static TaskBoard of(BuildContext context) =>
      context.dependOnInheritedWidgetOfExactType<TaskBoardScope>()!.notifier!;
}

// App root. Stateful only so it can own the board and dispose of it.

class TaskBoardApp extends StatefulWidget {
  const TaskBoardApp({super.key});

  @override
  State<TaskBoardApp> createState() => _TaskBoardAppState();
}

class _TaskBoardAppState extends State<TaskBoardApp> {
  final board = TaskBoard();

  @override
  void dispose() {
    board.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) => TaskBoardScope(
        board: board,
        child: const MaterialApp(
          title: 'Task Board',
          home: TaskBoardPage(),
        ),
      );
}

// The page: the list of tasks (the McCLIM "board" pane) above an input
// field (standing in for the interactor).

class TaskBoardPage extends StatelessWidget {
  const TaskBoardPage({super.key});

  @override
  Widget build(BuildContext context) {
    // Reading the scope here subscribes this widget to board changes.
    final board = TaskBoardScope.of(context);

    return Scaffold(
      appBar: AppBar(title: const Text('Task Board')),
      body: Column(
        children: [
          Expanded(
            child: ListView(
              children: [
                for (final task in board.tasks)
                  // The key matches each row to its Element from the last
                  // build, so deleting a task removes one row instead of
                  // shifting every row's state onto its neighbour.
                  TaskRow(
                    key: ValueKey(task.id),
                    task: task,
                    onToggle: () => board.toggle(task),
                    onSetUrgent: (urgent) => board.setUrgent(task, urgent),
                    onDelete: () => board.delete(task),
                  ),
              ],
            ),
          ),
          Padding(
            padding: const EdgeInsets.all(8),
            child: AddTaskField(onSubmit: board.add),
          ),
        ],
      ),
    );
  }
}

// One task. It knows nothing about TaskBoard: behaviour comes in as
// callbacks, which is how Flutter widgets stay reusable. Compare the McCLIM
// version, where behaviour is attached to the TASK type by translators.

enum _TaskAction { escalate, downgrade, delete }

class TaskRow extends StatelessWidget {
  const TaskRow({
    super.key,
    required this.task,
    required this.onToggle,
    required this.onSetUrgent,
    required this.onDelete,
  });

  final Task task;
  final VoidCallback onToggle;
  final ValueChanged<bool> onSetUrgent;
  final VoidCallback onDelete;

  @override
  Widget build(BuildContext context) {
    final base = Theme.of(context).textTheme.bodyLarge;
    final style = base?.copyWith(
      color: task.urgent ? Colors.red : null,
      fontWeight: task.urgent ? FontWeight.bold : null,
      decoration: task.done ? TextDecoration.lineThrough : null,
    );

    return ListTile(
      leading: Checkbox(value: task.done, onChanged: (_) => onToggle()),
      title: Text(task.title, style: style),
      onTap: onToggle,
      // The menu's items depend on the task, like the :TESTER on the
      // escalate translator and the URGENT-TASK-only downgrade translator.
      trailing: PopupMenuButton<_TaskAction>(
        onSelected: (action) {
          switch (action) {
            case _TaskAction.escalate:
              onSetUrgent(true);
            case _TaskAction.downgrade:
              onSetUrgent(false);
            case _TaskAction.delete:
              onDelete();
          }
        },
        itemBuilder: (context) => [
          if (!task.urgent)
            const PopupMenuItem(
              value: _TaskAction.escalate,
              child: Text('Escalate'),
            ),
          if (task.urgent)
            const PopupMenuItem(
              value: _TaskAction.downgrade,
              child: Text('Downgrade'),
            ),
          const PopupMenuItem(
            value: _TaskAction.delete,
            child: Text('Delete'),
          ),
        ],
      ),
    );
  }
}

// Text field for new tasks (the "Add Task" command). Stateful because it
// owns a TextEditingController, which must outlive individual builds.

class AddTaskField extends StatefulWidget {
  const AddTaskField({super.key, required this.onSubmit});

  final ValueChanged<String> onSubmit;

  @override
  State<AddTaskField> createState() => _AddTaskFieldState();
}

class _AddTaskFieldState extends State<AddTaskField> {
  final _controller = TextEditingController();

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  void _submit() {
    final title = _controller.text.trim();
    if (title.isEmpty) return;
    widget.onSubmit(title);
    _controller.clear();
  }

  @override
  Widget build(BuildContext context) => TextField(
        controller: _controller,
        decoration: InputDecoration(
          hintText: 'New task',
          suffixIcon: IconButton(
            icon: const Icon(Icons.add),
            onPressed: _submit,
          ),
        ),
        onSubmitted: (_) => _submit(),
      );
}
