# almighty-bolt 0.2 release notes

*Under development*

Welcome to almighty-bolt 0.2, in development. It starts with VoiceOver: a window of almighty-bolt can be read by the macOS screen reader. A scroll container, a list that builds only the rows that show, a text area, and copy and paste came next. See [what's new](#whats-new-in-02).

<!-- highlights:start -->
## Highlights

- Accessibility: VoiceOver reads every widget by what it is, what it is called and what it holds, can press buttons, check boxes, options and rows, move a slider and give a field the keyboard, is told when things change, and reads out a `:live` label or paragraph whenever its text changes.
<!-- highlights:end -->

## Platform and compatibility

- macOS on Apple silicon, with Metal. SBCL 2.6. One window, one fixed dark theme.
- Assistive tools are told what changes once they have asked what the window shows (or the system says VoiceOver or Switch Control is running).

## What's new in 0.2

### VoiceOver reads the screen

A window of almighty-bolt draws all of itself in one view, so to the system it was one blank rectangle. It now hands out what the [semantic tree](../../README.md#working-an-app-from-a-program-or-an-agent) says: each widget is an element with a role, a name, a value, an enabled state and a place on the screen, found again as the same element from frame to frame (so that VoiceOver's cursor stays where it was). The containers that only lay things out are left out.

| Widget | Is read as |
|---|---|
| label, paragraph | static text, saying its words |
| button | button |
| text field | text field, holding its text |
| text area | text area, holding its text |
| check box | check box, checked or not |
| slider | slider, with its value and its ends |
| progress bar | progress indicator, from 0 to 1 |
| choice | radio group, with a radio button for each option and the chosen one marked |
| list | list, with a row for each row that is showing and the picked one marked |
| canvas, scrim | group |

What VoiceOver can do with them:

- Read each one by its role, its name and its value: a button called Count, a text field holding Ada, a check box called Read the README that is checked.
- Press a button, a check box, an option of a choice or a row of a list (VO-Space). The press arrives as the events a hand makes (a move, a press and a release in the middle of the thing), so the app answers as it does to a click. A disabled one refuses.
- Move a slider up or down a step, the step of an arrow key (a twentieth of its range), by pressing where the value then is.
- Ask for the keyboard in a text field. Typing then goes to it.
- Find what is under the pointer, and know which thing has the keyboard.
- Be told, as they happen, when a value changes, when things come or go (a list that scrolls shows other rows), when a thing moves or changes size (VoiceOver draws its cursor where it was last told a thing is, so a button pushed along by a count that grew a digit keeps its cursor with it), when the picked row of a list changes, and when the keyboard moves. VoiceOver says a changed value only for the thing its cursor is on.
- Hear a `:live` label or paragraph whenever its text changes, wherever the cursor is.

Nothing is worked out for this until something asks what the window shows (VoiceOver, the Accessibility Inspector) or the system says that VoiceOver or Switch Control is running; from the first question on, what changes is told as it happens. A window that nothing has asked about costs what it did, and one at rest does no work.

A count that changes when a button elsewhere is pressed is not said by VoiceOver, since its cursor is on the button. Say that the text matters with `:live`: `(bolt:label (princ-to-string count) :id :count :live t)` is read out as the new count after each press. It is for a result, a count or a status that a person should hear without going to look for it, not for text that changes every frame. A live text that has just appeared is not read out, only one that changes, so a status that comes and goes is better as a label that is always there and sometimes empty.

A list is read as the rows that are showing, so that scrolling it changes which ones there are; a screen reader is not told of the rows out of sight. What a canvas draws is not read: it is a group, called by its label if it has one.

### A scroll container

`scroll` shows what is inside it, one under another, in a window that the wheel or a trackpad moves up and down:

```lisp
(bolt:scroll (:id :log :height 300)
  (dolist (line lines)
    (bolt:label (line-text line) :id (line-id line))))
```

- The window is `:height` points tall, or takes a share of a column's free space with `:flex`. The `:id` is required: the place the window is at is kept by it, across frames and across a reload.
- The wheel goes first to what is inside (a `list-box` in a `scroll` scrolls its own rows), and what is left of it moves the window while the pointer is over the container or anything in it.
- Tab brings the keyboard to something out of the window, and the window moves to show it.
- What is out of the window cannot be pressed. In the semantic tree it is `:hidden`, and the machine interface refuses to click it until it has been scrolled into sight.

Moving the window lays the screen out again: a wheel tick costs about 0.06 ms with 50 rows of a label and a button, and 2.8 ms with 2000. For long lists there is `lazy-list`, below.

VoiceOver reads what is inside a `scroll` as it reads what is inside a column, the rows out of the window included, and it does not yet scroll the window to the one it is on.

### A list that builds only the rows that show

`lazy-list` is a scrolling list whose rows are any widgets, written like `dolist`. Its rows are all one height, so it knows which of them show before it builds any, and it describes only those: a wheel tick costs about 0.04 ms whether the list has a hundred rows or a million.

```lisp
(bolt:lazy-list (task tasks :id :tasks :key #'task-id :row-height 28 :height 300)
  (bolt:row (:gap 8)
    (bolt:checkbox (task-text task) (task-done task) :id :done)))
```

- `:key` gives each item's id. What the ui keeps for a row (the caret of a field in it, the keyboard) stays with its item when the items are sorted again or one is put in above it. Without `:key`, a row is known by its index.
- The items are best a vector: a row is found by its index.
- The wheel, the bar at the edge, and what cannot be pressed out of the window are as for `scroll`. Tab reaches only the rows that are described.

### A text area

`text-area` is a text field of several lines. It is called as `text-field` is, and returns the text and whether the person changed it:

```lisp
(multiple-value-bind (text changed) (bolt:text-area notes :id :notes :width 320 :rows 6)
  (when changed (setf notes text)))
```

- Return puts in a line feed. A line too long for the width goes on in the next.
- Up and Down move a line, keeping to where across the first of them started. Command-Up and Command-Down go to the start and the end of the text, Command-Left and Command-Right to the start and the end of the line. The arrows with Shift select.
- Copy, cut and paste work as in a text field, and a pasted line break stays a line break.
- It scrolls itself to keep the caret in sight, and the wheel scrolls it.
- VoiceOver reads it as a text area holding its text. The machine interface types a line feed as Return.

### Minor features

#### Text fields

- Command-C, Command-X and Command-V copy, cut and paste in a text field, through the system's clipboard. A line break that is pasted becomes a space, since the field holds one line. Copy and cut do nothing when nothing is selected.

#### Machine interface

- A widget's `:id` is its element's accessibility identifier, so the id that an agent or a test finds a widget by is what the Accessibility Inspector shows for it.
