# almighty-bolt 0.2 release notes

*Under development*

Welcome to almighty-bolt 0.2, in development. It starts with VoiceOver: a window of almighty-bolt can be read by the macOS screen reader. See [what's new](#whats-new-in-02).

<!-- highlights:start -->
## Highlights

- Accessibility: VoiceOver reads every widget by what it is, what it is called and what it holds, can press buttons, check boxes, options and rows, move a slider and give a field the keyboard, and is told when things change.
<!-- highlights:end -->

## Platform and compatibility

- macOS on Apple silicon, with Metal. SBCL 2.6. One window, one fixed dark theme.
- VoiceOver and Switch Control are told what changes while they run. Other assistive tools read the window when they ask.

## What's new in 0.2

### VoiceOver reads the screen

A window of almighty-bolt draws all of itself in one view, so to the system it was one blank rectangle. It now hands out what the [semantic tree](../../README.md#working-an-app-from-a-program-or-an-agent) says: each widget is an element with a role, a name, a value, an enabled state and a place on the screen, found again as the same element from frame to frame (so that VoiceOver's cursor stays where it was). The containers that only lay things out are left out.

| Widget | Is read as |
|---|---|
| label, paragraph | static text, saying its words |
| button | button |
| text field | text field, holding its text |
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
- Be told, as they happen, when a value changes, when things come or go (a list that scrolls shows other rows), when the picked row of a list changes, and when the keyboard moves.

Nothing is worked out for this unless VoiceOver or Switch Control is running, or the system asks: a window at rest costs what it did.

A list is read as the rows that are showing, so that scrolling it changes which ones there are; a screen reader is not told of the rows out of sight. What a canvas draws is not read: it is a group, called by its label if it has one.

### Minor features

#### Machine interface

- A widget's `:id` is its element's accessibility identifier, so the id that an agent or a test finds a widget by is what the Accessibility Inspector shows for it.
