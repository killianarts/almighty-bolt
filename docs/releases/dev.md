# almighty-bolt 0.2 release notes

*Under development*

Welcome to almighty-bolt 0.2, in development. It starts with VoiceOver: a window of almighty-bolt can be read by the macOS screen reader. See [what's new](#whats-new-in-02).

<!-- highlights:start -->
## Highlights

- Accessibility: VoiceOver reads static text, buttons, text fields and check boxes by what they are, what they are called and what they hold, and can press a button or a check box and give a field the keyboard.
<!-- highlights:end -->

## Platform and compatibility

- macOS on Apple silicon, with Metal. SBCL 2.6. One window, one fixed dark theme.
- VoiceOver and Switch Control are told what changes while they run. Other assistive tools read the window when they ask.

## What's new in 0.2

### VoiceOver reads the screen

A window of almighty-bolt draws all of itself in one view, so to the system it was one blank rectangle. It now hands out what the [semantic tree](../../README.md#working-an-app-from-a-program-or-an-agent) says: each static text, button, text field and check box is an element with a role, a name, a value, an enabled state and a place on the screen, found again as the same element from frame to frame (so that VoiceOver's cursor stays where it was). The containers that only lay things out are left out.

What VoiceOver can do with them:

- Read each one by its role, its name and its value: a button called Count, a text field holding Ada, a check box called Read the README that is checked.
- Press a button or a check box (VO-Space). The press arrives as the events a hand makes (a move, a press and a release in the middle of the thing), so the app answers as it does to a click. A disabled one refuses.
- Ask for the keyboard in a text field. Typing then goes to it.
- Find what is under the pointer, and know which thing has the keyboard.
- Be told, as they happen, when a value changes, when things come or go, and when the keyboard moves.

Nothing is worked out for this unless VoiceOver or Switch Control is running, or the system asks: a window at rest costs what it did.

Sliders, progress bars, choices, lists, canvases and scrims are not read yet.

### Minor features

#### Machine interface

- A widget's `:id` is its element's accessibility identifier, so the id that an agent or a test finds a widget by is what the Accessibility Inspector shows for it.
