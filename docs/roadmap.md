# Roadmap

What is planned after [0.1](releases/0.1.md). These are plans, not promises, and the order inside a section is not a schedule. What has shipped is in the [release notes](releases/) and the [change log](../CHANGELOG.md).

## Next

- **VoiceOver.** The screen is already a semantic tree of roles, names, values and states; a bridge lets the macOS screen reader read it.
- **Everyday text and scrolling.** Copy and paste in text fields (the clipboard), a scroll container for anything, and a multi-line text field.
- **Theming.** Light and dark (following the system setting) and custom themes, for the widgets that exist, and for a part of a screen as well as the whole.
- **Overlays.** Dropdowns, menus and context menus, tooltips and popovers, modal dialogs, and the system's file open and save dialogs.
- **Images and icons.**

## Later

- **Windows.** More than one window, control over a window's title, size limits and full screen, and an API for the menu bar.
- **Text and layout.**
  - Bold, italic and a font for each widget; rich text.
  - Grid and wrapping layouts, minimum and maximum sizes, and splitters.
  - Tabs, a tree view, a table, and date and colour pickers.
- **Interaction.**
  - Mouse cursor shapes.
  - Drag and drop, inside an app and from the Finder.
  - Keyboard shortcuts.
  - Animation helpers (easing and tweens).
  - Undo and redo.
- **Tools.**
  - An inspector: click a widget in the running window to see what it is, and change it.
  - An API for writing your own widgets, which says how a widget appears in the semantic tree.
  - A widget designer, to build new widgets.
  - A GUI designer, to build a screen by placing widgets instead of writing code.
- **Delivery.** An application bundle (`.app`) with an icon, and signing; and a helper for work that goes on in the background while the window stays alive.
- **Platforms.** Windows and Linux. A Coalton library.
