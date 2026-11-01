# Roadmap

What is planned after [0.1](releases/0.1.md). These are plans, not promises, and the order inside a section is not a schedule. What has shipped is in the [release notes](releases/) and the [change log](../CHANGELOG.md).

## Next

- **Spacing in a theme.** Paddings, gaps, and the heights of rows and controls, set by a theme as its colours, font and corners are.

## Later

- **Images and icons.**
- **Windows.** More than one window, control over a window's title, size limits and full screen, and an app's own menus in the menu bar.
- **Text and layout.**
  - Bold, italic and a font for each widget; rich text.
  - Grid and wrapping layouts, minimum and maximum sizes, and splitters.
  - Tabs, a tree view, a table, and date and colour pickers.
- **Effects.** Shadows, glows, blur, opacity and other filters.
  - An effect can reach past the edges of the widget it belongs to, by as far as it needs.
  - A soft shadow or glow is drawn in the shader. A blur or filter reads what is under it, so it is drawn to a texture off screen first.
  - A scroll's clip lets each effect's reach through, as it does now for the focus ring, which becomes one effect among the others.
- **Keeping pixels.** The renderer keeps the pixels it drew, in tiles, and repaints only the tiles whose drawing commands changed. This is how the painter of the [Blick](https://blickeditor.com/blog/caching-an-immediate-mode-ui) video editor works. It saves the GPU work, and the battery, of frames where only a caret blinks or an animation runs, and of expensive effects that did not change.
  - A command's area includes the reach of its effect. When a glow changes, every tile it reaches is repainted, not only the tile of its widget.
  - The cached frames are compared with frames painted without the cache, and must match exactly.
- **Interaction.**
  - Drag and drop, inside an app and from the Finder.
  - Animation helpers (easing and tweens).
  - Undo and redo of the app's own changes (text fields undo their own already).
- **Tools.**
  - An inspector: click a widget in the running window to see what it is, and change it.
  - An API for writing your own widgets, which says how a widget appears in the semantic tree.
  - A widget designer, to build new widgets.
  - A GUI designer, to build a screen by placing widgets instead of writing code.
- **Widgets made with the widget designer.** These come after the designer and are built with it, to test it: a number stepper, a divider and a spacer, a collapsible section, a link, chips, badges and cards, and a toolbar, a sidebar and a status bar.
- **Delivery.** An application bundle (`.app`) with an icon, and signing; and a helper for work that goes on in the background while the window stays alive.
- **Platforms.** Windows and Linux. A Coalton library.
