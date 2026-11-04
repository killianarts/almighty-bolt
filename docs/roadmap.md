# Roadmap

What is planned after [0.1](releases/0.1.md). These are plans, not promises, and
the order inside a section is not a schedule. What has shipped is in the
[release notes](releases/) and the [change log](../CHANGELOG.md).

## Next

- **Windows.** Control over a window's title, size limits and full screen, and
  an app's own menus in the menu bar. (More than one window is in 0.3.)

## Later

- **Images and icons.**
- **Text and layout.**
  - Italic, weights between regular and bold, a font for each widget besides
    labels and paragraphs (which have `:weight :bold` and `:font`); rich text.
  - Wrapping layouts and splitters, and panes that can be docked, resized, put
    in tabs and moved.
  - Tabs, a tree view, a table, and date and colour pickers (a segmented
    `choice` switches between views until there are tabs).
- **Effects.** Shadows, glows, blur, opacity and other filters.
  - An effect can reach past the edges of the widget it belongs to, by as far as
    it needs.
  - A soft shadow or glow is drawn in the shader. A blur or filter reads what is
    under it, so it is drawn to a texture off screen first.
  - A scroll's clip lets each effect's reach through, as it does now for the
    focus ring, which becomes one effect among the others.
- **Keeping pixels.** The renderer keeps the pixels it drew, in tiles, and
  repaints only the tiles whose drawing commands changed. It saves the GPU work, and the battery, of frames where only a
  caret blinks or an animation runs, and of expensive effects that did not
  change.
  - A command's area includes the reach of its effect. When a glow changes,
    every tile it reaches is repainted, not only the tile of its widget.
  - The cached frames are compared with frames painted without the cache, and
    must match exactly.
- **Interaction.**
  - Drag and drop, inside an app and from the Finder.
  - Animation helpers (easing and tweens).
  - Undo and redo of the app's own changes (text fields undo their own already).
- **Tools.**
  - One app for the front end of a native app, in development only and never in
    the app that is delivered: the theme, widget and GUI designers and the
    tester, as panes of a single window, like Photoshop or Figma. The app being
    developed is drawn in a pane of it, live, and can be zoomed and panned. Its
    first tool, the inspector, is in 0.3, in a window of its own beside the
    app's.
  - The catalogue of widgets and their options (in 0.3, with what kind each
    option is, for five widgets) covers every widget, and the inspector shows
    and edits the options of the widget it picked.
  - An API for writing your own widgets, which says how a widget appears in the
    semantic tree and in the catalogue.
  - A widget designer, to build new widgets.
  - The GUI designer, begun in 0.3 (a view of labels, buttons, text fields,
    columns and rows, saved as a `define-view` file and wired to the app's code
    with `view`, checked when the code is compiled):
    - Every widget and container of the library in its catalogue.
    - Widgets placed by dragging them from the list into the view, and moved
      within it, besides being added by a click.
    - Views inside views: one view placed in another, as a message row is in a
      list.
    - A pane of the one app for the front end, beside the theme designer, with
      the app being developed drawn in it.
  - Undo and redo in the theme designer (the GUI designer has them in 0.3), and
    the app's own menus (File, Edit) in the menu bar.
- **Widgets made with the widget designer.** These come after the designer and
  are built with it, to test it: a number stepper, a divider, a collapsible
  section, a link, chips, badges and cards, and a toolbar, a sidebar and a
  status bar.
- **Web content.** A web view (WebKit's `WKWebView`) inside a window, laid out
  by the library like any widget and drawn by WebKit.
  - **Delivery.** An application bundle (`.app`) with an icon, and signing; and
    a helper for work that goes on in the background while the window stays
    alive.
- **Platforms.** Windows and Linux. A Coalton library.
