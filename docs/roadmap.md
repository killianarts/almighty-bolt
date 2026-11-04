# Roadmap

What is planned after [0.1](releases/0.1.md). These are plans, not promises, and the order inside a section is not a schedule. What has shipped is in the [release notes](releases/) and the [change log](../CHANGELOG.md).

## Next

- **Windows.** Control over a window's title, size limits and full screen, and an app's own menus in the menu bar. (More than one window is in 0.3.)

## Later

- **Images and icons.**
- **Text and layout.**
  - Italic, weights between regular and bold, a font for each widget besides labels and paragraphs (which have `:weight :bold` and `:font`); rich text.
  - Wrapping layouts and splitters, and panes that can be docked, resized, put in tabs and moved, as in Photoshop or Figma.
  - Tabs, a tree view, a table, and date and colour pickers (a segmented `choice` switches between views until there are tabs).
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
  - One app for the front end of a native app, in development only and never in the app that is delivered: the theme, widget and GUI designers and the tester, as panes of a single window, like Photoshop or Figma. The app being developed is drawn in a pane of it, live, and can be zoomed and panned. Its first tool, the inspector, is in 0.3, in a window of its own beside the app's.
  - A catalogue of the widgets and their options, with what kind each option is (a colour, a choice of keywords, a number), so that a tool can show an option and change it. The inspector is its first user: it shows and edits the options of the widget it picked.
  - An API for writing your own widgets, which says how a widget appears in the semantic tree and in the catalogue.
  - A widget designer, to build new widgets.
  - A GUI designer, to build a view by placing widgets instead of writing code.
    - A view is a piece of a window, from all of it down to a row of a list, and views nest. The designer saves each as a Lisp file in the app's project, `(define-view message-row (sender subject time) ...)`, which the app's system loads like any other.
    - A view takes parameters, so that one design is drawn many times with different data, as a message row is in a list.
    - The app wires its logic to a view by the ids given in the designer, naming each widget's kind and options as a call to it would: `(bolt:view login (button :sign-in :enabled (plusp (length name)) :on-press (sign-in name password)) (text-field :name :value name))`. `:value` shows a variable and sets it when the person changes it, `:on-press` is what a press does, and `:contents` fills a container that the design left empty. What is not wired keeps what the designer gave it.
    - The wiring is checked against the design when the app is compiled, and again when a design is saved: an id the design does not have, a widget of another kind, or an option the widget does not take is an error then, not a surprise in the window.
    - Saving in the designer defines the view again, and the window shows it at once, without the app's code being compiled again.
    - The designer can write the start of the wiring, one line for each id with its kind, to be copied into the app once.
  - Undo and redo in the designers, and the app's own menus (File, Edit) in the menu bar.
- **Widgets made with the widget designer.** These come after the designer and are built with it, to test it: a number stepper, a divider, a collapsible section, a link, chips, badges and cards, and a toolbar, a sidebar and a status bar.
- **Web content.** A web view (WebKit's `WKWebView`) inside a window, laid out by the library like any widget and drawn by WebKit: for HTML mail in an email client, or for the pages of a browser for developers that shows one page at several sizes side by side.
- **Delivery.** An application bundle (`.app`) with an icon, and signing; and a helper for work that goes on in the background while the window stays alive.
- **Platforms.** Windows and Linux. A Coalton library.
