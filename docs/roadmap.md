# Roadmap

What is planned after [0.1](releases/0.1.md). These are plans, not promises, and
the order inside a section is not a schedule. What has shipped is in the
[release notes](releases/) and the [change log](../CHANGELOG.md).

## Next

- **Windows.** Control over a window's title, size limits and full screen.
  (More than one window, and an app's own menus, are in 0.3.)

## Later

- **Images and icons.**
- **Text and layout.**
  - Italic, weights between regular and bold, a font for each widget besides
    labels and paragraphs (which have `:weight :bold` and `:font`); rich text.
  - Wrapping layouts.
  - A table, and date and colour pickers.
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
  - Drag and drop from the Finder (inside an app, it is in 0.3).
  - Animation helpers (easing and tweens).
  - Undo and redo of the app's own changes (text fields undo their own already).
- **Tools.** The designers, the inspector and the tester become one app: see
  [almighty-bolt studio](#almighty-bolt-studio) below.
- **Web content.** A web view (WebKit's `WKWebView`) inside a window, laid out
  by the library like any widget and drawn by WebKit.
  - **Delivery.** An application bundle (`.app`) with an icon, and signing; and
    a helper for work that goes on in the background while the window stays
    alive.
- **Platforms.** Windows and Linux. A Coalton library.

## almighty-bolt studio

One app for the front end of a native app, in development only and never in the
app that is delivered: the theme, widget and GUI designers, the inspector and
the tester, as panes of a single window, like Photoshop or Figma. An Almighty
Lisp developer designs with their hands and thinks in s-expressions; the studio
does not make them choose. Every gesture on the canvas is an edit to a form they
can read, and every form they write is a canvas they can touch, in the running
image. A prototype of it, in the design system taken from the almighty-lisp
site, is in [Figma][studio-prototype].

[studio-prototype]: https://www.figma.com/design/AnkjMYkC0RbbevQUkKMcom/almighty-bolt?node-id=0-1

### Ideals

- **The image is the document.** The studio is a window onto the running Lisp,
  not a file editor. Views, themes, state and windows are live objects, there in
  the REPL in its dock. Saving writes source; nothing is exported.
- **One thing, read three ways.** A view is a form. The canvas, the outline and
  the code are three editors of the same s-expression, in step as you type.
  Wiring is a second form beside it, checked against the first.
- **What you see is what the app draws.** The canvas is the library drawing the
  app, at its real size, in its real theme and with its real data, never an
  imitation.
- **Time is a material.** Every frame and every event is recorded: go back,
  replay, compare the state of two frames, step into what a press called, and
  keep a recording as a test.
- **Accessible as it is made.** The semantic tree, what VoiceOver says, the
  keyboard order and the contrast of every text are shown while you design, not
  found in an audit afterwards.
- **Agents work beside you.** The machine interface the tests use is how an
  agent proposes a change: you see it on the canvas and in the code, and take
  it, change it or undo it.

### Features

In five modes, with the panes of each. What 0.3 has is said; the rest is
planned.

- **Design.**
  - A catalogue of every widget and container of the library, and the app's own
    views and widgets beside them.
  - Widgets dragged from the catalogue into the view, and moved within it, with
    a marker where they will land: into this column, before its third child.
    (In 0.3.)
  - Handles that change a container's gap, alignment and flex and a widget's
    size limits, never x and y, since the layout places things. Widgets put in a
    column, row, grid or panel with a key (Command-G), and taken out of one.
  - Views inside views, as a message row is in a list.
  - The view in each window size, in both modes, and in its data's states
    (empty, long, invalid, loading), side by side.
  - Trying a widget in place: Command-click uses it instead of selecting it.
  - The outline, the inspector (what each option is and where its value comes
    from: given, wired, a default or the theme), undo and redo, and saving as a
    `define-view` file are in 0.3.
- **Wire and code.**
  - Wiring written in the inspector, which goes into the source as a clause,
    compiled and checked against the view as it is typed.
  - The stub of the wiring for every id (0.3 shows it to copy), an id renamed in
    the view and in every wiring of it, and where an id is wired.
  - The window's `state-let` values, live and editable, kept across a reload.
    (0.3 has them at the REPL, with `state`.)
  - The view's form edited as Lisp, with paredit and completion of ids, kinds
    and options, or the same file open in Emacs, kept in step through the image.
- **Debug.**
  - A timeline of frames: the build, layout and paint of each, the builds it
    took and the bytes it allocated, from the frame ring.
  - Replay and time travel: back to any event, replayed from there on a virtual
    clock, with the state of two frames compared.
  - Why a frame was laid out or painted again: what in its description changed.
  - What a press called, with its arguments, its result and its time, and a way
    into the debugger at it.
  - A recording kept as a replay script with checkpoints: an acceptance test
    written by using the app.
- **Theme.**
  - The theme designer as a pane: the seeds, the colours worked out from them,
    the sizes, both modes side by side, readability, and the `define-theme`
    form, kept in step with the design system's variables in Figma.
- **Test and agent.**
  - The semantic tree, spoken by VoiceOver on request, the keyboard order drawn
    over the canvas, and the contrast of every text checked. (0.3 has the tree,
    and VoiceOver.)
  - An agent pane: a change asked for in words, made by an agent through the
    machine interface and the MCP server, and proposed as a change to the view
    and the code, shown on the canvas.
- **Widgets made with the widget designer,** built with it to test it: a number
  stepper, a divider, a collapsible section, a link, chips, badges and cards,
  and a toolbar, a sidebar and a status bar.

### Gaps

What stands between the library today and the studio, as found while building
the GUI designer, and where on this roadmap each is closed.

- **The app in a pane is a picture to the system.** The app running in the
  studio's pane is not in the window's semantic tree, so VoiceOver and the
  machine interface do not see into it, and it is not made again when its code
  is compiled. A pane is moved only by the pointer.
- **Views take no parameters.** A view inside another is drawn as it was
  designed; wiring where a view is drawn gives what differs, which may be
  enough.
- **Type and effects.** No italic, no weights between regular and bold, no
  shadow, grain or scanlines: the design system's chrome cannot be drawn by the
  library yet ([Text and layout, Effects](#later)).
- **No code editor.** A text area is plain text: no colouring, no structural
  editing, no large files. A code widget, or a bridge to Emacs through the
  image.
- **Recording without a face.** The frame ring, the phase times and replay
  scripts exist, and nothing shows them; the timeline and replay panes are built
  on what is there.

### Stages

1. **Code and time.** A code editor, or the bridge to Emacs. The timeline,
   replay and state panes over the frame ring. The semantic and accessibility
   panes. Tests made from recordings.
2. **Craft and agents.** Effects for the chrome: shadow, grain, scanlines. The
   agent pane over the MCP server. The theme and the Figma variables kept in
   step. Shipping an `.app`.
