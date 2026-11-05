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
  - A font for every element that draws text: its family, any weight, its
    size, letter spacing and line height; fonts by role (label, body, heading,
    code, button) in a theme; fonts loaded from an app's own files. (In 0.3.)
  - Italic, and rich text.
  - Text turned 90 degrees. (In 0.3.)
  - Wrapping layouts.
  - A table, and date and colour pickers.
- **Effects.** Shadows, glows, blur, opacity and other filters, and dashed
  strokes. (Soft shadows under panels and pressables are in 0.3.)
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

In five modes, Design, Wire, Debug, Theme and Agent, chosen in the studio's
title bar. Each mode is a dock layout of its own, opened as the prototype
arranges it, which the person rearranges and which is kept. Over them, the
title bar says what is open (the view, the app, the file) and whether the image
is live, and has Save and Run Window; under them, a panel with the REPL, the
problems, the events and the agent; and a status bar. What 0.3 has is said; the
rest is planned, in the order of the [studio's plan](studio-plan.md).

- **Design.**
  - The canvas is the app's window: the running app, with its real data, in a
    window of the size, zoom and theme chosen over the canvas. In Design a
    press selects the widget of the view it is on, with its id and whether it
    is wired, and a widget let go of goes into the view; in Run the app has the
    input. (0.3 has the view and the running app in two panes, the app at a
    size of its own, and a widget picked in the app opened in its view.)
  - A catalogue of every widget and container of the library, and the app's own
    views and widgets beside them, with a search.
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
  - The running app edited in the studio. A view is opened on the canvas by
    its name, from the image; every edit on the canvas is made in the image at
    once, so the app running in the App pane changes as it is made, and saving
    writes the file. A widget picked in the running app opens the view that
    drew it, with the widget selected, or the code that did. What the inspector
    changes in a view is an edit to the view, not only to the frame. (In 0.3.)
  - The bridge to Emacs, for the code. The studio runs in the image Sly is
    connected to; a minor mode, `almighty-bolt.el`, keeps a buffer and the
    studio in step through it. Edit Source on a widget picked in the running
    app takes Emacs to its clause in the view, or to the code that wired it;
    point on a clause selects its widget on the canvas; an edit on the canvas
    rewrites the clause in the buffer, as an edit Emacs can undo; and a form
    typed in the buffer is made in the image after a pause, so the app follows
    without C-c C-c. The mode asks the studio for what to do; nothing is
    evaluated in Emacs from Lisp. Without Emacs, the studio works without it.
    (In 0.3, for a view's form; code is still compiled with C-c C-c, since a
    function made from what is half typed would run it.)
  - Wiring written in the inspector, which goes into the source as a clause,
    compiled and checked against the view as it is typed.
  - The stub of the wiring for every id (0.3 shows it to copy), an id renamed in
    the view and in every wiring of it, and where an id is wired.
  - The window's `state-let` values, live and editable, kept across a reload.
    (0.3 has them at the REPL, with `state`.)
  - The view's file and the code's file in the studio, with line numbers, and
    edited there lightly: a clause written inline with completion, a stub for
    every id, an id renamed. Each id beside them, with what wires it or that
    nothing does. Emacs, through the bridge, is the editor for the rest.
  - Problems checked across files, and before a save: a wire a save would
    leave with nothing to wire.
- **Debug.**
  - A timeline of frames: the build, layout and paint of each, the builds it
    took and the bytes it allocated, from the frame ring.
  - Every event since the window opened, each with its frame. Replay and time
    travel: back to any event, replayed from there on a virtual clock, with the
    state of two frames compared, and the canvas as it was then: what took the
    pointer, where a press was, and what it did.
  - Why a frame was laid out or painted again: what in its description changed.
  - What a press called, with its arguments, its result and its time, and a way
    into the debugger at it.
  - A recording kept as a replay script with checkpoints: an acceptance test
    written by using the app.
- **Theme.**
  - The theme designer as a pane: the seeds, the colours worked out from them,
    the sizes, both modes side by side, readability, and the `define-theme`
    form. The prototype's design system is imported once, as the studio's own
    theme; nothing is kept in step with Figma after.
- **Test and agent.**
  - The semantic tree, spoken by VoiceOver on request, the keyboard order drawn
    over the canvas, and checks that every control has a name, that Tab goes in
    reading order and that every text can be read. (0.3 has the tree, and
    VoiceOver.)
  - An agent pane: a change asked for in words, made by an agent through the
    machine interface and the MCP server, and proposed as a change to the view
    and the code, shown on the canvas.
- **Widgets made with the widget designer,** built with it to test it: a number
  stepper, a divider, a collapsible section, a link, chips, badges and cards,
  and a toolbar, a sidebar and a status bar.

### Gaps

What stands between the library today and the studio, as found while building
the GUI designer, and where on this roadmap each is closed.


- **Views take no parameters.** A view inside another is drawn as it was
  designed; wiring where a view is drawn gives what differs, which may be
  enough.
- **Effects and icons.** Fonts for each element, turned text and shadows are in
  0.3; no dashed stroke, grain or scanlines, and no icons: the prototype's
  chrome cannot all be drawn by the library yet ([Effects, Images and
  icons](#later)).
- **No code view.** A text area is plain text, with no line numbers. Emacs, over
  the bridge, edits the code (0.3); the studio needs a viewer with light
  editing for its Wire mode.
- **Recording without a face.** The frame ring, the phase times and replay
  scripts exist, and nothing shows them; the timeline and replay panes are built
  on what is there.

### Stages

1. **The prototype, built.** The running app edited in the studio and the
   bridge to Emacs are in 0.3. Then, as the [studio's plan](studio-plan.md)
   orders it: what the library needs to draw the prototype (fonts for every
   element, turned text, shadows, icons, a code view); the shell and its five
   modes as docks; Design with the canvas as the app's window; Wire; Debug,
   with the timeline, time travel and the state of each frame; Theme.
2. **Craft and agents.** Grain and scanlines for the chrome. The agent mode
   over the MCP server. Shipping an `.app`.
