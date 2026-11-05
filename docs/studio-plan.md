# The studio: the plan

How the studio becomes what the [Figma prototype][studio-prototype] shows: its
four screens (01 Design, 02 Wire, 03 Debug, 04 Theme) and a fifth mode, Agent,
named there and not yet drawn. The [roadmap](roadmap.md#almighty-bolt-studio) is
the source of truth for what the studio is for; this is the order the work is
done in, and what each step needs. Each step is done as the rest of the library
is: tested from outside, with a deliberate defect for every test, documented in
the reference and the release notes, and committed when `make test` passes.

[studio-prototype]: https://www.figma.com/design/AnkjMYkC0RbbevQUkKMcom/almighty-bolt?node-id=0-1

## Decisions

- **Figma is a starting point, not a source.** The prototype's design system is
  imported once, in phase B; nothing is synchronised with Figma after, and the
  prototype is not kept up to date.
- **Fonts.** Every element that draws text can be given its font: the family,
  the weight (any of them, 100 to 900, not only regular and bold), the size,
  the letter spacing and the line height, and a theme gives each role (label,
  body, heading, code, button) its own. Which families the studio uses is
  worked out later; until then the studio is in the system's (`"System"` and
  `"System Mono"`) at the prototype's sizes, since its families (Construct
  Mono, Mohave, Science Gothic, Geist Mono, JF Dot M+Q) are not on every Mac and
  their licences are not known.
- **Modes are docks.** Each mode opens as a dock layout of its own, arranged as
  the prototype draws it, which the person can rearrange, and which is kept.
  Fixed layouts may come later, if switching between workflows wants them.
- **Code in the studio is a viewer with light editing.** The view's file and
  the code's file are shown in the studio, with line numbers, and edited there
  in small ways: a clause written inline, a stub written for every id, an id
  renamed. Emacs, through the bridge, stays the editor for the rest.

## Where the studio is

What 0.3 has, against the prototype:

- **Have:** docked panes; the GUI designer's parts (catalogue, outline, canvas
  on a stage of its own size, options, wiring stub); the running app in a pane,
  at a zoom and at a size of its own; the inspector beside it; a widget picked
  in the running app opened in its view, live; the bridge to Emacs; the theme
  designer (seeds, worked-out colours, sizes, readability, the `define-theme`
  form, undo); the semantic tree and VoiceOver; the frame ring, the recorder,
  replay scripts and a virtual clock; fonts for every element (any weight,
  letter spacing, line height, by role in a theme, from files); text turned
  90 degrees; soft shadows and dashed edges; images, and icons tinted by the
  theme; a code view, its lines numbered and marked.
- **Part:** the canvas and the running app are two panes, not one; options do
  not say where their values come from; the frame ring, the recorder and the
  kept state have no face; the theme designer previews widgets, not the app's
  own view.
- **Missing:** vector icons; colours for code's syntax; the studio's shell
  (modes, rail, status bar, bottom panel, REPL); the timeline, time travel and
  the call tree.

## Phase A: the library can draw the prototype

Library work, each item a feature of its own, which the studio is the first
user of.

1. **Fonts per element.** Font, weight, size, letter spacing and line height
   on every widget that draws text, and fonts by role in a theme. Fonts
   registered from files (Core Text), so an app can ship its own. (Done:
   `:font`, `typeface`, the theme's role fonts, `load-font-file`.)
2. **Text turned 90 degrees,** for the rail's vertical labels. (Done: `label`'s
   `:turn :up` and `:down`, and `context-push-text`'s turn.)
3. **Effects:** a soft drop shadow, drawn in the shader and reaching past its
   widget's edges (the roadmap's Effects); then a dashed stroke, for outlines
   and hit boxes. (Done: `:shadow` and `:border-dash` on panels and
   pressables, `push-shadow`, and dashes on `push-rounded-stroke`.)
4. **Images and icons,** drawn from a texture, for the catalogue's tiles and
   the tool bars (the roadmap's Images and icons). (Done: `image`,
   `load-image`, `make-image`, `:tint` for icons, and `context-push-image`.)
5. **A code view:** monospace text with line numbers, lines marked (an error,
   the line a press called), and light editing, built on the text area.
   (Done: `code-view`, with `:marks`.)

## Phase B: the shell

6. **The studio's theme,** imported once from the prototype's design system:
   its colours (white, panels `#f2f2f2`, lines `#e4e4e4`, black for what is
   chosen, green for what is well), sizes, radii and spacing, and its text
   styles as fonts by role, written as a `define-theme` form in the studio's
   source. After that the theme is the studio's own: nothing is kept in step
   with Figma. (Done: `:studio-light` and `:studio-dark` in
   `apps/studio/theme.lisp`, picked by `appearance`, in the system's fonts.)
7. **The title bar:** the mark and name, the five modes (each a dock layout of
   its own, kept), the breadcrumb of what is open (view, app, file), whether
   the image is live and which Lisp, Save, and Run Window (the app in a real
   window of its own, with `open-window`). (Done, but for the mark, which needs
   vector icons; until their panes are built, Wire, Debug and Theme show the
   panes they have, and Agent says it is not built.)
8. **The rail, the section headers, the status bar and the bottom panel:** a
   label, its kana and a count over a rule; what was last done, the wires, the
   problems and the undo; tabs for the REPL, the problems, the events and the
   agent. (Done: the rail, the designer's sections with the studio's headers,
   the bottom panel as a dock of its own in every mode, and the status bar; the
   problems are the live view's wiring problems, and the events and the agent
   say they are not built.)
9. **A REPL pane** that evaluates in the app's package, with its history and
   what it printed.

## Phase C: Design

10. **The canvas is the app's window.** The running app, with its real data,
    on a stage with a window's bar. In Design, a press picks the widget of the
    view it is on (`view-widget`) and does not reach the app, and a widget let
    go of goes into the view's container under it; in Run, the app has the
    input. With no app running, the view is drawn with sample data.
11. **The canvas's bar:** the window's size, the zoom, the theme (the app drawn
    in a theme the studio picks), live or sample data, Design or Run, and
    Command-click to try a widget in Design.
12. **The selection:** corner brackets, and a tag that says the widget, its id
    and whether it is wired; what is dragged says the id it will have.
13. **The catalogue and the outline:** a search, groups with counts, icons;
    each widget's kind abbreviated and each container's count.
14. **The inspector:** each option's value and where it comes from (given,
    wired, a default, the theme); the container the widget is in, and its gap
    and alignments, to change; the wiring, checked against the view, with the
    file and line of the `view` form that wires it.
15. **The frames chart** (build, layout and paint of each frame, from the
    ring), and wrapping widgets in a container with Command-G.

## Phase D: Wire

16. **The views, and the state:** every view with its wire count; every
    `state-let` variable of the window, live and editable, kept across a
    reload, and pinned to be kept even when its first value is edited.
17. **The view's file and the code's file** in code views, in step with the
    canvas and, through the bridge, with Emacs.
18. **The wires:** each id of the view, what wires it or that nothing does; the
    chosen wire's clauses as written, and when it was last pressed.
19. **Light editing:** a clause written inline, with completion, into the code
    and compiled; the stub written for every id; an id renamed in the view and
    in every wiring of it.
20. **Problems,** checked across files, and before a save: a wire a save would
    leave with nothing to wire.

## Phase E: Debug

21. **Every event recorded** from the window's first frame, with the frame it
    came in.
22. **The timeline:** each frame's build, layout and paint, a scrubber, play
    and step; going back to a frame by replaying the recording into a copy of
    the app, on the virtual clock.
23. **The canvas at a frame:** what took the pointer, where the press was, and
    what the event did.
24. **The state at a frame, and what changed between two;** the semantic tree
    as a pane; accessibility checks (every control named, Tab in reading
    order, the contrast of every text).
25. **Why a frame drew:** the parts that were laid out or painted again, and
    what in their description changed.
26. **What a press called:** the calls, their arguments, results, times and
    bytes, and Step Into, with SBCL's stepper. The least certain of all; last.

## Phase F: Theme

27. **The theme designer as the prototype lays it out:** each setting tagged
    given, a default or worked out; the app's own view previewed in both modes
    side by side; the themes as tabs; the theme applied to the running app,
    with undo.

## After: stage 2

- **Agent:** a change asked for in words, made through the machine interface
  and proposed as a change to the view and the code. Not designed beyond a
  sentence yet.
- Grain and scanlines for the chrome.

## Checking it against the prototype

`make studio-shots` opens the studio on the sign-in example in each mode and
takes a screenshot through the machine interface, to compare with the
prototype's frame side by side at each step. Behaviour is tested headless and
from outside, as the rest of the library is.
