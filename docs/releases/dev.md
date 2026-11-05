# almighty-bolt 0.3 release notes

*Under development*

Welcome to almighty-bolt X.Y! One paragraph on the theme of the release, then
links to the major sections: [what's new](#whats-new-in-xy),
[backwards incompatible changes](#backwards-incompatible-changes-in-xy),
[deprecations](#features-deprecated-in-xy).

See [Upgrading to X.Y](../upgrading.md) if you are moving from X.(Y-1).

<!-- highlights:start -->
## Highlights

- Area: one line a user would care about.
- Area: one line a user would care about.
<!-- highlights:end -->

## Platform and compatibility

- Supported OSes and GPU APIs.
- Supported SBCL (and Coalton) versions.

## What's new in X.Y

### The inspector

While you develop an app (`open-app`, or `run-app` with `:hot`), Command-Option-I in its window opens the inspector in a window of its own beside it. Press Pick, then a widget in the app's window, and the press picks it instead of pressing it; the inspector says what it is, and an outline in the app's window shows where.

- Its kind and role, its id, its name and value, where it is and how big, and the theme it is drawn in.
- Whether it is enabled, and what the semantic tree says of its state: whether a check box is checked, the items of a list, the options of a choice.
- While it picks, the outline follows the pointer; Escape, in either window, stops.
- It changes the picked widget's value as a hand would: Press for a button or a check box, a field and Set for a text field or a slider, a button for each option of a choice, a dropdown or a list. The app stays in charge of what it holds, and the inspector says why when something cannot be done (the widget is disabled).
- It changes a setting of the theme the picked widget is drawn in (`accent`, `row-height`, `font-size`), which defines that theme again, and shows the theme's `define-theme` form to copy into the app's code.
- It is there only while the app is developed: in an app that is delivered, Command-Option-I is the app's. It closes with the app's window, and the app's semantic tree has nothing of it.

### Views: a design wired to code

A view made in the GUI designer is a file of Lisp that defines it, `(bolt:define-view login (:column :form (:gap 12) ...))`, and the app draws it with `bolt:view`, wiring its own code to it by the ids the designer gave:

```lisp
(bolt:view login
  (label :greeting :text (format nil "Hello ~a" name))
  (text-field :name :value name)
  (button :sign-in :enabled (plusp (length name)) :on-press (sign-in name password))
  (column :recent :contents (dolist (user (recent-users)) (bolt:button user :id user))))
```

- A clause names a widget of the view by its kind and id, and gives it options by the widget's own names, `:on-press` (a button), `:value` (a place a text field shows and typing sets) or `:contents` (what goes in a column or row). What is not wired is as the designer left it.
- Wiring that does not fit is an error when the code is compiled: an id the view does not have, a widget of another kind, an option or a wire the widget does not take. A view not yet defined then is a style warning, and the check is made when it is drawn.
- A view defined again (the designer saves it) is shown in every open window at once, without the app's code being compiled again, and the code that draws it is checked against it.
- `(bolt:view (message-row :id (message-id m)) ...)` draws one view many times in one container, each wired to its own.
- Drawing a view allocates nothing beyond the same widgets written by hand.

A view is named by a symbol of the app's package, as a function is, so that views of two apps or libraries do not take each other's names. It takes no parameters: what differs from one place it is drawn to the next is given by the wiring there.

### An app in a pane

`(bolt:app-pane 'my-app:make-app :id :running :zoom 2 :flex 1)` runs another app, working, in an area of this one, at a zoom, its text sharp at that size: the pointer, the wheel and the keys are its own there, and so is what is typed through the input method, whose candidates open by the caret in the pane. What is in it is in the window's semantic tree, inside the pane and where the pane shows it, so VoiceOver reads and presses its widgets and an agent finds them (`:within` the pane's id) and clicks, types and sets values in them; a field in it has the keyboard only while the pane does. When code is compiled, the app in the pane is made again with the window's, keeping its `state-let` state, the keyboard and its carets, as a window's app does; if the new code cannot make it, the one there stays. In the studio the app runs at a size of its own, as its window would be, set as the canvas's is; at a zoom, that size in its points is shown larger. `(bolt:stage (:id :frame :width 480 :height 360) ...)` is what both stand on: what is in it is laid out at that size, whatever room it is given, and cut off where the room ends. `(bolt:inspector-pane app :id :inspector)` is the inspector of it, beside it in the same window. For a widget that a view drew, the inspector says which view and widget it is and what of it the code gives, and in the studio Edit in the View opens the view in the designer, live, with the widget selected and the running app beside it: an edit there shows in the running app as it is made. The studio has both: an App pane, which runs the app whose constructor is named in its field, at 50%, 100% or 200%, and an Inspector pane that looks at it.

### The studio and Emacs

The studio runs in the Lisp Sly is connected to, and `emacs/almighty-bolt.el` keeps a buffer and the studio in step through it. Edit Source, in the inspector, on a widget picked in the running app, takes Emacs to its clause in the view's file, or to the code that drew it; Edit Wiring, to where the code wires it. Point on a widget's clause in a `define-view` selects that widget in the studio's designer, with the running app beside it. An edit in the designer rewrites the view's form in its buffer, as an edit Emacs's undo takes back, and a view's form typed in Emacs is defined in the image after a pause, so the running app follows without C-c C-c. A view typed in Emacs is read with `*read-eval*` off and checked, never evaluated; other code is compiled with C-c C-c as before. Emacs asks the Lisp what to do, so nothing is evaluated in Emacs from Lisp. Load the file, add `almighty-bolt-mode` to `lisp-mode-hook`, and turn on `almighty-bolt-bridge-mode` once Sly is connected; the Lisp side is the system `almighty-bolt/emacs`, which the studio loads.

### A font for every element

Every widget that draws text takes `:font`: a family, a role of the theme (`:body`, `:label`, `:heading`, `:code`, `:button`), or one of those with what differs, `(bolt:typeface "Mohave" :weight 500 :size 16 :tracking 1.3 :line-height 20)`. A weight is any from 1 to 1000 or a name (`:light`, `:medium`, `:semibold` and the rest); the tracking is the space after each character; the line height spaces a paragraph's and a text area's lines and sets a label's height. A theme gives each role its font, `:heading-font ("Georgia" :size 30)`, following its `:font-name` and `:font-size` for what it does not say, so a studio, or any app, can set its labels, headings, code and buttons apart. A font is data, written in a theme's `define-theme` form and read back. `(bolt:load-font-file "fonts/Mohave-Medium.ttf")` makes the fonts in a file ones the app can ask for by name, for as long as it runs, so an app can bring its own.

A label turns a quarter, for the tabs on the side of a window: `(bolt:label "OUTLINE" :id :rail-outline :turn :up)` reads upwards from its bottom and `:turn :down` downwards from its top, and the label is as wide as a line is tall. Each glyph is drawn from the same bitmap as upright, turned as it is drawn, so turned text is as sharp as upright text, and it turns in a view and in an app in a pane at a zoom too. In a canvas, `context-push-text` takes the turn as an optional last argument.

### Shadows

A panel or a pressable casts a soft shadow with `:shadow '(:y 8 :blur 24 :color ("#000000" :alpha 0.16))`, or several, stacked as a design tool stacks them: moved across and down, blurred, spread larger or smaller, in any colour. The blur is a design tool's (the Gaussian's standard deviation is half of it), so a shadow from a design is given the numbers the design has, and the shader draws it in one pass, its corners round with the panel's. It reaches past the panel's edges; a scroll container or a pane still cuts it at theirs. In a canvas, `push-shadow` draws one, and `push-rounded-stroke`, which the reference has described for canvases since 0.2, is now exported from `almighty-bolt` too.

### Docked panes

`(bolt:dock (:id :studio :layout layout) (:outline "Outline" ...) (:canvas "Canvas" ...))` is panes in groups of tabs, side by side or one over the other, that the person arranges: a tab dragged to another group's tabs puts its pane there, and to a group's edge splits the group. A split of the layout can keep the size of its second group, `(:split :horizontal 300 left right :second)`, as the studio's right-hand group does, and `:min` says how small a group may be made. A tab's context menu moves its pane to the next or the previous group, or splits its group with it, and Control-Shift-Right and -Left move the active pane, so a pane is moved with the keyboard, by an agent and by VoiceOver too. An app moves them too: `show-pane` brings a pane to the front, `place-pane` puts it in or beside another's group, and `pane-group` says which panes share a group. What is in a pane keeps its state when it moves, and a `shortcut` in a pane is heard only while it is the pane last pressed in or that has the keyboard. The layout is a list, returned each frame, for the app to keep and give back. A `split` given a new `:size` now takes it up, so an app can keep a split's width too.

### The studio, begun

`apps/studio` has the GUI designer and the theme designer in one window: `(almighty-bolt-studio:open-studio)` after loading the system `almighty-bolt-studio`. Their parts are panes of a dock, which the person arranges: the widgets to add and the outline, the canvas, the selected widget's options, the theme's settings and its preview, the app being made, running, and the inspector of it, with the GUI designer's toolbar over them. Each designer keeps what it holds (the view, the theme being made, the undo of each) while a part is hidden or moved, and Command-Z undoes in the one last used. Each designer gives its parts as functions, `designer-parts`, which its own window puts side by side as before.

### A tree view

`(bolt:tree roots :id :outline :children #'node-children :label #'node-title :key #'node-id :selected picked)` shows a tree of items, each indented a level further, with a mark that opens and closes those that have children, and returns the key selected. It takes the keyboard as one thing: the arrows move the selection, open and close. VoiceOver reads it as an outline of rows. The GUI designer's outline is one, so that a container's contents can be hidden while the rest is worked on.

### Two panes and a divider

`(bolt:split (:id :sides :size 240) left right)` puts LEFT and RIGHT side by side (or, with `:direction :vertical`, one over the other) with a divider between them that the pointer drags and the arrow keys move. The first pane's width is kept by the split's id and returned, within `:min` and `:max`; with `:keep :second` the second's is, which keeps it as the window is made larger or smaller (a sidebar on the right). A pane the window leaves no room for is shown smaller and is its size again when there is room. What does not fit in a pane is cut at its edge. VoiceOver reads the divider as a splitter. A view, and the GUI designer, can have one: the first widget in it goes in the first pane, the others in the second.

### Dragging

`(bolt:drag-source (:id :new-card :payload card :label "Card") ...)` drags what is in it, carrying PAYLOAD, with a ghost under the pointer; a press in place is still a click (`:clicked`). `(bolt:drop-target (:id :list) ...)` shows where among its children a drop would go and returns what was dropped and its index. In the GUI designer, a widget is dragged from the list on the left to where it goes in the view, and a widget of the view is dragged to another place in it; a click on the list still adds one.

### The app's own menus

`(bolt:app-menu "File" '(("New" :key #\n) :separator ("Save" :key #\s)))`, in the app's function, puts a menu of its own in the menu bar, after the application's menu and before Edit and Window, and returns the item chosen, from the menu or by its shortcut, on the frame it is chosen. The menu bar has the menus of the window that has the keyboard. The GUI designer has a File menu: New, Open… and Save….

### Every widget in the catalogue, and yours

A view, and the GUI designer, can have every widget and container of the library (but the spacer), not five: a paragraph, a text area, a combo box, a slider, a choice, a tab bar, a tree view, a check box, a switch, a dropdown, a list box, a progress bar, a spinner, a grid, a stack, a padding, a panel, a center, a scroll, a pressable and a split besides. `:value` wires a place to whatever widget holds a value (a slider's value, a check box's state, a list's selected index). And an app adds its own with `(bolt:define-widget-entry :stars (:title "Stars" :options ((:count :number 3)) :value :count) ...)`, its options, what can be wired to it and how it is drawn; the designer lists it and a view can have it. Options can be lists of strings (a dropdown's options), edited in the designer one to a line.

A view can be inside another: `(:view :card (:name card))` in a view's form draws the view `card` there. The GUI designer lists the views defined in the running Lisp under Views, to drag or click into the one being designed as any widget is, and a view's option on the right is a menu of them. A view inside itself says so in its place.

### The GUI designer, begun

`apps/gui-designer` is the start of the GUI designer on the [roadmap](../roadmap.md): `(almighty-bolt-gui-designer:open-designer)` after loading the system `almighty-bolt-gui-designer`. It shows on the left the widgets it knows (every one in the catalogue), the views defined and an outline of the view, the view being designed in the middle, and the selected widget on the right.

- The view is drawn with the real widgets, in the theme, as the app will draw it, but none of them works: a press selects the widget it was on (the innermost: a press between two widgets selects the column they are in), and typing goes nowhere. The selected widget is outlined, and so is the one the pointer is over.
- The outline is a tree of every widget of the view in order, by its kind and id, indented by how deep it is; the mark before a container hides what is in it, or shows it again. A row selects its widget, as a press in the view does, which is how a container whose children fill it (a row of buttons) is selected; the selected widget's row is chosen in it.
- A widget from the left goes at the end of the selected column or row, or just after the selected widget, or at the end of the view; it is called by its kind and a number (`:button-1`), and selected.
- On the right, the selected widget's options, each with an editor of its kind: a field for text and for numbers (a number that is not one is shown as wrong and changes nothing), a check box, a menu of choices. What is changed shows in the view at once. Its id is changed with Return, to one no other widget in the view has. Remove takes it out, with what is in it.
- Undo and Redo, in the toolbar and as Command-Z and Command-Shift-Z, take back every change to the view and put it back: a widget added or removed, an option changed (a word typed into one is one step), an id, a view opened or begun. A text field with the keyboard undoes its own typing first.
- New starts an empty view, Save… writes it to a `.lisp` file that defines it, `(almighty-bolt:define-view login (:column ...))`, named after the file and in the package given in the toolbar's Package field (the file begins with its `in-package`), and Open… reads one back, its name and package with it. The view is each widget as its kind, its id and the options that are not the default, by the widget's own names, then what is in it, such as `(:row :actions (:gap 8) (:button :sign-in (:text "Sign in")))`. A file is read with `*read-eval*` off and nothing in it is evaluated, and one that is not a view is not opened, with what is wrong said: a kind of widget the designer does not have, an option a widget does not have or a value it cannot have, an id two widgets have, or something inside a widget that holds nothing.
- Save also defines the view in the running Lisp, so an app's window that draws it shows it at once, and says if the app's code that wires it no longer fits (an id it wires that the view has lost).
- The view is designed at a size of its own, 480 by 360 to begin with, as a window: its width and height are typed over it, or dragged by the grip under its corner, and it is laid out at that size whatever the canvas's pane is, cut off where the pane ends rather than made narrower.
- Open View lists the views the running Lisp has, by `package::name`, and opens one live: every edit (and every Undo) is made in the image as it is made, so an app that draws the view, running, changes as it is edited, without a save or a compile. A view remembers the file it was defined in, and Save writes it there without asking, if that file holds nothing but the view; a file with code in it too is left as it is, and Save asks where.
- Below the selected widget, the start of the code that draws the view: `(bolt:view login (column :sign-in-form) (label :heading) ...)`, a line for each widget, to copy.

It is built on a new container, `design-surface`, which any designer can use: what is in it is drawn but does not work, no input reaches it (the pointer, the keyboard, a shortcut, a dialog's Escape; what floats from a widget in it, a menu or a dialog, takes none either, and a canvas in it is not given the keyboard), and a press on it returns the id of the thing it was on. `(bolt:design-surface (:id :canvas :selected picked) ...)` outlines the thing whose id is `picked`.

### More than one window

An app can open more windows: `(bolt:open-window 'make-preferences :title "Preferences" :width 400 :height 300)`, called where a button says it was pressed (the window opens after the frame) or from the REPL, opens a window that shows the app `make-preferences` makes, and returns it for `(bolt:close-window window)`.

- Each window's app is an app of its own, with its own `state-let`. Two windows share what Lisp shares: a closure over the first app's variables, or a special variable.
- A frame in which one window took input (a press, a key, the wheel) draws the others, so a preferences window and the window it sets show the same thing. A frame that took none wakes nobody, so windows at rest stay at rest.
- While you develop (`open-app`, or `run-app` with `:hot`), compiling code makes every window's app again, not only the first.
- Closing the app's window leaves the others open; closing the last stops the windows' loop and leaves the Lisp running. `close-app` closes the app's window only, and Quit (Command-Q) closes them all: before, it closed only the window in front.
- `open-session` takes a window, for the machine interface on a window other than the app's.
- Where a window opens can be given, to `open-app`, `open-window` and `run-app`: `:top`, `:left`, `:bottom` and `:right`, points from those edges of the screen's usable area as in CSS's absolute positioning (`:top 0 :right 0` is the top right corner), or `:beside window`, with `:side` (`:right`, `:left`, `:above`, `:below`) and `:gap`. A window given neither opens below and to the right of the last one, and the first in the middle of the screen; before, every window opened in the middle, on top of the others. The inspector opens beside the app's window.
- `(bolt:this-window)`, in an app's function, is the window the app is in, as `open-window` returns it: a button can close its own window with `(bolt:close-window (bolt:this-window))`, or open another next to it with `:beside (bolt:this-window)`. Outside a frame it is `nil`.

### Spacing in a theme

A theme now says how much room there is in and round the controls, as it says their colours, font and corners. It is worked out from one number, `:spacing`, as the colours are worked out from three seeds, and any of the sizes can be given on its own as well.

```lisp
(define-theme :roomy (:base :light)
  :spacing 1.25)        ; everything a quarter roomier

(define-theme :dense (:base :dark)
  :spacing 0.8
  :row-height 20)       ; and the rows of lists and menus exactly 20 points
```

At a spacing of 1 every widget is the size it was before. Each size is its size at 1 times the spacing, to the nearest point. The sizes, in points:

- `:button-padding-x` and `:button-padding-y` (14 and 7): round the words of a button, a menu button and a dropdown.
- `:field-padding-x` and `:field-padding-y` (8 and 6): round the text of a text field, a combo box and a text area.
- `:row-height` (24): a row of a list box or a menu. `:menu-padding` (4): round the rows of a menu.
- `:popover-padding` and `:popover-gap` (12 and 8), and `:dialog-padding` and `:dialog-gap` (20 and 12): inside a popover and a dialog, and between what is in them.
- `:option-gap` and `:option-row-gap` (20 and 6): between the options of a choice, side by side and one under another.
- `:control-height` (22): a checkbox, a switch and an option of a choice. `:slider-height` (24) and `:progress-height` (14).

The room in a toast and a tooltip follows the spacing too. What a mark is (a checkbox's box, a radio dot, a slider's thumb, a switch's track) stays its own size. The gaps between the parts of the app, which it gives with `:gap` and `padding`, are the app's.

A theme that gives the spacing works out every size again, except those it gives as well; a theme that does not give it keeps its base's. `theme-form` writes the spacing and the sizes down as numbers, and `+theme-sizes+` lists the sizes with what each is at a spacing of 1. The theme designer has a Spacing slider, from 50% to 200%.

### Minor features

#### Windowing

- A window whose frame fails no longer stops: it shows the last frame that did not fail, with the error along its bottom, says the error once (not on every move of the pointer), and goes on, so that a press on what it shows can put the app right. Tests and the REPL still get the error. The machine interface and VoiceOver are given the tree of the frame that is shown. See [When a frame fails](../reference.md#when-a-frame-fails).
- [`symbol`](../reference/page.md#symbol) now does X.

#### Rendering

- A `panel` or a `pressable` can have an edge: `:border-color` (any colour, a theme's too) and `:border-width`, drawn inside it, round its corners, antialiased as its fill is. It shows over a translucent fill, as a larger rectangle behind a smaller one would not: a calendar's today can be ringed in the accent colour and keep its own tint. The renderer draws it in the same pass as rectangles and glyphs; `push-rounded-stroke` puts one in a canvas.

- A `:color` can be a colour of the theme that the thing is drawn in, so that an app's tints suit a dark window and a light one without a colour for each: a setting (`:accent`, `:dim-text`, `:panel` ...), `(:accent :alpha 0.2)`, or `(:mix :accent :background 0.8)`. Labels, paragraphs, panels and pressables take them, and `"#rrggbb"` strings as well as packed colours. `(theme-color designator)` gives the packed colour.

#### Text

- `context-font` gives a font of any weight, from 1 to 1000 or by name (`:light`, `:medium`, `:semibold` and the rest): the upright face of the family nearest that weight and as wide as its regular face, so Helvetica Neue's `:black` is its bold and not its condensed black. And it takes a tracking, the points of space after each character, which measuring, carets, clicks in text and breaking into lines all count. In an app in a pane at a zoom, the tracking is drawn at the zoom too.

- `label` and `paragraph` take `:weight` (`:regular` or `:bold`) and `:font`, a font family: a time or a heading can be bold at the size of what is round it, and a number can be in `"Menlo"`. Bold is the family's bold face, made by Core Text; a family that has none is drawn regular. A paragraph is broken into lines again when its weight or font changes.

#### Layout

- `pressable` is a container that is pressed as a button is, so that a card, a calendar day or a row of two lines is one thing to press and not a button among labels. It returns true on the frame it is clicked, by the pointer anywhere on it or by Space or Return. It looks like a button (`:look :button`, with what is in it in the button's text colour) or like a panel that answers the pointer (`:look :plain`), and with `:selected` it is a toggle. A screen reader reads it as one button named by the words in it; a button or a field inside it is still pressed on its own.

- `grid` lays its children out in `:columns` columns, a row at a time, each stretched to its cell. The columns are equal or take shares of the width (`:column-widths '(1 2 1)`); the rows are each as tall as their tallest child (`:auto`), all as tall as the tallest child of all (`:uniform`, for a month of days whose cells should be one size), or a given height. In the semantic tree it is a `:grid`, and each cell says its `:row` and `:column`.

- A container can be given a size. `column`, `row`, `stack`, `padding`, `panel` and `center` take `:width` and `:height`, which fix it, and `:min-width`, `:min-height`, `:max-width` and `:max-height`, which bound one side. A paragraph in a container with a greatest width is broken to that width, and what is too large for a container with a greatest size is cut off at its edges, as in a scroll container. The limits hold within what the parent allows: a parent's limits win. Before, only a `scroll` took a height.

- `(spacer)` is empty room that takes the free space of the row or column it is in, so that what comes after it goes to the far edge: `(row (:fill t) (label "Title" :id :title) (spacer) (button "Done" :id :done))`. `:flex` gives it more than one share. It needs no id, since it holds nothing, and it is not in the semantic tree. It replaces an empty `(row (:id :gap :flex 1))`.

#### Input

- `choice` takes `:style :segmented`, which draws its options as one control of joined segments with the chosen one filled, as a Mac segmented control is: the usual look for switching between two or three views, which radio buttons are not. It is chosen as any choice is, and is a radio group in the semantic tree and to VoiceOver. `(bolt:tabs '("Design" "Wire") page :id :pages)` is a tab bar, a choice with `:style :tabs`, which VoiceOver reads as a tab group.

- A menu longer than the window shows as many rows as fit, with a bar that says where they are among them all, and scrolls: by the wheel, and by the arrow keys, which keep the highlighted row in view. `dropdown` and `menu-button` take `:max-rows`. Letters typed while a menu is open move the highlight to the first row they begin, as in a Mac pop-up button. Before, a dropdown of sixty countries opened a menu that ran past the bottom of the window. A row is highlighted by the pointer only when the pointer moves onto it, so a still pointer no longer takes the highlight back from the keys.

- A `list-box` whose selected row is out of its window shows it: on its first frame, and when the app selects a row other than the one it selected before (say, from a search field beside it). While the selection stays the same, the list stays where the wheel put it. Before, a list opened at its top whatever was selected.

- `(shortcut #\s :command)` is true in the frame its keys are pressed, for the app's own keyboard shortcuts: `(when (bolt:shortcut #\s :command) (save))`. It takes the key, and the modifiers held must be the ones given.
- VoiceOver shows a `context-menu`: on what can be pressed in its area it offers Show Menu (VO-Shift-M), which opens the menu there as a secondary press does, and then reads its items. The semantic tree marks those things `:menu t`. Before, a context menu could be opened only with the pointer.
- `canvas` takes `:cursor`, the shape the pointer has over it: a crosshair to draw with, a pointing hand over something that can be pressed, a hand to drag with, arrows to resize. The app can give another each frame.

#### Machine interface

- `make-session` takes `:app`, a function that returns the app instance the window runs, where it took `:variant`, and `session-app` is what `session-variant` was. The library's internal names follow: what a window runs is an app instance (`almighty-bolt/app-instance`, `immediate-app`, `host-app`), not a variant.
- Every call that names a thing (`find-node`, `click`, `set-value` and the rest) takes `:within`, the id of a container it is inside. Ids need be unique only within a container, so `(m:click session :id :show :within :gregorian-search)` presses the Show button of one panel of two that each have one. A name that fits more than one thing says to use it. The MCP server's tools take `within` as well.

#### Examples

- The 7GUIs tasks and the stress apps no longer carry the name of the immediate "variant", from when the project compared three kinds of ui. The make targets take `TASK=` alone (`make 7guis TASK=counter`, `make 7guis-smoke TASK=counter`, `make snapshots`, `make replay SCRIPT=…`), `make image` writes `build/apps`, each task's app is in `examples/7guis/<task>/app.lisp` (and `bench/stress/<app>/app.lisp`), and the systems are `almighty-bolt-7guis/apps` and `almighty-bolt-stress/apps`. Replay scripts no longer write `:variant :immediate` in their header; old scripts still play. Benchmark records are version 3, without `:variant`.
- The examples that draw in a canvas (the cells' grid, the circles and the stress apps' rows, editor and chart) import only from the public `almighty-bolt` package, as a program that uses the library does; they took the same functions from `almighty-bolt/internals` before. What they draw with is all public: `push-rect`, `context-push-text`, the key constants and the rest.
- The gallery shows how each widget, container and function in it is called, under it: its arguments and options with their defaults, as its lambda list has them, in a fixed-width face that can be selected and copied. It also shows the containers it did not have before, `grid`, `pressable` and `spacer`, and a segmented `choice`.
- The theme designer has Undo and Redo, by its buttons and by Command-Z and Command-Shift-Z: every change to the theme is taken back and put back, a run of changes to one control (a slider dragged, a colour typed) as one step.

## Backwards incompatible changes in X.Y

### `context-push-text` takes a turn

`context-push-text` is now `(context-push-text context display-list font string x baseline color &optional turn)`, with `turn` NIL, `:up` or `:down`. A method of it for a context of your own must take the optional argument too:

```lisp
(defmethod context-push-text ((context my-context) list font string x baseline color
                              &optional turn)
  (my-push-text list font string x baseline color turn))
```

### `context-font` takes a weight and a tracking

`context-font` is now `(context-font context name size &optional weight tracking)`. The weight is a number from 1 to 1000 as CSS has it (400 regular, 700 bold) or a name, `:thin`, `:extra-light`, `:light`, `:regular` (or NIL), `:medium`, `:semibold`, `:bold`, `:extra-bold` or `:black`; the tracking is the points of space after each character (0 or NIL for none). A method of it for a context of your own (a test double, say) must take both optional arguments, or SBCL says its lambda list does not agree with the generic function's:

```lisp
(defmethod context-font ((context my-context) name size &optional weight tracking)
  (my-font name size (or weight :regular) (or tracking 0)))
```

### Area

Before:

```lisp
(old-code)
```

After:

```lisp
(new-code)
```

What to do, and why the change was made.

## Features deprecated in X.Y

- `old-symbol` is deprecated. Use [`new-symbol`](../reference/page.md#new-symbol)
  instead. It will be removed in X.(Y+2).

## Features removed in X.Y

These features have reached the end of their deprecation cycle. See the
[X.(Y-2) release notes](X.(Y-2).md) for how to migrate.

## Bug fixes

- A float put by another box than in the frame before, with nothing else in the frame changed, kept its old place: the GUI designer's outline of the selected widget stayed on the one selected before when the studio's right-hand pane showed the theme or the inspector, and only some rows of the outline seemed to select. A float is laid out again when its anchor is another box.

- Text fields, combo boxes and text areas took keys, but not what the system's input method makes of them. Japanese, Chinese and Korean could not be typed into them, and neither could an accent made with a dead key (Option-E, then E, for é) or a character from the Character Viewer. Now what is typed goes through the input method while one of them has the keyboard. What it is composing is shown at the caret, underlined, with the part it is converting underlined more, and its candidates open by the caret. Once committed, the text goes into the field as one edit, which Command-Z undoes whole. The app is given only committed text, and so are VoiceOver and the machine interface. A field that loses the keyboard while text is being composed drops it.
- An app's function that described two things at the top, with no container round them, showed only the last, and said nothing if they had ids of their own. It is now the error it already was when they had none (`duplicate-id`, which says to put them in a container).
- Text fields, combo boxes and text areas could not undo. Now Command-Z undoes an edit and Command-Shift-Z does it again. A run of typing is undone as one, and so is a run of deleting; a paste, a cut or a Return is one of its own, and a press or a key that moves the caret ends a run. When the app gives the field text of its own, what was done before can no longer be undone into it.
- An app whose whole interface was a dialog (or anything else that floats), with nothing described under it, showed an empty window. It now shows the dialog over an empty screen.
- The pointer was an arrow everywhere. It is now a text cursor over text fields, text areas, combo boxes and selectable text, as in other Mac apps.
- The menu bar had the application's menu with Quit only. It now has the menus every Mac app has: Hide, Hide Others and Show All beside Quit; Edit, with Undo, Redo, Cut, Copy, Paste and Select All, which do what their keys do in whatever has the keyboard; and Window, with Minimize, Zoom and Close.
- A key pressed with Command and Option was what Option makes of the key (Option-I is the dead key of the circumflex, and types nothing), so a `shortcut` with `:option` never came. With Command held, the key is now the key itself.
- A screenshot taken through a session opened as soon as a window was there (in `run-app`'s `:driver`, say) was a PNG one pixel square, since the window had not yet made a frame or been given its size. `open-session` now returns once it has.
- `:labelled-by` named a field after the first widget in the window with that id, though ids need be unique only within a container: of two panels that each had a label `:title` and a field labelled by it, both fields were called by the first panel's label. A field is now named by the label nearest it: among what is in its own container first, then outward.
- A panel given a transparent `:color` (`"#00000000"`, or `(pack-color 0 0 0 0)`) was drawn in the theme's panel colour, since 0 stood for "the theme's". It now draws nothing, and a panel with no `:color` is the theme's. The same holds for a label's or a paragraph's `:color`.
- A slider whose value the app keeps rounded (a whole number, say) was moved again in every build of a frame while the pointer held it, so the frame did not settle and a warning was printed, every frame of a drag. It now slides only when the pointer has moved, and shows the value the app keeps. The theme designer's Size, Corners and Spacing sliders did this.
- The wheel over a list, a lazy list, a scroll container or a text area inside a scroll container went to it even when it could move no further that way, so a page scrolled up to a list at its top stopped there. Now what cannot move the way the wheel goes leaves it to the container it is in: the page goes on up past the list, and over a text area whose lines all show the page scrolls. A menu or a popover still keeps the wheel from what is under it.
