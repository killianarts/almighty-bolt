# almighty-bolt: the API as decided (draft)

*Stage 4, step 0 of the roadmap, with Micah's answers of 2026-10-30. The library is folded into one
system and its packages are renamed (`almighty-bolt/...`); the public package `almighty-bolt` and
the entry points below are built in Stage 5. This file becomes the README then.*

almighty-bolt is a GUI library for Common Lisp, on macOS first. It is immediate mode: the interface
is a function that is called again every frame, and each widget says in its return value what the
person did to it.

## A first app

```lisp
(defpackage #:counter
  (:use #:cl #:almighty-bolt))
(in-package #:counter)

(defun make-counter ()
  (kept-let ((count 0))                  ; state that survives a reload
    (lambda ()                           ; called every frame: describes the whole screen
      (padding (:all 24)
        (row (:gap 16 :cross-align :center)
          (label (princ-to-string count) :id :count :size 32)
          (when (button "Count" :id :count-button)   ; true on the frame it is clicked
            (incf count)))))))

(open-app 'make-counter :title "Counter" :width 360 :height 160)
```

`make-counter` is given by name, so that compiling it again changes the window. It returns the
function that describes the screen; `open-app` does the wrapping that `(make-immediate-variant
(lambda () ...))` does in the apps today.

## Working from the REPL (Sly)

1. `C-u M-x sly`, and pick `sbcl` (the default here is CCL, which this library does not support).
2. `(asdf:load-system "almighty-bolt")`, load your file, and `(open-app 'make-counter ...)`. The
   window opens and the REPL is yours again: there is no server.
3. Edit, and compile with C-c C-c or C-c C-k. The window changes in about half a second, with the
   state `kept-let` holds, the caret and selection of fields, the list scroll and the keyboard focus.
4. Close the window (the button, Command-W or Command-Q) or `(close-app)`: Lisp keeps running, and
   `open-app` opens it again from the start.
5. For a program that is only a window, `(run-app 'make-counter ...)` runs on the main thread and
   returns when the window closes, and `:watch-sources t` also reloads when a source file is saved.

## The public package `almighty-bolt`

| What | Symbols |
|---|---|
| Opening | `run-app`, `open-app`, `close-app` |
| State | `kept-let`, `kept-value` (to look at a variable from the REPL) |
| Widgets | `label`, `paragraph`, `button`, `text-field`, `slider`, `choice`, `checkbox`, `list-box`, `progress`, `canvas`, `scrim` |
| Containers | `column`, `row`, `stack`, `padding`, `panel`, `center` |
| Time and frames | `now`, `ask-for-frame` (an animation asks for the next frame; a still app asks for none) |
| Drawing in a canvas | `pack-color`, `push-rect`, `push-rounded-rect`, `push-clip`, `push-unclip`, `context-font`, `context-measure`, `context-push-text`, `context-caret-x`, `context-index-at-x`, `current-context` |
| Events in a canvas | `key-event-kind`, `key-event-logical-key`, `key-event-modifiers`, `modifier-down-p`, the `+key-...+` and `+modifier-...+` constants, `+points-per-line+`, `set-text-input-caret` |
| Text editing | `make-text-state`, `edit-text`, `text-state-string`, `text-state-caret`, `text-state-selection-p`, `text-state-selection-start`, `text-state-selection-end` |
| Later (Stage 6) | `accessibility-tree` and the machine interface |

What stays out of it: the variant contract, the host, the display list's internals, the replay and
test support. They stay in the system under `almighty-bolt/...` packages for the tests and the
benchmarks, and are not promised to anyone.

## How widgets are called

Every widget takes a required `:id` keyword. It names the widget among its siblings, and it is what
keeps a field's caret and a list's scroll across frames and across a reload, and what a test or a
replay script finds the widget by. Leaving it out is an error on the first frame, with a message
that says which widget. Containers (`column`, `row`, `stack`, `padding`, `panel`, `center`) keep an
optional `:id`: they hold no state. Generating ids from the source (the form a widget edits, its
label) is for later.

```lisp
(label "text" :id :title :size 15)
(button "Save" :id :save :enabled t)                  ; true when clicked
(text-field name :id :name :width 160)                ; returns text, changed, submitted
(slider volume 0 11 :id :volume)                      ; returns value, changed
(checkbox "All" all :id :all)                         ; returns state, changed
(choice '("S" "M" "L") size :id :size)                ; returns the index, changed
(list-box items picked :id :list :rows 6)             ; returns the index, changed
(canvas 300 300 #'paint-board :id :board :focusable t) ; returns the events for the canvas
```

In a loop over data, the id is the item's own: `:id (task-id task)`, so that state follows the item
when the list is re-sorted.

## Where it lives after the fold

One system, `almighty-bolt`, at the root of the repository, laid out like `almighty-toki`
(`almighty-bolt.asd`, `src/`, `t/`). Packages `almighty-bolt/<file>`; the ui, which was the immediate
variant, in `almighty-bolt/ui/...`; the file that the ui and the apps import from the core is
`almighty-bolt/internals`. The apps (`almighty-bolt-7guis`, `almighty-bolt-stress`) and the
benchmarks stay their own systems and use the library like anyone.

## Decided on 2026-10-30

1. **Entry points:** `run-app`, `open-app` and `close-app`.
2. **What an app makes:** the function that describes the screen. "Variant" does not appear in the
   public API.
3. **The id of a widget:** a required `:id` keyword on every widget (containers optional); ids
   generated from the source are for another time.
4. **The theme:** fixed, one dark theme, not public in v0.1.
