# almighty-bolt: the API as proposed (draft for review)

*Stage 4, step 0 of the roadmap. Nothing here is renamed yet. The names below are what the rename
script will produce if you agree; the open questions are at the end. This file becomes the README
in Stage 5.*

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
          (label (princ-to-string count) :size 32)
          (when (button "Count")         ; true on the frame it is clicked
            (incf count)))))))

(open-app 'make-counter :title "Counter" :width 360 :height 160)
```

`make-counter` is given by name, so that compiling it again changes the window. It returns the
function that describes the screen. Today that is `(make-immediate-variant (lambda () ...))`; the
sketch has `open-app` do that wrapping.

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

## How widgets are called, as they are today

```lisp
(label "text" :id :title :size 15)           ; id is a keyword argument, and optional
(button "Save" :id :save :enabled t)         ; true when clicked
(text-field :name name :width 160)           ; id first, then the value; returns text, changed, submitted
(slider :volume volume 0 11)                 ; id, value, minimum, maximum; returns value, changed
(checkbox :all "All" all)                    ; id, label, state; returns state, changed
(choice :size '("S" "M" "L") size)           ; id, options, selected index; returns index, changed
(list-box :list items picked :rows 6)        ; id, items, selected index; returns index, changed
(canvas :board 300 300 #'paint-board :focusable t)   ; returns the events for the canvas
```

Stateful widgets take their id first and positionally, because it is what keeps a field's caret and a
list's scroll across frames and across a reload; `label`, `button` and `paragraph` have nothing to
keep and take `:id` only when a test or a reload needs to find them.

## Where it lives after the fold

One system, `almighty-bolt`, at the root of the repository, laid out like `almighty-toki`
(`almighty-bolt.asd`, `src/`, `t/`). Packages `almighty-bolt/<file>`; the ui, now the immediate
variant, in `almighty-bolt/ui/...`. The apps (`almighty-bolt-7guis`, `almighty-bolt-stress`) and the
benchmarks stay their own systems and use the library like anyone.

## Questions

1. **Names of the entry points.** `run-app`, `open-app` and `close-app` (the 7GUIs launcher already
   has them). `open` and `close` themselves are Common Lisp's.
2. **What an app makes.** The function that describes the screen (as above), against
   `(make-immediate-variant ...)` as today. "Variant" would then not appear in the public API.
3. **The id of a widget.** As today (positional for the stateful ones, `:id` for the others), or `:id`
   for every widget. Positional is shorter and cannot be forgotten where it matters; `:id`
   everywhere is one rule. Changing it means every call of `text-field`, `slider`, `choice`,
   `checkbox`, `list-box`, `canvas` and `scrim` in the 7GUIs and stress apps, mechanically.
4. **The theme.** Today a fixed dark theme in `theme.lisp` (colours, Helvetica 15). For v0.1 either keep
   it fixed and not public, or make it a value the app can give, which the frame caches would have
   to include.
