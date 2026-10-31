# Getting started

This takes you from a clone of the repository to a small app that you change while it is open. It assumes the [README](../README.md) has got you as far as `make deps`. Everything here is done in the REPL of an editor; the examples are in [`examples/`](../examples) and are run by `make smoke`, so what follows works.

## 1. A REPL that can open a window

almighty-bolt opens its window from a thread other than the main one and runs the window's event loop on the main thread, which has to be waiting for something. An editor that starts Lisp itself does exactly that: it starts `sbcl` with a pipe for its input, sends it the form that starts its server, and then waits. So:

- **Emacs with Sly.** Start with `C-u M-x sly` and pick `sbcl`. (If your `sly-default-lisp` is something else, such as CCL, this library does not run in it, and loading it there stops with a message that says so.) Open a file from the repository first, so that Sly starts in the right directory.
- **SLIME** with threads works the same way.
- **A terminal `sbcl`** runs its REPL on the main thread, which is the window's. There, use `run-app` (section 6) and do not expect a REPL while the window is open.

In the REPL, point ASDF at the repository and load the examples:

```lisp
(require :asdf)
(asdf:initialize-source-registry
 '(:source-registry (:tree "/path/to/almighty-bolt/") :ignore-inherited-configuration))
(asdf:load-system "almighty-bolt-examples")
```

## 2. Open a window

```lisp
(almighty-bolt:open-app 'almighty-bolt-examples/counter:make-counter
                        :title "Counter" :width 360 :height 160)
;; => #<ALMIGHTY-BOLT/PLATFORM/MACOS/HOST:HOST {...}>
```

A window with a number and a button opens, and the REPL prompt comes back. Click the button a few times. Close the window: Lisp is still running, and the same call opens it again (from the start). `(almighty-bolt:close-app)` closes it from the REPL.

## 3. Read the app

[`examples/counter.lisp`](../examples/counter.lisp) is the whole thing:

```lisp
(defun make-counter ()
  (kept-let ((count 0))
    (lambda ()
      (padding (:all 24)
        (row (:gap 16 :cross-align :center)
          (label (princ-to-string count) :id :count :size 32)
          (when (button "Count" :id :count-button)
            (incf count)))))))
```

Three things to see.

- The function that `make-counter` returns **describes the whole screen** and is called again for every frame. There are no widgets to create and keep; the screen is a function of the state.
- `(button ...)` **returns true on the frame it is clicked**. What the click does is written where the button is: `(incf count)`. A text field returns the text now and whether it was changed this frame, and you pass the text back in the next frame.
- `kept-let` is `let` for the state. You will see why next.

## 4. Change it while it is open

Open `examples/counter.lisp` in the editor, with the window still open and the count at, say, 5. Add a label next to the button:

```lisp
(label (princ-to-string count) :id :count :size 32)
(label "times" :id :times)            ; <- new
(when (button "Count" :id :count-button)
```

Put the cursor in `make-counter` and compile it with `C-c C-c`. The window changes within about half a second, and the count is still 5. That is `kept-let`: the window kept the variable's cell, and the new `make-counter` was handed it instead of making a new one. Click the button, and it goes on from 5.

What was kept is the state you declared, and also what the person did to the widgets: the caret and selection of text fields, the scroll of lists, and which widget has the keyboard. What is not kept is anything you wrote with a plain `let`: it starts again, which is exactly the difference you want to be able to choose.

Edit the initial form of a variable, `(count 0)` to `(count 10)`, and compile: that variable starts over, so an edited initial value shows. Close the window and open it again to start the whole app over.

If the new code does not work, say a misspelled function that is called in the first frame, the window keeps the old app and the error is printed (in `*inferior-lisp*` if you opened the window from a source buffer, in the REPL if you opened it from there). Fix it and compile again.

Look at the app from the REPL:

```lisp
(almighty-bolt:app-value 'almighty-bolt-examples/counter::count)   ; => 5
```

## 5. Ids

Every widget has a required `:id`, and you wrote them above without being told why. The ui matches what it described this frame with what it described last frame by the id, and everything the person has done to a widget is kept under it. Without one, adding a widget above a text field would give the field's caret to the widget after it; so leaving it out is an error (`missing-id`, with the widget's name), and so are two things in one container with the same id (`duplicate-id`, which names both and the container), because they would be one widget.

Open the second example, [`examples/todo.lisp`](../examples/todo.lisp):

```lisp
(almighty-bolt:open-app 'almighty-bolt-examples/todo:make-todo :title "To do" :width 360 :height 320)
```

It has a text field, a button, a check box for each thing and a count of what is left. The check boxes are made in a loop, and their ids are the things' own numbers (`:id (thing-id thing)`), so that when the list changes under it, the keyboard stays with the thing it was on. Type something and press Return, or the Add button; check things off; then compile `make-todo` after changing a label, and see that the list, the half-typed text and the caret are all where they were.

## 6. As a program

```lisp
(almighty-bolt:run-app 'almighty-bolt-examples/todo:make-todo :title "To do" :width 360 :height 320)
```

on the main thread (`sbcl --load app.lisp`) opens the window and ends the process when it is closed. `:hot t` makes the app again when code is compiled into the image, and `:watch-systems '("my-system")` also loads those systems again when a file in them is saved, for editing in a program that does not compile into the running image.

## 7. Let an agent use it

The screen can be read as a tree of things with roles, names and values, and worked by name. An [MCP](https://modelcontextprotocol.io) server for the open window puts that in an agent's hands. With the counter open (section 2):

```lisp
(asdf:load-system "almighty-bolt/mcp")      ; needs com.inuoe.jzon, which make deps fetched
(almighty-bolt/mcp:start-mcp-server :port 7071)
;; MCP server on 127.0.0.1:7071. For an MCP client that wants a pipe: command nc, arguments 127.0.0.1 7071
```

Tell an MCP client to start `nc 127.0.0.1 7071` as its server (in Claude Code: `claude mcp add counter -- nc 127.0.0.1 7071`), and ask it to press the button. It calls `snapshot` and reads

```
window @0,0,222,80
  static-text "5" id=count @24,24,36,32
  button "Count" id=count-button @130,26,68,29
```

then `click` with the role `button` and the name `Count` (or the id `count-button`), which answers with the screen afterwards: the count is 6. Edit the app and compile it, and the agent's next `snapshot` is of the new screen. `(almighty-bolt/mcp:stop-mcp-server)` closes the server and its connections. It listens on this machine only and nothing starts it but that call; anything on the machine that can connect can press your app's buttons.

The same things are done from Lisp, without MCP, by the machine interface (`almighty-bolt/machine`); the [README](../README.md#working-an-app-from-a-program-or-an-agent) lists the calls.

## 8. Where next

- The widgets, the containers and the canvas are in the [README](../README.md).
- A `canvas` is where you draw yourself and handle the pointer and the keyboard; [`examples/7guis/circles/immediate.lisp`](../examples/7guis/circles/immediate.lisp) and [`examples/7guis/cells/immediate.lisp`](../examples/7guis/cells/immediate.lisp) are small ones, and [`bench/stress/editor/immediate.lisp`](../bench/stress/editor/immediate.lisp) is a text editor with input-method support.
- `make test` and `make smoke` are what has to pass before a change is committed, and [`CLAUDE.md`](../CLAUDE.md) says how the project is worked on.
