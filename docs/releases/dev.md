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

### Major feature name

What it lets you do, in a sentence or two, and the problem it replaces.

```lisp
(example)
```

What the example does or returns. List the new options if there are several.
Links to reference documentation: [`symbol`](../reference/page.md#symbol).

### Minor features

#### Windowing

- [`symbol`](../reference/page.md#symbol) now does X.

#### Rendering

#### Text

#### Layout

#### Input

- `(shortcut #\s :command)` is true in the frame its keys are pressed, for the app's own keyboard shortcuts: `(when (bolt:shortcut #\s :command) (save))`. It takes the key, and the modifiers held must be the ones given.
- `canvas` takes `:cursor`, the shape the pointer has over it: a crosshair to draw with, a pointing hand over something that can be pressed, a hand to drag with, arrows to resize. The app can give another each frame.

#### Machine interface

## Backwards incompatible changes in X.Y

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

- Text fields, combo boxes and text areas could not undo. Now Command-Z undoes an edit and Command-Shift-Z does it again. A run of typing is undone as one, and so is a run of deleting; a paste, a cut or a Return is one of its own, and a press or a key that moves the caret ends a run. When the app gives the field text of its own, what was done before can no longer be undone into it.
- An app whose whole interface was a dialog (or anything else that floats), with nothing described under it, showed an empty window. It now shows the dialog over an empty screen.
- The pointer was an arrow everywhere. It is now a text cursor over text fields, text areas, combo boxes and selectable text, as in other Mac apps.
- The menu bar had the application's menu with Quit only. It now has the menus every Mac app has: Hide, Hide Others and Show All beside Quit; Edit, with Undo, Redo, Cut, Copy, Paste and Select All, which do what their keys do in whatever has the keyboard; and Window, with Minimize, Zoom and Close.
