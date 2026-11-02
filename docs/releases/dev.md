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
