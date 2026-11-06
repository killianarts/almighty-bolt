(defpackage #:almighty-bolt
  (:use)
  (:local-nicknames (#:app #:almighty-bolt/app)
                    (#:core #:almighty-bolt/internals)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:widgets #:almighty-bolt/ui/widgets)
                    (#:drag #:almighty-bolt/ui/drag)
                    (#:split #:almighty-bolt/ui/split)
                    (#:tree #:almighty-bolt/ui/tree)
                    (#:dock #:almighty-bolt/ui/dock)
                    (#:app-pane #:almighty-bolt/ui/app-pane)
                    (#:inspector-pane #:almighty-bolt/machine/inspector-pane)
                    (#:catalogue #:almighty-bolt/ui/catalogue)
                    (#:views #:almighty-bolt/ui/views)
                    (#:design-surface #:almighty-bolt/ui/design-surface)
                    (#:font #:almighty-bolt/text/font)
                    (#:image-file #:almighty-bolt/render/image-file)
                    (#:image-atlas #:almighty-bolt/render/metal/image-atlas)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:immediate #:almighty-bolt/ui/immediate))
  (:import-from #:almighty-bolt/app
                #:run-app
                #:open-app
                #:close-app
                #:open-window
                #:close-window
                #:state)
  (:import-from #:almighty-bolt/internals
                #:state-let
                #:pack-color
                #:push-rect
                #:push-rounded-rect
                #:push-rounded-stroke
                #:push-shadow
                #:push-clip
                #:push-unclip
                #:context-font
                #:context-measure
                #:context-push-text
                #:context-push-image
                #:make-image
                #:image-width
                #:image-height
                #:context-caret-x
                #:context-index-at-x
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-alt+
                #:+modifier-control+
                #:+modifier-shift+
                #:+modifier-super+
                #:+key-delete-forward+
                #:+key-down+
                #:+key-end+
                #:+key-home+
                #:+key-left+
                #:+key-page-down+
                #:+key-page-up+
                #:+key-right+
                #:+key-up+
                #:+points-per-line+
                #:set-text-input-caret
                #:make-text-state
                #:edit-text
                #:text-state-string
                #:text-state-caret
                #:text-state-selection-p
                #:text-state-selection-start
                #:text-state-selection-end)
  (:import-from #:almighty-bolt/ui/ui
                #:duplicate-id
                #:duplicate-id-id)
  (:import-from #:almighty-bolt/ui/widgets
                #:missing-id
                #:label
                #:paragraph
                #:button
                #:text-field
                #:text-area
                #:code-view
                #:appearance
                #:combo-box
                #:slider
                #:choice
                #:checkbox
                #:switch
                #:tabs
                #:spinner
                #:dropdown
                #:menu-button
                #:context-menu
                #:popover
                #:dialog
                #:toast
                #:shortcut
                #:spacer
                #:grid
                #:pressable
                #:app-menu
                #:open-file-dialog
                #:save-file-dialog
                #:list-box
                #:progress
                #:image
                #:canvas
                #:scrim
                #:column
                #:row
                #:stack
                #:padding
                #:panel
                #:center
                #:stage
                #:scroll
                #:lazy-list
                #:with-theme)
  (:import-from #:almighty-bolt/ui/drag
                #:drag-source
                #:drop-target
                #:dragging)
  (:import-from #:almighty-bolt/ui/split
                #:split)
  (:import-from #:almighty-bolt/ui/tree
                #:tree)
  (:import-from #:almighty-bolt/ui/dock
                #:dock
                #:show-pane
                #:place-pane
                #:pane-group)
  (:import-from #:almighty-bolt/ui/app-pane
                #:app-pane
                #:app-pane-frames)
  (:import-from #:almighty-bolt/machine/inspector-pane
                #:inspector-pane)
  (:import-from #:almighty-bolt/ui/catalogue
                #:define-widget-entry)
  (:import-from #:almighty-bolt/ui/views
                #:define-view
                #:view)
  (:import-from #:almighty-bolt/ui/design-surface
                #:design-surface)
  (:import-from #:almighty-bolt/text/font
                #:load-font-file)
  (:import-from #:almighty-bolt/render/image-file
                #:load-image)
  (:import-from #:almighty-bolt/render/metal/image-atlas
                #:image-too-large)
  (:import-from #:almighty-bolt/ui/theme
                #:theme
                #:make-theme
                #:define-theme
                #:find-theme
                #:theme-names
                #:theme-name
                #:theme-setting
                #:theme-color
                #:theme-form
                #:typeface
                #:+font-roles+
                #:+theme-settings+
                #:+theme-seeds+
                #:+theme-colors+
                #:+theme-sizes+
                #:parse-color
                #:color-hex
                #:mix-colors
                #:contrast-ratio)
  (:import-from #:almighty-bolt/ui/immediate
                #:now
                #:ask-for-frame
                #:current-context
                #:this-window)
  (:export #:run-app #:open-app #:close-app #:open-window #:close-window #:state
           #:state-let
           #:missing-id #:duplicate-id #:duplicate-id-id
           #:label #:paragraph #:button #:text-field #:text-area #:code-view #:appearance #:combo-box #:slider #:choice #:checkbox
           #:switch #:tabs #:spinner #:dropdown #:menu-button #:context-menu #:popover
           #:dialog #:toast #:shortcut #:spacer #:grid #:pressable #:open-file-dialog #:save-file-dialog #:list-box
           #:progress #:canvas #:scrim #:design-surface #:app-menu #:drag-source #:drop-target #:dragging
           #:split #:tree #:dock #:show-pane #:place-pane #:pane-group #:app-pane #:app-pane-frames #:inspector-pane
           #:define-view #:view #:define-widget-entry
           #:column #:row #:stack #:padding #:panel #:center #:stage #:scroll #:lazy-list
           #:typeface #:+font-roles+ #:load-font-file
           #:image #:make-image #:load-image #:image-width #:image-height #:image-too-large
           #:theme #:make-theme #:define-theme #:find-theme #:with-theme #:theme-names #:theme-name
           #:theme-setting #:theme-color #:theme-form #:+theme-settings+ #:+theme-seeds+ #:+theme-colors+ #:+theme-sizes+
           #:parse-color #:color-hex #:mix-colors #:contrast-ratio
           #:now #:ask-for-frame #:current-context #:this-window
           #:pack-color #:push-rect #:push-rounded-rect #:push-rounded-stroke #:push-shadow
           #:push-clip #:push-unclip
           #:context-font #:context-measure #:context-push-text #:context-push-image #:context-caret-x
           #:context-index-at-x
           #:key-event-kind #:key-event-logical-key #:key-event-modifiers #:modifier-down-p
           #:+modifier-alt+ #:+modifier-control+ #:+modifier-shift+ #:+modifier-super+
           #:+key-delete-forward+ #:+key-down+ #:+key-end+ #:+key-home+ #:+key-left+
           #:+key-page-down+ #:+key-page-up+ #:+key-right+ #:+key-up+
           #:+points-per-line+ #:set-text-input-caret
           #:make-text-state #:edit-text #:text-state-string #:text-state-caret
           #:text-state-selection-p #:text-state-selection-start #:text-state-selection-end))

;;; The public face of the library: what a program that uses almighty-bolt writes, and nothing
;;; else. It defines nothing; every symbol is one that a file under src/ defines, imported and
;;; exported again, so that the library's insides can be moved without anyone's program changing.
;;; The app contract, the host and the display list's internals are not here (they are in
;;; almighty-bolt/internals, for the apps' tests and the benchmarks, and are not promised).
