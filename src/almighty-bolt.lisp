(defpackage #:almighty-bolt
  (:use)
  (:import-from #:almighty-bolt/app
                #:run-app
                #:open-app
                #:close-app
                #:state)
  (:import-from #:almighty-bolt/internals
                ;; state that survives a reload
                #:state-let
                ;; drawing in a canvas
                #:pack-color
                #:push-rect
                #:push-rounded-rect
                #:push-clip
                #:push-unclip
                #:context-font
                #:context-measure
                #:context-push-text
                #:context-caret-x
                #:context-index-at-x
                ;; events in a canvas
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
                ;; editing text
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
                #:combo-box
                #:slider
                #:choice
                #:checkbox
                #:switch
                #:spinner
                #:dropdown
                #:menu-button
                #:context-menu
                #:popover
                #:dialog
                #:toast
                #:shortcut
                #:open-file-dialog
                #:save-file-dialog
                #:list-box
                #:progress
                #:canvas
                #:scrim
                #:column
                #:row
                #:stack
                #:padding
                #:panel
                #:center
                #:scroll
                #:lazy-list
                #:with-theme)
  (:import-from #:almighty-bolt/ui/theme
                #:theme
                #:make-theme
                #:define-theme
                #:find-theme
                #:theme-names
                #:theme-name
                #:theme-setting
                #:theme-form
                #:+theme-settings+
                #:+theme-seeds+
                #:parse-color
                #:color-hex
                #:mix-colors
                #:contrast-ratio)
  (:import-from #:almighty-bolt/ui/immediate
                #:now
                #:ask-for-frame
                #:current-context)
  (:export #:run-app #:open-app #:close-app #:state
           #:state-let
           #:missing-id #:duplicate-id #:duplicate-id-id
           #:label #:paragraph #:button #:text-field #:text-area #:combo-box #:slider #:choice #:checkbox
           #:switch #:spinner #:dropdown #:menu-button #:context-menu #:popover
           #:dialog #:toast #:shortcut #:open-file-dialog #:save-file-dialog #:list-box
           #:progress #:canvas #:scrim
           #:column #:row #:stack #:padding #:panel #:center #:scroll #:lazy-list
           #:theme #:make-theme #:define-theme #:find-theme #:with-theme #:theme-names #:theme-name
           #:theme-setting #:theme-form #:+theme-settings+ #:+theme-seeds+
           #:parse-color #:color-hex #:mix-colors #:contrast-ratio
           #:now #:ask-for-frame #:current-context
           #:pack-color #:push-rect #:push-rounded-rect #:push-clip #:push-unclip
           #:context-font #:context-measure #:context-push-text #:context-caret-x
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
;;; The variant contract, the host and the display list's internals are not here (they are in
;;; almighty-bolt/internals, for the apps' tests and the benchmarks, and are not promised).
