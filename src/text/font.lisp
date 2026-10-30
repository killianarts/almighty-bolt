(defpackage #:almighty-bolt/text/font
  (:use #:cl)
  (:import-from #:almighty-bolt/text/core-foundation
                #:ccall
                #:cf-string
                #:cf-release)
  (:export #:font
           #:make-font
           #:system-font
           #:font-name
           #:font-size
           #:font-ct-font
           #:font-ascent
           #:font-descent
           #:font-leading
           #:font-line-height
           #:font-lines
           #:font-older-lines))

(in-package #:almighty-bolt/text/font)

;;; A font at a size. It wraps a CTFont and caches the lines shaped with it (see
;;; the line module), so a string measured twice is shaped once. Metrics are
;;; single floats: they are immediate values, so passing them around allocates
;;; nothing, and the display list uses singles too.
;;;
;;; Fonts are never released; they live as long as the process.

(defstruct (font (:constructor %make-font))
  (name "" :type string :read-only t)
  (size 0f0 :type single-float :read-only t)
  (ct-font 0 :type (unsigned-byte 64) :read-only t)
  (ascent 0f0 :type single-float :read-only t)
  (descent 0f0 :type single-float :read-only t)
  (leading 0f0 :type single-float :read-only t)
  ;; The lines shaped with this font, in two generations (see GET-LINE in the line module): the
  ;; ones used lately, and the ones before them.
  (lines (make-hash-table :test 'equal))
  (older-lines (make-hash-table :test 'equal)))

(defun font-metrics-from (name size ct-font)
  (%make-font :name name
              :size (float size 1f0)
              :ct-font ct-font
              :ascent (float (ccall "CTFontGetAscent" :double (:ulong ct-font)) 1f0)
              :descent (float (ccall "CTFontGetDescent" :double (:ulong ct-font)) 1f0)
              :leading (float (ccall "CTFontGetLeading" :double (:ulong ct-font)) 1f0)))

(defun make-font (name size)
  "The font called NAME (such as \"Helvetica\" or \"Menlo\") at SIZE points. If the
system has no such font, the system font is used, as Core Text does."
  (let* ((cf-name (cf-string name))
         (ct-font (ccall "CTFontCreateWithName" :ulong
                         (:ulong cf-name) (:double (float size 1d0)) (:ulong 0))))
    (cf-release cf-name)
    (font-metrics-from name size ct-font)))

(defun system-font (size)
  "The system user interface font at SIZE points."
  (let ((ct-font (ccall "CTFontCreateUIFontForLanguage" :ulong
                        (:int 2)        ; kCTFontUIFontSystem
                        (:double (float size 1d0))
                        (:ulong 0))))
    (font-metrics-from "system" size ct-font)))

(defun font-line-height (font)
  "The distance between baselines: ascent, descent and leading."
  (+ (font-ascent font) (font-descent font) (font-leading font)))
