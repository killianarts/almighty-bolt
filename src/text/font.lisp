(defpackage #:almighty-bolt/text/font
  (:use #:cl)
  (:import-from #:almighty-bolt/text/core-foundation
                #:ccall
                #:cf-constant
                #:cf-string
                #:cf-release
                #:cf-array-count
                #:cf-array-ref
                #:cf-dictionary
                #:cf-number-double)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:lisp-string)
  (:export #:font
           #:make-font
           #:system-font
           #:font-name
           #:font-size
           #:font-weight
           #:font-tracking
           #:font-face-name
           #:weight-number
           #:load-font-file
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
  ;; How heavy it is, 1 to 1000 as CSS has it (400 regular, 700 bold), as asked for; and the points
  ;; of space after each character.
  (weight 400 :type (integer 1 1000) :read-only t)
  (tracking 0f0 :type single-float :read-only t)
  (ct-font 0 :type (unsigned-byte 64) :read-only t)
  (ascent 0f0 :type single-float :read-only t)
  (descent 0f0 :type single-float :read-only t)
  (leading 0f0 :type single-float :read-only t)
  ;; The lines shaped with this font, in two generations (see GET-LINE in the line module): the
  ;; ones used lately, and the ones before them.
  (lines (make-hash-table :test 'equal))
  (older-lines (make-hash-table :test 'equal)))

(defun font-metrics-from (name size ct-font &optional (weight 400) (tracking 0))
  (%make-font :name name
              :size (float size 1f0)
              :weight weight
              :tracking (float tracking 1f0)
              :ct-font ct-font
              :ascent (float (ccall "CTFontGetAscent" :double (:ulong ct-font)) 1f0)
              :descent (float (ccall "CTFontGetDescent" :double (:ulong ct-font)) 1f0)
              :leading (float (ccall "CTFontGetLeading" :double (:ulong ct-font)) 1f0)))

(defparameter +weights+
  '((:thin 100) (:extra-light 200) (:light 300) (:regular 400) (:medium 500) (:semibold 600)
    (:bold 700) (:extra-bold 800) (:black 900))
  "The weights by name, as CSS numbers them.")

(defun weight-number (weight)
  "WEIGHT as a number from 1 to 1000: a number, or one of the names of +WEIGHTS+ (NIL is
regular)."
  (cond ((null weight) 400)
        ((and (integerp weight) (<= 1 weight 1000)) weight)
        ((second (assoc weight +weights+)))
        (t (error "~s is not a weight: a number from 1 to 1000 (400 is regular, 700 bold), or one ~
                   of ~{~(~s~)~^, ~}." weight (mapcar #'first +weights+)))))

(defparameter +core-text-weights+
  '((100 -0.8d0) (200 -0.6d0) (300 -0.4d0) (400 0d0) (500 0.23d0) (600 0.3d0) (700 0.4d0)
    (800 0.56d0) (900 0.62d0))
  "Where each weight is on Core Text's scale, from -1 to 1, as AppKit's font weights are.")

(defun core-text-weight (weight)
  "The weight WEIGHT (1 to 1000) on Core Text's scale, between the named weights by a straight line."
  (loop :for ((low low-ct) (high high-ct)) :on +core-text-weights+
        :while high
        :when (<= weight high)
          :return (if (<= weight low)
                      low-ct
                      (+ low-ct (* (- high-ct low-ct) (/ (- weight low) (- high low)))))
        :finally (return 0.62d0)))

(defconstant +italic-trait+ 1 "kCTFontTraitItalic, among a font's symbolic traits.")

(defun descriptor-trait (descriptor key)
  "The trait KEY (a Core Text constant, as CF-CONSTANT gives it) of the font DESCRIPTOR, as a
double, or NIL."
  (let ((traits (ccall "CTFontDescriptorCopyAttribute" :ulong
                       (:ulong descriptor) (:ulong (cf-constant "kCTFontTraitsAttribute")))))
    (unless (zerop traits)
      (prog1 (cf-number-double (ccall "CFDictionaryGetValue" :ulong
                                      (:ulong traits) (:ulong key)))
        (cf-release traits)))))

(defun font-of-weight (ct-font size weight)
  "A new font of CT-FONT's family, at SIZE, whose weight is nearest WEIGHT (1 to 1000) among the
family's upright faces as wide as CT-FONT (Helvetica Neue's bold, not its condensed bold), or the
nearest in width if none is; or NIL if the family has none (it is a font of its own)."
  (let* ((family (ccall "CTFontCopyFamilyName" :ulong (:ulong ct-font)))
         (attributes (cf-dictionary (cf-constant "kCTFontFamilyNameAttribute") family))
         (wanted (ccall "CTFontDescriptorCreateWithAttributes" :ulong (:ulong attributes)))
         (faces (ccall "CTFontDescriptorCreateMatchingFontDescriptors" :ulong
                       (:ulong wanted) (:ulong 0)))
         (target (core-text-weight weight))
         (width (let ((own (ccall "CTFontCopyFontDescriptor" :ulong (:ulong ct-font))))
                  (prog1 (or (descriptor-trait own (cf-constant "kCTFontWidthTrait")) 0d0)
                    (cf-release own))))
         (best nil)
         (distance 0d0))
    (cf-release attributes)
    (cf-release wanted)
    (cf-release family)
    (unless (zerop faces)
      (dotimes (i (cf-array-count faces))
        (let* ((face (cf-array-ref faces i))
               (face-weight (or (descriptor-trait face (cf-constant "kCTFontWeightTrait")) 0d0))
               (face-width (or (descriptor-trait face (cf-constant "kCTFontWidthTrait")) 0d0))
               (symbolic (or (descriptor-trait face (cf-constant "kCTFontSymbolicTrait")) 0d0)))
          (unless (logtest (round symbolic) +italic-trait+)
            ;; The width first (a step in width outweighs any in weight), then the weight.
            (let ((d (+ (* 10 (abs (- face-width width))) (abs (- face-weight target)))))
              (when (or (null best) (< d distance))
                (setf best face distance d))))))
      (prog1 (and best (ccall "CTFontCreateWithFontDescriptor" :ulong
                              (:ulong best) (:double (float size 1d0)) (:ulong 0)))
        (cf-release faces)))))

(defun make-font (name size &optional weight (tracking 0))
  "The font called NAME (such as \"Helvetica\" or \"Menlo\") at SIZE points, of WEIGHT (a number from
1 to 1000, or a name such as :regular, :medium or :bold: see WEIGHT-NUMBER), with TRACKING points of
space after each character. The face is the upright one of NAME's family whose weight is nearest
WEIGHT; if the system has no font called NAME, the system font is used, as Core Text does."
  (let* ((weight (weight-number weight))
         (cf-name (cf-string name))
         (ct-font (ccall "CTFontCreateWithName" :ulong
                         (:ulong cf-name) (:double (float size 1d0)) (:ulong 0))))
    (cf-release cf-name)
    (unless (= weight 400)
      (let ((weighted (font-of-weight ct-font size weight)))
        (when weighted
          (cf-release ct-font)
          (setf ct-font weighted))))
    (font-metrics-from name size ct-font weight tracking)))

(defun font-face-name (font)
  "The PostScript name of the face FONT is drawn in, such as \"HelveticaNeue-Medium\"."
  (let ((name (ccall "CTFontCopyPostScriptName" :ulong (:ulong (font-ct-font font)))))
    (prog1 (lisp-string name)
      (cf-release name))))

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

;;; --- fonts from files ---

(defconstant +process-scope+ 1 "kCTFontManagerScopeProcess: for this process, while it runs.")
(defconstant +already-registered+ 105 "kCTFontManagerErrorAlreadyRegistered.")

(defun load-font-file (path)
  "Make the fonts in the file at PATH (a .ttf, .otf or .ttc) ones that this process can ask for by
name, as if they were installed, until it ends; nothing is installed for the person. Returns the
names of their families, as a font is asked for by. A file loaded already is loaded again without
complaint. Load a font before the window first asks for it: a window keeps the fonts it was given,
and one it asked for by a name it did not have then is the system font."
  (let ((file (probe-file path)))
    (unless file
      (error "There is no font file ~a." path))
    (let* ((cf-path (cf-string (namestring file)))
           (url (ccall "CFURLCreateWithFileSystemPath" :ulong
                       (:ulong 0) (:ulong cf-path) (:long 0) (:bool nil))))
      (cf-release cf-path)
      (unwind-protect
           (progn
             (sb-alien:with-alien ((failure sb-alien:unsigned-long 0))
               (unless (ccall "CTFontManagerRegisterFontsForURL" :bool
                              (:ulong url) (:uint +process-scope+)
                              (:pointer (sb-sys:sap-int (sb-alien:alien-sap
                                                         (sb-alien:addr failure)))))
                 (let ((code (if (zerop failure)
                                 0
                                 (ccall "CFErrorGetCode" :long (:ulong failure)))))
                   (unless (zerop failure) (cf-release failure))
                   (unless (= code +already-registered+)
                     (error "The fonts in ~a could not be loaded (Core Text's error ~d)."
                            (namestring file) code)))))
             (let ((faces (ccall "CTFontManagerCreateFontDescriptorsFromURL" :ulong (:ulong url)))
                   (families '()))
               (unless (zerop faces)
                 (dotimes (i (cf-array-count faces))
                   (let ((family (ccall "CTFontDescriptorCopyAttribute" :ulong
                                        (:ulong (cf-array-ref faces i))
                                        (:ulong (cf-constant "kCTFontFamilyNameAttribute")))))
                     (unless (zerop family)
                       (pushnew (lisp-string family) families :test #'string=)
                       (cf-release family))))
                 (cf-release faces))
               (nreverse families)))
        (cf-release url)))))
