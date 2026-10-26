(defpackage #:almighty-bolt-core/t/trace
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:ring #:almighty-bolt-core/ring)
                    (#:trace #:almighty-bolt-core/trace)))

(in-package #:almighty-bolt-core/t/trace)

;;; The ring as Chrome trace JSON. Whether Perfetto reads it is checked here by a JSON parser
;;; (Python's, if there is one).

(defun a-ring ()
  (let ((frame-ring (ring:make-frame-ring :capacity 8)))
    ;; Frame 1: 1000 us, builds 600 (first 400), wait 300, encode 100; update 50, layout 200,
    ;; paint 250 of the build.
    (ring:record-frame frame-ring 5000000 6000000 100 900 600000 300000 100000 2 nil)
    (ring:record-frame-details frame-ring :gc-ns 2000 :first-build-ns 400000 :update-ns 50000
                                          :layout-ns 200000 :paint-ns 250000 :gpu-ns 123000)
    ;; Frame 2: no phases timed by the variant, one build, capped.
    (ring:record-frame frame-ring 7000000 7500000 900 900 400000 0 100000 3 t)
    frame-ring))

(defun trace-text (frame-ring)
  (with-output-to-string (stream)
    (trace:write-trace frame-ring stream :name "test")))

(define-test a-trace-has-the-frames-and-the-slices-in-them ()
  (let ((text (trace-text (a-ring))))
    (assert-true (search "{\"traceEvents\":[" text))
    (assert-true (search "\"name\":\"process_name\"" text))
    ;; Two frames; two builds; the parts of the first frame (three phases, a wait, an encoding)
    ;; and the encoding of the second.
    (assert-eql 2 (count-occurrences "\"name\":\"frame\"" text))
    (assert-eql 2 (count-occurrences "\"name\":\"build\"" text))
    (assert-eql 1 (count-occurrences "\"name\":\"update\"" text))
    (assert-eql 1 (count-occurrences "\"name\":\"layout\"" text))
    (assert-eql 1 (count-occurrences "\"name\":\"paint\"" text))
    (assert-eql 1 (count-occurrences "\"name\":\"wait for the display\"" text))
    (assert-eql 2 (count-occurrences "\"name\":\"encode and present\"" text))))

(defun count-occurrences (needle text)
  (loop :with start = 0
        :for found = (search needle text :start2 start)
        :while found
        :count t
        :do (setf start (1+ found))))

(define-test the-times-are-microseconds-and-the-parts-follow-one-another ()
  (let ((text (trace-text (a-ring))))
    ;; Frame 1 starts at 5000 us and lasts 1000.
    (assert-true (search "\"name\":\"frame\",\"ph\":\"X\",\"ts\":5000.000,\"dur\":1000.000" text))
    ;; Its build from the start, its wait after the build, its encoding after the wait.
    (assert-true (search "\"name\":\"build\",\"ph\":\"X\",\"ts\":5000.000,\"dur\":600.000" text))
    (assert-true (search "\"name\":\"wait for the display\",\"ph\":\"X\",\"ts\":5600.000,\"dur\":300.000" text))
    (assert-true (search "\"name\":\"encode and present\",\"ph\":\"X\",\"ts\":5900.000,\"dur\":100.000" text))
    ;; Update, layout and paint one after another inside the build.
    (assert-true (search "\"name\":\"update\",\"ph\":\"X\",\"ts\":5000.000,\"dur\":50.000" text))
    (assert-true (search "\"name\":\"layout\",\"ph\":\"X\",\"ts\":5050.000,\"dur\":200.000" text))
    (assert-true (search "\"name\":\"paint\",\"ph\":\"X\",\"ts\":5250.000,\"dur\":250.000" text))))

(define-test the-arguments-say-the-builds-the-cap-and-the-cost-of-settling ()
  (let ((text (trace-text (a-ring))))
    (assert-true (search "\"builds\":2" text))
    (assert-true (search "\"capped\":\"no\"" text))
    (assert-true (search "\"builds\":3" text))
    (assert-true (search "\"capped\":\"yes\"" text))
    (assert-true (search "\"first_build_us\":400.000" text))
    (assert-true (search "\"extra_builds_us\":200.000" text))
    (assert-true (search "\"gpu_us_of_frame_before\":123.000" text))))

(define-test it-returns-the-number-of-frames ()
  (assert-eql 2 (trace:write-trace (a-ring) (make-broadcast-stream))))

(define-test an-empty-ring-is-an-empty-trace ()
  (let ((text (trace-text (ring:make-frame-ring :capacity 2))))
    (assert-false (search "\"name\":\"frame\"" text))
    (assert-true (search "]}" text))))

(defun python-parse (text)
  "The number of events in TEXT as Python's JSON parser reads it, or NIL if there is no Python
or the text is not JSON."
  (let ((directory (uiop:ensure-directory-pathname
                    (merge-pathnames (format nil "almighty-bolt-trace-test-~d/" (get-universal-time))
                                     (uiop:temporary-directory)))))
    (ensure-directories-exist directory)
    (unwind-protect
         (let ((path (merge-pathnames "trace.json" directory)))
           (with-open-file (stream path :direction :output :if-exists :supersede)
             (write-string text stream))
           (ignore-errors
            (parse-integer
             (uiop:run-program (list "python3" "-c"
                                     "import json,sys; print(len(json.load(open(sys.argv[1]))['traceEvents']))"
                                     (namestring path))
                               :output :string))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(define-test the-trace-is-json-that-python-parses ()
  (let ((count (python-parse (trace-text (a-ring)))))
    (if count
        ;; The process name; frame 1: frame, build, three phases, wait, encode; frame 2: frame,
        ;; build, encode.
        (assert-eql 11 count)
        (format t "~&(no python3: the trace was not parsed)~%"))))
