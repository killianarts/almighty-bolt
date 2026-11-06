(defpackage #:almighty-bolt-studio/chrome
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:studio-theme #:almighty-bolt-studio/theme))
  (:export #:caps
           #:words
           #:+small+
           #:+separator+
           #:section-header
           #:rail
           #:status-bar))

(in-package #:almighty-bolt-studio/chrome)

;;; What the studio's chrome is made of, as the prototype draws it: words in the label font, in
;;; capitals, with a square between them; a section's header, its title and its kana and a count
;;; over a line; the rail down the left, which says the mode, the view and how many widgets it has;
;;; and the status bar along the bottom, which says what was last done and how things stand.

(defparameter +separator+ "■"
  "What goes between the parts of the breadcrumb and of what is said about the image.")

(defun caps (text)
  (string-upcase text))

(defparameter +small+ '(:label :size 10)
  "The chrome's words: the theme's label font, at the prototype's small size.")

(defun words (id parts &key (color :dim-text) (bright 1) tooltip)
  "PARTS, a list of strings, in a row with the separator between them: the one at BRIGHT (an index,
or NIL) in the text's colour, the rest in COLOR. TOOLTIP is said by the last."
  (bolt:row (:id id :gap 8 :cross-align :center)
    (loop :for (part . more) :on parts
          :for i :from 0
          :do (unless (zerop i)
                (bolt:label +separator+ :id (list :separator i) :font +small+ :color :dim-text))
              (bolt:label (caps part) :id (list :part i) :font +small+
                                      :color (if (eql i bright) :text color)
                                      :tooltip (and (null more) tooltip)))))

(defparameter +kana+
  '((:palette-heading "Catalogue" "カタログ") (:views-heading "Views" "ビュー")
    (:outline-heading "Outline" "アウトライン") (:selection-heading "Selected" "選択")
    (:wiring-heading "Wiring" "配線") (:inspector-heading "Inspector" "インスペクタ")
    (:problems-heading "Problems" "問題") (:events-heading "Events" "イベント")
    (:agent-heading "Agent" "エージェント") (:repl-heading "REPL" "リプル")
    (:state-heading "State" "状態") (:view-code-heading "View" "ビュー") (:code-heading "Code" "コード")
    (:wires-heading "Wires" "ワイヤー") (:wire-heading "Selected wire" "配線"))
  "The studio's sections: each heading's id, its title and its kana. A heading not here is called
what it is given.")

(defun section-header (id title &optional count)
  "A section's header: its title (the studio's, if +KANA+ has the heading ID, else TITLE) in
capitals, with the label ID, which what it heads is labelled by; its kana; a line; and COUNT, if
there is one."
  (let ((known (assoc id +kana+)))
    (bolt:row (:id (list id :header) :gap 8 :cross-align :center :fill t)
      (bolt:label (caps (if known (second known) title)) :id id
                  :font '(:label :size 11 :weight :bold))
      (when known
        (bolt:label (third known) :id :kana :font +small+ :color :dim-text))
      (bolt:panel (:id :line :color (studio-theme:tool-color :line) :radius 0 :height 1 :flex 1))
      (when count
        (bolt:label (princ-to-string count) :id :count :font +small+ :color :dim-text)))))

(defun rail (mode about)
  "The rail down the left of the studio: whether a view is live, as the title bar's dot, and the
MODE, the view and how many widgets it has (as ABOUT, the GUI designer's, says), turned to read
upwards."
  (bolt:panel (:id :rail :color :background :radius 0 :width 36 :min-height 0)
    (bolt:column (:id :rail-column :fill t :cross-align :center :gap 12)
      (bolt:spacer)
      (bolt:panel (:id :rail-dot :radius 4 :width 8 :height 8
                   :color (if (getf about :live) (studio-theme:tool-color :ok) (studio-theme:tool-color :line-strong))))
      (bolt:label (caps (format nil "~a ~a view ~a ~a ~d widget~:p"
                                (symbol-name mode) +separator+
                                (getf about :name) +separator+
                                (length (getf about :ids))))
                  :id :rail-words :font +small+ :color :dim-text :turn :up)
      (bolt:spacer))))

(defun status-bar (parts)
  "The status bar: PARTS (strings), what was last done first, with the studio's motto at the right."
  (bolt:panel (:id :status-bar :color :background :radius 0 :left 16 :right 16 :height 26)
    (bolt:row (:id :status-row :gap 12 :cross-align :center :fill t)
      (words :status-words parts :bright nil)
      (bolt:spacer)
      (bolt:label "オールマイティーになれ" :id :motto :font +small+ :color :dim-text))))
