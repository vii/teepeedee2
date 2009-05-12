(in-package #:tpd2.ml)

; From http://www.w3.org/TR/REC-CSS2/propidx.html
; if you want more just use "strings"
(define-constant +css-properties+ '(
				:azimuth
				:background
				:background-color 
				:background-image 
				:background-repeat 
				:background-attachment 
				:background-position 
				:background-attachment
				:background-color
				:background-image
				:background-position
				:background-repeat
				:border
				:border-width 
				:border-style 
				:border-collapse
				:border-color
				:border-spacing
				:border-style
				:border-top
				:border-right
				:border-bottom
				:border-left
				:border-top-width 
				:border-style 
				:border-top-color
				:border-right-color
				:border-bottom-color
				:border-left-color
				:border-top-style
				:border-right-style
				:border-bottom-style
				:border-left-style
				:border-top-width
				:border-right-width
				:border-bottom-width
				:border-left-width
				:border-width
				:bottom
				:caption-side
				:clear
				:clip
				:color
				:content
				:counter-increment
				:counter-reset
				:cue
				:cue-before 
				:cue-after 
				:cursor
				:direction
				:display
				:elevation
				:empty-cells
				:float
				:font
				:font-style 
				:font-variant 
				:font-weight 
				:font-size 
				:font-family 
				:font-family
				:font-size
				:font-size-adjust
				:font-stretch
				:font-style
				:font-variant
				:font-weight
				:height
				:left
				:letter-spacing
				:line-height
				:list-style
				:list-style-type 
				:list-style-position 
				:list-style-image
				:list-style-position
				:list-style-type
				:margin
				:margin-top
				:margin-right
				:margin-bottom
				:margin-left
				:marker-offset
				:marks
				:max-height
				:max-width
				:min-height
				:min-width
				:orphans
				:outline
				:outline-color 
				:outline-style 
				:outline-color
				:outline-style
				:outline-width
				:overflow
				:padding
				:padding-top
				:padding-right
				:padding-bottom
				:padding-left
				:page
				:page-break-after
				:page-break-before
				:page-break-inside
				:pause
				:pause-after
				:pause-before
				:pitch
				:pitch-range
				:play-during
				:position
				:quotes
				:richness
				:right
				:size
				:speak
				:speak-header
				:speak-numeral
				:speak-punctuation
				:speech-rate
				:stress
				:table-layout
				:text-align
				:text-decoration
				:text-indent
				:text-shadow
				:text-transform
				:top
				:unicode-bidi
				:vertical-align
				:visibility
				:voice-family
				:volume
				:white-space
				:widows
				:width
				:word-spacing
				:z-index

				:x-opacity
				:x-column-width
				:x-column-gap
				)
  :test 'equalp)

;; Write CSS like this: (("p.asdfsaf" "p + p") :property "value" :property "value")

(defun validate-properties (properties)
  (loop for (property) on properties by #'cddr
	when (keywordp property) do 
	(assert (member property +css-properties+) (property))))

(defun css-output-properties (properties)
  (append (list " {")
	  (css-output-properties-form properties)
	  (list  "}" #\Newline)))

(defgeneric css-output-selector-form (selector properties))

(defmethod css-output-selector-form ((str string) properties)
  (append (list str)
	  (css-output-properties properties)))

(defun css-selector-form-to-string (form)
  (cond ((symbolp form)
	 (assert (and (eql #\< (char (symbol-name form) 0)) (fboundp form)) (form) "Misspelled? ~A" form)
	 (subseq (symbol-name form) 1))
	(t form)))

(defmethod css-output-selector-form ((sym symbol) properties)
  (css-output-selector-form (css-selector-form-to-string sym) properties))

(defmethod css-output-selector-form ((l list) properties)
  (case (first l)
    (quote
     (append
      (rest l)
      (css-output-properties properties)))
    (:possibly-unsupported-selectors
     (loop for form in (rest l)
	   append (css-output-selector-form form properties)))
    (t
     (append
      (loop for once = t then nil
	    for form in l
	    unless once collect ","
	    collect (css-selector-form-to-string form))
      (css-output-properties properties)))))

(defgeneric css-output-property-form (property value))

(defun css-output-property-value-form (value)
  (loop for v in (force-list value) for once = t then nil unless once collect " "  collect v))

(defmethod css-output-property-form (property value)
  (list* (if (keywordp property) 
	     (string-downcase (symbol-name property)) 
	     property)
	 ": " 
	 (css-output-property-value-form value)))

(defun css-output-property-under-different-names (names value)
  (loop for p in names 
	for once = nil then t
	append
	(css-output-property-form p value)
	unless once collect ";"))

(defmethod css-output-property-form ((property (eql :x-opacity)) value)
  (check-type value (real 0 1))
  (append
   (css-output-property-under-different-names '("opacity" "-moz-opacity") value)
   (list ";")
   (css-output-property-form "filter" (strcat "alpha(opacity="  (floor (* 100 value)) ")"))))

(defmethod css-output-property-form ((property (eql :x-column-gap)) value)
  (css-output-property-under-different-names '("-moz-column-gap" "column-gap") value))

(defmethod css-output-property-form ((property (eql :x-column-width)) value)
  (css-output-property-under-different-names '("-moz-column-width" "column-width") value))

(defun css-output-properties-form (properties)
  (loop for (property value) on properties by #'cddr
     append (css-output-property-form property value)
     collect  ";"))

(defmacro css-html-style (&body selector-properties)
  (flet ((validate (selector properties)
	   (declare (ignore selector))
	   (validate-properties properties)))
    `(tpd2.ml.html:<style :type "text/css"
			  (output-ml-comment
			   #\Newline
			   ,@(loop for sp in selector-properties
				  for selector = (first sp)
				  for properties = (rest sp)
				  do
				  (validate selector properties)
				  append (css-output-selector-form selector properties))))))

(defmacro css-attrib (&rest properties)
  (validate-properties properties)
  `(sendbuf-to-byte-vector 
    (with-sendbuf ()
      ,@(css-output-properties-form properties))))
