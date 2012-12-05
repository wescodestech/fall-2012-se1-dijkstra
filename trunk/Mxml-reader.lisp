;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Mxml-reader.lisp
  Reads and parses the xml data file.
  Date: December 6th, 2012
  Team Dijkstra
|#

(require "Ixml-reader.lisp")

(module Mxml-reader-private
  (include-book "io-utilities" :dir :teachpacks)

  ; (extractXMLTag xml tag)
  ; Extracts the contents of an XML tags (all the data that is contained 
  ; between the tags)
  ;
  ; xml - the string/char list that contains the RAW XML data
  ; tag - the tag that will be extracted from the XML.
  ;
  ; The data between the opening <TAG> and closing </TAG> is returned as a 
  ; char list.  The data that occurs after the </TAG> is located, is 
  ; returned as a string unparsed. The return format for this function is 
  ; (parsed_char_array . the_rest_as_string)
  (defun extractXMLTag (xml tag)
    (if (listp xml)
        (if (equal (car xml) #\<)
            (if (equal (cadr xml) #\/)
                (if (equal (list (caddr xml) (cadddr xml)) (coerce tag 'list))
                    (let* ((therest (cdr (cddddr xml))))
                      (if (equal nil therest)
                          (cons nil nil)
                          (cons nil therest)))
                    (let* ((rec (extractXMLTag (cdr xml) tag)))
                      (cons (cons (car xml) (car rec)) (cdr rec))))
                (let* ((parsed (list (cadr xml) (caddr xml))))
                  (if (equal parsed (coerce tag 'list))
                      (extractXMLTag (cddddr xml) tag)
                      (let* ((rec (extractXMLTag (cdr xml) tag)))
                        (cons (cons (car xml) (car rec)) (cdr rec))))))
            (if (endp xml)
                (cons nil nil)
                (let* ((rec (extractXMLTag (cdr xml) tag)))
                  (if (or (equal (car xml) #\Newline) (equal (car xml) #\Tab))
                      (cons (car rec) (cdr rec))
                      (cons (cons (car xml) (car rec)) (cdr rec))))))
            (extractXMLTag (coerce xml 'list) tag)))
  
  (export Ixml-reader))

(link Mxml-reader
      (import)
      (export Ixml-reader)
      (Mxml-reader-private))