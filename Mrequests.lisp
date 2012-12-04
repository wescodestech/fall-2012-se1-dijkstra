;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis 
  Mrequests.lisp
  Request module that reads and parses the request file.

  Usage: (getRequests filename)

  Date: December 6th, 2012
  Team Dijkstra
|#
(interface Irequests
  (sig getRequests (file)))

(require "Ixml-reader.lisp")
(require "Ilex-ops.lisp")

(module Mrequests-public
  (in-package "ACL2")
  (import Ixml-reader)
  (import Ilex-ops)
  
  (include-book "list-utilities" :dir :teachpacks)
  (include-book "io-utilities" :dir :teachpacks)
  
  ; (trim string)
  ; Removes all blanks and newline characters in the string
  (defun trim (str)
    (let* ((split (packets-set '(#\return #\newline #\tab #\space) 
                               (str->chrs str))))
      (chrs->str (concat split))))
  
  ; (extractTKlist xml)
  ; Returns: (TK1 TK2 ... TKn)
  (defun extractTKlist (xml)
    (if (equal nil xml)
        nil
        (let* ((tkxml (extractXMLTag xml "tk"))
               (tk (coerce (car tkxml) 'string)))
          (cons tk (extractTKlist (cdr tkxml))))))
  
  ; (extractRequests xml)
  ; Given a string of xml characters, extracts a list of requests.
  (defun extractRequests (xml)
    (if (equal nil xml)
        nil
        (let* ((request (extractXMLTag xml "ar"))
               (ar (coerce (car request) 'string))
               (sdxml (extractXMLTag ar "sd"))
               (sd (coerce (car sdxml) 'string))
               (edxml (extractXMLTag (cdr sdxml) "ed"))
               (ed (coerce (car edxml) 'string))
               (tks (extractTKlist (cdr edxml))))
          (cons (list sd ed tks) (extractRequests (cdr request))))))
  
  ; Main function to run to open a file and parse the requests into
  ; ((SD ED (TK1 TK2 ... TKn))1 ... (SD ED (TK1 TK2 ... TKn))n)
  (defun getRequests (file)
    (mv-let (input-text error-open state)
            (file->string file state)
            (extractRequests (trim input-text))))
  
  (export Irequests))

(link Mrequests
      (import Ixml-reader Ilex-ops)
      (export Irequests)
      (Mrequests-public))