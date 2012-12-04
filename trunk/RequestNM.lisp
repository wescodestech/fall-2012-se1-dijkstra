(include-book "StocksNM")  
(include-book "lexopsnm")
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
               (sd (str->rat (coerce (car sdxml) 'string)))
               (edxml (extractXMLTag (cdr sdxml) "ed"))
               (ed (str->rat (coerce (car edxml) 'string)))
               (tks (extractTKlist (cdr edxml))))
          (cons (list sd ed tks) (extractRequests (cdr request))))))
  
  ; Main function to run to open a file and parse the requests into
  ; ((SD ED (TK1 TK2 ... TKn))1 ... (SD ED (TK1 TK2 ... TKn))n)
  (defun getRequests (file)
    (mv-let (input-text error-open state)
            (file->string file state)
            (extractRequests (trim input-text))))