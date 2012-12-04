(include-book "io-utilities" :dir :teachpacks)

; (extractXMLTag xml tag)
; Extracts the contents of an XML tags (all the data that is contained 
; between the tags)
;
; xml - the string that contains the RAW XML data
; tag - the tag that will be extracted from the XML.
;
; The data between the opening <TAG> and closing </TAG> is returned as a 
; char list.  The data that occurs after the </TAG> is located, is 
; returned as a string unparsed. The return format for this function is 
; (parsed_char_array . the_rest_as_string)
(defun extractXMLTag (xml tag)
  ; Covert the XML string into a character list
  (let* ((xmlCharacters (coerce xml 'list)))
    ; See if we are working with a tag
    (if (equal (car xmlCharacters) #\<)
        ; Is it a closing tag?
        (if (equal (cadr xmlCharacters) #\/)
            ; Is it the closing tag of our tag we are looking for?
            (if (equal (coerce (list (caddr xmlCharacters) 
                                     (cadddr xmlCharacters)) 'string) tag)
                ; Ingore the rest of the tag
                (let* ((therest (cdr (cddddr xmlCharacters))))
                  ; Done processing XML!
                  (if (equal nil therest)
                      ; No more XML to process, return nil
                      (cons nil nil)
                      ; More XML let, return it as a string
                      (cons nil (coerce therest 'string))))
                ; Keep processing the record
                (let* ((rec (extractXMLTag 
                             (coerce (cdr xmlCharacters) 'string) tag)))
                  ; Add to the record char array cons the rest to process
                      (cons (cons (car xmlCharacters) (car rec)) 
                            (cdr rec))))
                
            ; Acquire the next two characters to determine the tag
            (let* ((parsed (coerce (list (cadr xmlCharacters) 
                                         (caddr xmlCharacters)) 'string)))
              ; We found our opening tag!
              (if (equal parsed tag)
                  (extractXMLTag (coerce (cddddr xmlCharacters) 'string) 
                                 tag)
                  (let* ((rec (extractXMLTag (coerce (cdr xmlCharacters) 
                                                     'string) tag)))
                    (cons (cons (car xmlCharacters) (car rec)) 
                          (cdr rec))))))
        
        (if (endp xmlCharacters)
            (cons nil nil); Nothing to see here!
            (let* ((rec (extractXMLTag (coerce (cdr xmlCharacters) 'string) tag)))
              (if (or (equal (car xmlCharacters) #\Newline)
                    (equal (car xmlCharacters) #\Tab))
                  ; Ignore the tab/newline
                  (cons (car rec) (cdr rec))
                  (cons (cons (car xmlCharacters) (car rec)) (cdr rec)))))))) 