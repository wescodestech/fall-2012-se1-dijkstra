(include-book "io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)

; (extractXMLTag xml tag)
; Extracts the contents of an XML tags (all the data that is contained between
; the tags)
; xml - the string that contains the RAW XML data
; tag - the tag that will be extracted from the XML.
;
; The data between the opening <TAG> and closing </TAG> is returned as a char list.
; The data that occurs after the </TAG> is located, is returned as a string unparsed.
; The return format for this function is (parsed_char_array . the_rest_as_string)
(defun extractXMLTag (xml tag)
  (let* ((xmlCharacters (coerce xml 'list)))
    (if (equal (car xmlCharacters) #\<)
        (if (equal (cadr xmlCharacters) #\/)
            (if (equal (coerce (list (caddr xmlCharacters) (cadddr xmlCharacters)) 'string) tag)
                (let* ((therest (cdr (cddddr xmlCharacters))))
                  (if (equal nil therest)
                      (cons nil nil)
                      (cons nil (coerce therest 'string))))
                (let* ((rec (extractXMLTag (coerce (cdr xmlCharacters) 'string) tag)))
                      (cons (cons (car xmlCharacters) (car rec)) (cdr rec))))
                
            ; Acquire the next two characters to determine the tag
            (let* ((parsed (coerce (list (cadr xmlCharacters) (caddr xmlCharacters)) 'string)))
              (if (equal parsed tag)
                  (extractXMLTag (coerce (cddddr xmlCharacters) 'string) tag)
                  (let* ((rec (extractXMLTag (coerce (cdr xmlCharacters) 'string) tag)))
                    (cons (cons (car xmlCharacters) (car rec)) (cdr rec))))))
        
        (if (endp xmlCharacters)
            (cons nil nil); Nothing to see here!
            (let* ((rec (extractXMLTag (coerce (cdr xmlCharacters) 'string) tag)))
              (cons (cons (car xmlCharacters) (car rec)) (cdr rec)))))))

; (extractStockRecords xml)
; Extracts the data encapsulated by the stock records and returns the string of those values.
(defun extractStockRecords (xml)
  (if (equal nil xml)
      nil
      (let* ((record (extractXMLTag xml "sr")))
        (cons (coerce (car record) 'string) (extractStockRecords(cdr record))))))

(defun read-data (file state)
  (mv-let (text error state) (file->string file state)
          (if error
              (mv error state)
              (mv (len (str->chrs text)) state))))

















