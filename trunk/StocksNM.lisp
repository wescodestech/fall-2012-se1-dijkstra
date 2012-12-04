(include-book "avl-rational-keys" :dir :teachpacks)
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
            (let* ((rec (extractXMLTag 
                         (coerce (cdr xmlCharacters) 'string) tag)))
              (if (or (equal (car xmlCharacters) #\Newline)
                    (equal (car xmlCharacters) #\Tab))
                  ; Ignore the tab/newline
                  (cons (car rec) (cdr rec))
                  (cons (cons (car xmlCharacters) (car rec)) 
                        (cdr rec))))))))
  
  ; (populateTree parent child)
  ; This function will insert data into the tree where it can locate a
  ; key-value pair and correctly construct a subtree for that node, given
  ; the data adheres to the specific structure (td (tk op)) for each
  ; child value inserted.
  ;
  ; ** It is important to note that the TK value will require a hashing
  ;    mechanic.  (See hash-string function)
  ;
  ; parent - The tree upon which the insertion will occur.
  ; child  - The data structure that will be inserted into the tree in 
  ;          the form (td (tk op)) where td is the target date, tk is 
  ;          the token a stock represents and op is the opening price.
  (defun populateTree(parent node)
    (if (occurs-in-tree? (car node) parent)
        (let* ((found (avl-retrieve parent (car node))))
          (avl-insert (avl-delete parent (car node)) (car node)
            (cons (cadr found) (list (cons (cadr node) (caddr node))))))
        (avl-insert parent (first node) 
                  (cons (list (cadr node) (caddr node)) nil))))
  
  ; (readStockData xml)
  ; This function will read the XML string for sotck record data and 
  ; parse it as well as insert it into the tree structure that houses
  ; the information for searching.
  ;
  ; xml - The XML string that is read from the hist.txt file.
  (defun readStockData (xml)
    (if (equal nil xml)
        nil
        (let* ((sr (extractXMLTag xml "sr"))
               (tk (extractXMLTag (coerce (car sr) 'string) "tk"))
               (td (extractXMLTag (cdr tk) "td"))
               (hp (extractXMLTag (cdr td) "hp"))
               (lp (extractXMLTag (cdr hp) "lp"))
               (op (extractXMLTag (cdr lp) "op"))
               (cp (extractXMLTag (cdr op) "cp"))
               (tr (extractXMLTag (cdr cp) "tr")))
          (populateTree (readStockData (cdr sr)) 
                        (list (str->rat (coerce (car td) 'string)) 
                              (coerce (car tk) 'string) 
                              (str->rat (coerce (car op) 'string)))))))
  
  ; (getStockData)
  ; Acquires the stock data in tree formate where TD is the key and 
  ; (TK OP) are values housed as a list in the value of the node.
  ;
  ; This function takes no arguments as the hist.txt is a static input
  ; which is updated externally.  "short_hist.txt" is used as a shorter
  ; notation for parsing purposes.
  (defun getStockData () 
    (mv-let (input-text error-open state)
            (file->string "short_hist.txt" state)
            (readStockData input-text)))