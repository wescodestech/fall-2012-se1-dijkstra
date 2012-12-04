(include-book "StocksNM") 
(include-book "RequestNM")
(include-book "output")
(include-book "avl-rational-keys" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)
  
  
  ; (isPresent tks sr)
  ; Verifies if the token for the stock record is present in the list of
  ; tks are are used for search.
  ;
  ; tks - a list of tokens to verify agains
  ; sr  - the stock record that houses information for comparison
  (defun isPresent (tks sr)
    (if (equal nil (car tks))
        ; No Token is present for the SR
        nil
        ; Might be a token present
        (if (equal (car tks) sr)
            ; Token does occur
            t
            ; Token does not occur, try next
            (isPresent (cdr tks) sr))))
  
  ; (calcValue tks srs total_value)
  ; Calculates a stocks value, given is occurs on that target date,
  ; 
  ; tks - the tokens that will be used to verify that a SR occurs on that
  ;       date.
  ; srs - the stock records that are present on a target date.
  ; total_value - The total value of the stock records that are verified 
  ;       against tks for the target date.
  (defun calcValue (tks srs total_value)
    (if (or (equal nil (car tks)) (equal nil (car srs)))
        total_value
        (if (isPresent tks (caar srs))
            (calcValue tks (cdr srs) (+ total_value (cadar srs)))
            (calcValue tks (cdr srs) total_value))))
        
  ; (getStockValues flattened_tree request)
  ; Acquires stock values from a request
  (defun getStockValues (flattened_tree request)
    (if (equal nil (car flattened_tree))
        nil
        (let* ((current (car flattened_tree))
               (date (car current))
               (stocks (cdr current))
               (sd (car request))
               (ed (cadr request))
               (tks (caddr request)))
          (if (and (<= sd date) (>= ed date))
              (cons (list date (calcValue tks stocks 0)) 
                    (getStockValues (cdr flattened_tree) request))
              (getStockValues (cdr flattened_tree) request)))))
  
  
  (defun mapData (stocks requests)
    (if (equal nil (car requests))
        nil
        (let* ((req (car requests))
               (lin (avl-flatten stocks)))
          (outputStockData (cons (list (getStockValues lin req) (caddr req)) 
                (mapData stocks (cdr requests)))))))
  