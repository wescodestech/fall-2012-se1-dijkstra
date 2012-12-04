(include-book "list-utilities" :dir :teachpacks)
  
   ; (mux (x0 x1 x2...) (y0 y1 y2...)) = (x0 y0 x1 y1 x2 y2...)
  (defun mux (xs ys)
    (if (endp xs)
        ys
        (if (endp ys)
            xs
            (append (list (car xs) (car ys))
                    (mux (cdr xs) (cdr ys))))))
  
  ; (dmx (x0 y0 x1 y1 x2 y2 ...)) = ((x0 x1 x2...) (y0 y1 y2...))
  (defun dmx (xys)
    (if (endp xys)
        (list nil nil)
        (let* ((x (car xys))
               (ysxs (dmx (cdr xys)))
               (ys (car ysxs))
               (xs (cadr ysxs)))
          (list (cons x xs) ys))))
  
  ;Concat function from lecture
  ;concats a list of lists into one list
  (defun concat (xss)
    (if (consp xss)
        (append (car xss) (concat (cdr xss)))
        xss))
  
  ; (blocks n xs) = (b1 b2 .. bk)
  ; where (concat (blocks n xs)) = xs
  ; and (len bj) = n
  (defun blocks (n xs)
    (if (and (posp n) (consp xs))
        (let* ((s (break-at-nth n xs))
               (fr (car s))
               (bk (cadr s)))
          (cons fr (blocks n bk)))
        nil))
  
  ; (split-at-delimiter ds xs)
  ;  Delivers a list with two elements.  The first element is the longest
  ;    prefix of xs containing no elements of ds.  The second element is
  ;    the rest of xs.
  ;  Note: if xs is nil, this returns (nil nil).
  ;  Note: is ds is nil, this returns (xs nil).
  (defun split-at-delimiter (ds xs) 
    (if (endp xs) 
        (list nil nil)
        (if (member-equal (car xs) ds) 
            (list nil xs)
            (let* ((split (split-at-delimiter ds (cdr xs))))
              (list (cons (car xs) (car split)) 
                    (cadr split))))))
  
  ; (span ps xs)
  ;  Delivers a list with two elements.  The first element is the longest
  ;    prefix of xs containing only elements of ps.  The second element is
  ;    the rest of xs.
  ;  Note: if xs is nil, this returns (nil nil).
  ;  Note: if ps is nil, this returns (nil xs).
  (defun span (ps xs)
    (if (endp xs) 
        (list nil nil)
        (if (member-equal (car xs) ps)
            (let* ((span-xs (span ps (cdr xs))))
              (list (cons (car xs) (car span-xs)) 
                    (cadr span-xs)))
            (list nil xs))))
  
  ; (splitoff-prefix ps xs)
  ;  Delivers a list with three elements.  The first element is the longest
  ;    shared prefix of ps and xs.  The second element is the rest of ps.
  ;    The third element is the rest of xs.
  ;  Note: if xs is nil, this returns (nil ps nil).
  ;  Note: if ps is nil, this returns (nil nil xs).
  ;  Note: if the first element of xs and ps are not equal, this returns 
  ;    (nil ps xs).
  (defun splitoff-prefix (ps xs)
    (if (endp ps)
        (list nil nil xs)
        (if (endp xs)
            (list nil ps nil)
            (if (equal (car xs) (car ps))
                (let* ((split (splitoff-prefix (cdr ps) (cdr xs))))
                  (list (cons (car xs) 
                              (car split)) 
                        (cadr split)
                        (caddr split)))
                (list nil ps xs)))))
  
  ; (split-on-token tok xs)
  ;  Delivers a list with three elements.  The first element is the prefix of
  ;    xs before the first contiguous sublist matching tok if tok is found,
  ;    xs otherwise.  The second element is tok if tok is found, nil 
  ;    otherwise.  The third element is the suffix after the first contiguous
  ;    sublist matching tok if tok is found, nil otherwise.
  ;  Note: if tok is nil, this returns (nil nil xs).
  ;  Note: if xs is nil, this returns (nil nil nil).
  (defun split-on-token (tok xs)
    (if (endp xs)
        (list nil nil nil)
        ; split-off-prefix will return a list with a second element of nil
        ; if all of ps is a prefix of xs - i.e. the first n elements of xs
        ; match ps, where n is the length of ps.  In that case, the third
        ; element of that list will be the suffix of xs that needs to be
        ; delivered in the third element of the list that this function
        ; returns.
        (let* ((prefix (splitoff-prefix tok xs))
               (split (split-on-token tok (cdr xs))))
          (if (endp (cadr prefix))
              (list nil tok (caddr prefix))
              (list (cons (car xs) (car split)) 
                    (cadr split)
                    (caddr split))))))