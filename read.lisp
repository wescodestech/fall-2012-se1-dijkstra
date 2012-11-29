(include-book "list-utilities" :dir :teachpacks)
(include-book "io-utilities" :dir :teachpacks)
(set-state-ok t)

(defun rd (state)
  (mv-let (str error state) (file->string "hist.txt" state)
     (if error
         (mv error state)
         (mv (chrs->str (take 50 (str->chrs str))) state))))
(defun rd-scan (state)
  (mv-let (str error state) (file->string "hist.txt" state)
     (if error
         (mv error state)
         (mv (last (str->chrs str)) state))))
(defun rd-rev-count (state)
  (mv-let (str error state) (file->string "hist.txt" state)
     (if error
         (mv error state)
         (mv (len (reverse (str->chrs str))) state))))
(defun rd-rev (state)
  (mv-let (str error state) (file->string "hist.txt" state)
     (if error
         (mv error state)
         (mv (chrs->str (reverse (take 50 (reverse (str->chrs str))))) state))))