(in-package "ACL2")
(include-book "io-utilities" :dir :teachpacks)
(include-book "list-utilities" :dir :teachpacks)




; These four functions are required
(defun sum (xs)
  (if (consp (cdr xs)) 
      (+ (car xs) (sum (cdr xs)))
      (car xs)
  )
)
(defun avg (xs)
  (/ (sum xs) (len xs))
)
(defun vector+scalar (xs x)
  (if (consp (cdr xs))
    (cons (+ x (car xs)) (vector+scalar (cdr xs) x)) 
    (list (+ x (car xs)))
  )
)
(defun vector*vector (xs ys)
  (if (consp (cdr xs))
      (cons (* (car xs) (car ys)) (vector*vector (cdr xs) (cdr ys)) )
      (list (* (car ys) (car xs)))
  )
)

; list containing the differences between each 
; single point and the average of a list
(defun differences (xs)
  (if (consp (cdr xs))
    (cons (-(car xs) (avg xs)) (differences (cdr xs)))
    (-(car xs) (avg xs))
  )
)

; Variance
(defun xVar (xs)
  (let* ((ds (differences xs)))
    (let* ((sds (vector*vector ds ds)))
      (avg sds)
    )
  )
)

; r(x,y)
(defun r (xs ys)
  (let* ((xds (differences xs)) (yds (differences ys)))
    (let* ((xyds (vector*vector xds yds)))
      (avg xyds)
    )
  )
)

(defun getY (x a b)
  (+(* a x) b)
)

(defun getYs (xs a b)
  (if (consp (cdr xs))
      (cons (getY(car xs) a b) (getYs (cdr xs) a b)) 
      (list (getY (car xs) a b))
  )
)


(defun combine (xs ys zs)
    (if (consp (cdr xs)) 
        (concatenate 'string (concatenate 'string "[" (rat->str(car xs) 2) "," (rat->str(car ys) 2) "," (rat->str (car zs) 2) "]" ) "," (combine (cdr xs) (cdr ys) (cdr zs))  ) 
        (concatenate 'string "[" (rat->str (car xs) 2) "," (rat->str (car ys) 2) "," (rat->str (car zs) 2) "]" ) 
    )
)


(defun regressionPoints (xs ys)
  (let* ((xAvg (avg xs)) (yAvg (avg ys)))
    (let* ((b (/ (r xs ys) (xVar xs))))
      (let* ((a (- yAvg (* b xAvg))))
       (combine xs ys (getYs xs b a))
      )
    )
  )
)



(defun modify-file (str)
   (list (chrs->str (str->chrs str))))

(defun getHTML (xs ys)
  (concatenate 'string 
               "
<html>
  <head>
    <script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script>
    <script type=\"text/javascript\">
      google.load(\"visualization\", \"1\", {packages:[\"corechart\"]});
      google.setOnLoadCallback(drawChart);
      function drawChart() {
        var data = google.visualization.arrayToDataTable(
               "
               
               
               "[" "['x','y','regression']," (regressionPoints xs ys) "]"

               
               "
);
        var options = {
          title: \"Pi / 4 Using the Monte Carlo method\"
        };

        var chart = new google.visualization.LineChart(document.getElementById(\"chart_div\"));
        chart.draw(data, options);
      }
    </script>
  </head>
  <body>
    <div id=\"chart_div\" style=\"width: 900px; height: 500px;\"></div>
  </body>
</html>
               "
  )
)

(defun rtw (f-in f-out state)
  (mv-let (input-as-string error-open state) (file->string f-in state)
     (if error-open
         (mv error-open state)
         (mv-let (error-close state)
                 (string-list->file f-out
                                    (modify-file (getHTML '(1 2 3 4) '(5 6 4 5)))
                                    state)
            (if error-close
                (mv error-close state)
                (mv (string-append "input file: "
                     (string-append f-in
                      (string-append ", output file: " f-out)))
                    state))))))






;(getHTML '(1 2 3 4) '(5 5 5 5))
(rtw "in.txt" "out.html" state)