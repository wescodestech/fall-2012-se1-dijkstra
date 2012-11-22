#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis
  Moutput.lisp
  Outputs the data as an HTML file.
  Date: December 6th, 2012
  Team Dijkstra
|#

  (in-package "ACL2")
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)

  
  ; private
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
  (defun firsts (tups)
    (if (consp (cdr tups))
        (cons (first (car tups)) (firsts (cdr tups)))
        (list (first (car tups)))
    )
  )
  (defun seconds (tups)
    (if (consp (cdr tups))
        (cons (second (car tups)) (seconds (cdr tups)))
        (list (second (car tups)))
    )
  )

  
  ; defuns
  (defun generateData (dates sums)
    ; do the linear regression to get a and b
    ; then use algebra to get a list of regression values at every date (getYs)
    ; then combine dates sums and regression values into a list of triples as a string
    (let* ((xAvg (avg dates)) (yAvg (avg sums)))
      (let* ((b (/ (r dates sums) (xVar dates))))
        (let* ((a (- yAvg (* b xAvg))))
         (combine dates sums (getYs dates b a))
        )
      )
    )
  )
  (defun tickerNiceString (tickers)
    (if (consp (cdr tickers))
      (concatenate 'string (car tickers) ", " (tickerNiceString (cdr tickers)))
      (car tickers)
    )
  )
  (defun dateNiceString (dates)
    (concatenate 'string (rat->str (first dates) 0) " - " (rat->str (car(last dates)) 0) )
  )
  (defun getHTML (dates sums tickers)
    (concatenate 'string 
      "<html><head>
       <script type=\"text/javascript\" src=\"https://www.google.com/jsapi\"></script>
       <script type=\"text/javascript\">
         google.load(\"visualization\", \"1\", {packages:[\"corechart\"]});
         google.setOnLoadCallback(drawChart);
         function drawChart() {
           var data = google.visualization.arrayToDataTable("
                 "[" "['x','y','regression']," (generateData dates sums) "]"     
           ");
           var options = {
             title: \"Stock Analysis For: " (tickerNiceString tickers) " Between " (dateNiceString dates) "\"
           };

           var chart = new google.visualization.LineChart(document.getElementById(\"chart_div\"));
           chart.draw(data, options);
         }
       </script>
     </head><body>
       <div id=\"chart_div\" style=\"width: 900px; height: 500px;\"></div>
     </body>
   </html>"
    )
  )

(defun modify-file (str)
   (list (chrs->str (str->chrs str))))

  (defun writeToFile (dates sums tickers f-out state)
    (mv-let (error-close state)
      (string-list->file 
        f-out
        (modify-file (getHTML dates sums tickers))
        state
      )
      (if error-close
        (mv error-close state)
        (mv  (string-append ", output file: " f-out)
            state
        )     
      )
    )
  )

  (defun tickerString (tickers)
    (if (consp (cdr tickers)) 
      (concatenate 'string (car tickers) "_" (tickerString (cdr tickers)))
      (car tickers)
    )
  )
  (defun dateString (dates)
    (concatenate 'string (rat->str (first dates) 0) "_" (rat->str (car(last dates)) 0) )
  )
  (defun getName (tickers dates)
    (concatenate 'string (dateString dates) "_" (tickerString tickers) ".html")
  )
  (defun outputStockData (data)
    (if (consp (cdr data)) 

        ; Recursive Case
        ; the below line doesn't really need these consed, it's just
        ; a hack to do something to the car and then something to the
        ; cdr. the values returned are irrelevant.
        (cons (outputStockData (list(car data))) (outputStockData (cdr data))) 

        ; Base Case - called once for every recursive case
        ; first take the car, which is the dates and sums
        ; then take the cdr, which is the ticker symbols
        (let* ((values (car (car data))) (tickers (car (cdr (car data)))) )
          ;then change values into a list of dates and a list of sums (xs and ys)
          (let* ((dates (firsts values)) (sums (seconds values)) )
            ;get the linear regression data (in JSON form)
            (writeToFile dates sums tickers (getName tickers dates) state)
          )
        )
    )
  )

  ; output these two data items for testing
  (outputStockData (list ( list (list '(20121115 10) '(20121116 15) '(20121117 50) '(20121118 1)) (list "GOOG" "FB" "MSFT")) ( list (list '(20121112 20) '(20121113 70) '(20121114 40)) (list "AMZN" "ZNGA")))  )
  