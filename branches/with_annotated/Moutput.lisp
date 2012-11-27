;; The first four lines of this file were added by Dracula.
;; They tell DrScheme that this is a Dracula Modular ACL2 program.
;; Leave these lines unchanged so that DrScheme can properly load this file.
#reader(planet "reader.rkt" ("cce" "dracula.plt") "modular" "lang")
#|This line is 75 characters long.---------------------------------------|#
#|tImpl: Stock History Analysis
  Moutput.lisp
  Outputs the data as an HTML file.
  Date: December 6th, 2012
  Team Dijkstra
|#
(interface IOutput
  (sig outputStockData (x))
)
(module MOutput
  (in-package "ACL2")
  (include-book "io-utilities" :dir :teachpacks)
  (include-book "list-utilities" :dir :teachpacks)

  ; take the sum of a list
  (defun sum (xs)
    (if (consp (cdr xs)) 
        (+ (car xs) (sum (cdr xs)))
        (car xs)
    )
  )
  
  ; take the average of a list 
  (defun avg (xs)
    (/ (sum xs) (len xs))
  )
  
  ; add to every element in a list
  (defun vector+scalar (xs x)
    (if (consp (cdr xs))
      (cons (+ x (car xs)) (vector+scalar (cdr xs) x)) 
      (list (+ x (car xs)))
    )
  )
  
  ; combine two lists by multiplication of their elements
  (defun vector*vector (xs ys)
    (if (consp (cdr xs))
        (cons (* (car xs) (car ys)) (vector*vector (cdr xs) (cdr ys)) )
        (list (* (car ys) (car xs)))
    )
  )

  ; take the differences between each element and the average of the list
  (defun differences (xs)
    (if (consp (cdr xs))
      (cons (-(car xs) (avg xs)) (differences (cdr xs))) 
      (-(car xs) (avg xs))
    )
  )

  ; take the variance of the list
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
  
  ; get y for ax + b
  (defun getY (x a b)
    (+(* a x) b)
  )
  
  ; get a set of ys for each x in xs
  (defun getYs (xs a b)
    (if (consp (cdr xs))
        (cons (getY(car xs) a b) (getYs (cdr xs) a b)) 
        (list (getY (car xs) a b))
    )
  )

  (defun googleDate (date)
    (let* ((yd (break-at-nth 4 (str->chrs date))))
      (let* ((md (break-at-nth 2 (second yd))))
        (let* ((y (car yd)) (m (car md)) (d (second md)))
          (concatenate 'string "new Date(" (chrs->str y) ", " (chrs->str m) ", " (chrs->str d) ")")
        )
      )
    )
  ) 

  ; combine three lists of size k into a JSON array of k triples
  (defun JSONcombine (xs ys zs)
    (if (consp (cdr xs)) 
        (concatenate 'string (concatenate 'string "[" (googleDate (rat->str(car xs) 2)) "," (rat->str(car ys) 2) "," (rat->str (car zs) 2) "]" ) "," (JSONcombine (cdr xs) (cdr ys) (cdr zs))  ) 
        (concatenate 'string "[" (googleDate (rat->str (car xs) 2)) "," (rat->str (car ys) 2) "," (rat->str (car zs) 2) "]" ) 
    )
  )
  
  ; given a list of tuples, take all first elements
  (defun firsts (tups)
    (if (consp (cdr tups))
        (cons (first (car tups)) (firsts (cdr tups)))
        (list (first (car tups)))
    )
  )
  
  ; given a list of tuples, take all second elements
  (defun seconds (tups)
    (if (consp (cdr tups))
        (cons (second (car tups)) (seconds (cdr tups)))
        (list (second (car tups)))
    )
  )
  
  ; perform linear regression on the sum as a function of the date,
  ; given a list of dates and a list of sums. Output a JSON array of
  ; triples of the format [date,sum, linear approximation of sum]
  (defun generateData (dates sums)
    ; do the linear regression to get a and b
    ; use algebra to get a list of approximate values at every date
    ; format as a JSON array
    (let* ((xAvg (avg dates)) (yAvg (avg sums)))
      (let* ((b (/ (r dates sums) (xVar dates))))
        (let* ((a (- yAvg (* b xAvg))))
         (JSONcombine dates sums (getYs dates b a))
        )
      )
    )
  )

  ; concatenate the ticker symbols using some delimiter
  (defun tickerString (tickers delim)
    (if (consp (cdr tickers)) 
      (concatenate 'string (car tickers) delim (tickerString (cdr tickers) delim))
      (car tickers)
    )
  )
  
  ; concatenate the first and last dates using some delimiter
  (defun dateString (dates delim)
    (concatenate 'string (rat->str (first dates) 0) delim (rat->str (car(last dates)) 0) )
  )
  
  ; pass the raw data to generateData and wrap the results with HTML
  (defun getHTML (dates sums tickers)
    (concatenate 'string 
      "<html>
         <head>
           <script type='text/javascript' src='http://www.google.com/jsapi'></script>
           <script type='text/javascript'>
             google.load('visualization', '1', {'packages':['annotatedtimeline']});
             google.setOnLoadCallback(drawChart);
             function drawChart() {
               var data = new google.visualization.DataTable();
               data.addColumn('date', 'Date');
               data.addColumn('number', 'Sum Value');
               data.addColumn('number', 'Trend Value');
               data.addRows([" (generateData dates sums) "]);
               var chart = new google.visualization.AnnotatedTimeLine(document.getElementById('chart_div'));
               chart.draw(data, {displayAnnotations: true});
             }
           </script>
         </head>
         <body>
           <h1>Linear Regression Analysis</h1><h2>" (tickerString tickers "<br>") "</h2>
           <div id='chart_div' style='width: 800px; height: 400px;'></div>
         </body>
       </html>"
    )
  )

  ; pass the raw data to generateHTML and write the results to a file
  (defun writeToFile (dates sums tickers f-out state)
    (mv-let (error-close state)
      (string-list->file 
        f-out
        (list (getHTML dates sums tickers))
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

  ; generate a unique file name based on the data
  (defun getFileName (tickers dates)
    (concatenate 'string (dateString dates "_") "_" (tickerString tickers "_") ".html")
  )
  
  ; take the raw data and output HTML files
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
            (writeToFile dates sums tickers (getFileName tickers dates) state)
          )
        )
    )
  )
  (export IOutput)
)