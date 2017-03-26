
;sample input
;(dijkstra-search 5 6  '(3 4 1 2 8 6
;                        6 1 8 2 7 4
;                        5 9 3 9 9 5
;                        8 4 1 3 2 6
;                        3 7 2 8 6 4))

(defn get-top-path [rows, next-column, current-position]
  (if (= current-position 0)                  ;Check for wrapping around the matrix
      (nth next-column (- rows 1))            ;Gets the element in the specified position
    (nth next-column (- current-position 1))
    )
  )

(defn get-middle-path [next-column, current-position]
  (nth next-column current-position)          ;Gets the element in the specified position
  )

(defn get-bottom-path [rows, next-column, current-position]
  (if (= current-position (- rows 1))         ;Check for wrapping around the matrix
      (nth next-column 0)                     ;Gets the element in the specified position
    (nth next-column (+ 1 current-position))
    )
  )

(defn get-rest-matrix [position matrix]
  (if (zero? position)                        ;Checks if the function is in the correct position in the matrix
     matrix                                   ;Returns the matrix if true
    (recur (dec position) (rest matrix))
    )
  )

(defn get-index
  ([value collection]                         ;Function that recurs through a collection to get the position of the
   (get-index value collection 0))            ;Required element

  ([value collection count]
   (if (= value (first collection))
     count

     (recur value (rest collection) (inc count)))
    )
  )

(defn get-column
  ([rows, columns, position, matrix]
   (get-column (dec rows) columns (+ position columns) matrix (cons (first (get-rest-matrix position matrix)) '()))   ;Function that uses the get-rest-matrix function
)                                                                                                                     ;to build a list of variables in the required column

  ([rows, columns, position, matrix, results]
   (if (zero? rows)
     (reverse results)

     (recur (dec rows) columns (+ position columns) matrix (cons (first (get-rest-matrix position matrix)) results)))
    )
  )

(defn get-possible-paths [current-position, position, matrix, rows, columns]
  (let [next-column (get-column rows columns (+ 1 position) matrix)]                                                    ;Helper function that uses the path variable functions
                                                                                                                      ;to build a list of possible routes.
     [(get-top-path rows, next-column, current-position)
       (get-middle-path next-column, current-position)
       (get-bottom-path rows, next-column, current-position)])

  )

(defn dijkstra-search
  ([rows, columns, matrix]
   (let [current-value (apply min (get-column rows columns 0 matrix))                                                 ;Assigns current value and current position using
         current-position (get-index current-value (get-column rows columns 0 matrix))]                               ;helper functions.

     (dijkstra-search rows columns matrix current-position current-value 0 current-value)))

  ([rows, columns, matrix, current-position, current-value, position, results]                                        ;Iterates through each column, and decreases the
   (if (= (+ 1 position) columns)                                                                                     ;row count each time.
     results

     (let [next-value (apply min (get-possible-paths current-position, position, matrix, rows, columns))
           next-position (get-index next-value (get-column rows columns (+ position 1) matrix))]

       (recur rows columns matrix next-position next-value (+ position 1) (+ results next-value)))
     )
    )
  )
