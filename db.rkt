;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname db) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



; ======================
; data definitions


(define-struct database [schema content])
; a Database is a [Schema Content]
#;
(define (fn-on-db db)
  (... (fn-on-schema (database-schema db))
       ... (fn-on-content (database-content db))))


; a Schema is a [ListOf Spec]
#;
(define (fn-on-schema hd)
  (cons (fn-on-spec (first hd)) (fn-on-schema (rest hd))))


(define-struct spec [name pred])
; a Spec is a [String [Any -> Boolean]]
#;
(define (fn-on-spec scm)
  (... (fn-on-string (spec-name scm))
       ... (fn-on-func (spec-pred scm))))


; a Content is a [ListOf Row]
#;
(define (fn-on-content d)
  (cons (fn-on-row (first d)) (fn-on-content (rest d))))


; a Row is a [ListOf Any]
#;
(define (fn-on-row r)
  (cons (fn-on-any (first r)) (fn-on-row (rest r))))



; =====================
; constants


; ====================
; functions

  
(define (check-integrity db)
  ; content Database -> Database
  ; ensures the data integrity of the database
  (local (
          (define schema (database-schema db))
          (define content (database-content db))
          (define preds (map (lambda (spc) (spec-pred spc)) schema))
          (define l-preds (length preds))
          (define (correct-and-unique? row otherrows)
            ; Row -> Boolean
            ; does the new row have the correct structure and composition
            ; according to the schema, and would it be unique?
            (and
             (= (length row) l-preds)
             (andmap (lambda (p el) (p el)) preds row)
             (not (ormap (lambda (db-row) (equal? row db-row)) otherrows))))
          (define (check-row rows)
            ; Content -> Content
            ; deletes improper rows from database
            (cond
              [(empty? rows) '()]
              [(correct-and-unique? (first rows) (rest rows))
               (cons (first rows) (check-row (rest rows)))]
              [else (check-row (rest rows))])))
    ; - IN -
    (make-database schema (check-row content))))


(define (delete-column db col)
  ; Database String -> Database
  ; remove column named col
  (local (
          (define sch (database-schema db))
          (define i (get-column-index sch col))
          (define (strike ls n)
            ; [ListOf X] N -> [ListOf X]
            ; strikes the ith element from a list
            (cond
              [(empty? ls) #f]
              [(= 0 n) (rest ls)]
              [else (cons (first ls) (strike (rest ls) (sub1 n)))])))
    ; - IN -
    (make-database (strike sch i)
                   (map (lambda (r) (strike r i)) (database-content db)))))
          

(define (filter-content db col pred)
  ; Database String [X -> Boolean] -> Database
  ; keep only rows that meet defined predicate conditions
  (local (
          (define sch (database-schema db))
          (define i (get-column-index sch col))
          (define (selektor row n)
            ; [ListOf X] N -> [ListOf X]
            ; selects the ith element from a list
            (cond
              [(empty? row) #f]
              [(= 0 n) (first row)]
              [else (selektor (rest row) (sub1 n))])))
    ; - IN -
    (make-database sch (filter (lambda (r) (pred (selektor r i)))
                               (database-content db)))))


(define (get-column-index head col)
  ; Schema N -> N
  ; returns the index of column called col
  (local (
          (define (index ls n)
            (cond
              [(empty? ls) #f]
              [(string=? (spec-name (first ls)) col) n]
              [else (index (rest ls) (add1 n))])))
    ; - IN -
    (index head 0)))


(define (true? b)
  ; Boolean -> Boolean
  ; returns true if #true
  (not (false? b)))



; ======================
; checks


(define name (make-spec "Name" string?))
(define age (make-spec "Age" number?))
(define living (make-spec "Living" boolean?))
(define schema `(,name ,age ,living))
(define deleted-schema `(,name ,living))
(define content  '(("Job" 792 #f) ("Esai" 231 #t) ("Hemat" 666 #f)
                                  ("Babil" 4 #t)))
(define invalid-content '(("Job" 792 #f) ("Esai" 231 #t) ("Hemat" 666 #f)
                                         ("Babil" 4 #t) ("Akatesh" "792" #f)))
(define mismatch-content '(("Job" 792 #f) ("Esai" 231 #t) ("Hemat" 666 #f)
                                          ("Babil" 4 #t)
                                          ("Akatesh" 792 #f "wrong")))
(define dupli-content '(("Hemat" 666 #f) ("Babil" 4 #t) ("Job" 792 #f)
                                         ("Esai" 231 #t) ("Hemat" 666 #f)
                                         ("Babil" 4 #t)))
(define deleted-content  '(("Job" #f) ("Esai" #t) ("Hemat" #f) ("Babil" #t)))
(define filtered-content  '(("Job" 792 #f) ("Hemat" 666 #f)))
(check-expect (check-integrity (make-database schema content))
              (make-database schema content))
(check-expect (check-integrity (make-database schema invalid-content))
              (make-database schema content))
(check-expect (check-integrity (make-database schema mismatch-content))
              (make-database schema content))
(check-expect (check-integrity (make-database schema dupli-content))
              (make-database schema content))
(check-expect (delete-column (make-database schema content) "Age")
              (make-database deleted-schema deleted-content))
(check-expect (filter-content (make-database schema content) "Age"
                              (lambda (el) (> el 500)))
              (make-database schema filtered-content))




; ======================
; action!


