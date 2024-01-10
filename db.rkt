;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname db) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



; ======================
; data definitions


(define-struct database [header content])
; a Database is a [Header Content]
#;
(define (fn-on-db db)
  (... (fn-on-header (database-header db))
       ... (fn-on-content (database-content db))))


; a Header is a [ListOf Schema]
#;
(define (fn-on-header hd)
  (cons (fn-on-schema (first hd)) (fn-on-header (rest hd))))


(define-struct schema [name pred])
; a Schema is a [String [Any -> Boolean]]
#;
(define (fn-on-schema scm)
  (... (fn-on-string (schema-name scm))
       ... (fn-on-func (schema-pred scm))))


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

(define data-wallop '(("Alice" 35 #t) ("Bob" 25 #f) ("Carol" 30 #t)
                                      ("Dave" 32 #f) ("Pete" "27" #f)
                                      ("Pete" 27 #f) ("Jete" 49 #t)))


; ====================
; functions


(define (add-content nucontent db)
  ; content Database -> Database
  ; adds a whole chunk of content to a database
  (local (
          (define preds (map (lambda (p) (schema-pred p)) (database-header db)))
          (define (correct-and-unique? row)
            (and
             (= (length row) (length preds))
             (andmap (lambda (p el) (p el)) preds row)
             (not (ormap (lambda (db-row) (equal? row db-row))
                          (database-content db)))))
          (define great-filtered
            (filter (lambda (row) (correct-and-unique? row)) nucontent)))
    ; - IN -
    (make-database (database-header db)
                   (append (database-content db) great-filtered))))


(define (delete-column db col)
  ; Database String -> Database
  ; remove column named col
  (local (
          (define i (get-column-index (database-header db) col))
          (define (eliminate ls n)
            ; [ListOf X] N -> [ListOf X]
            ; strikes the ith element from a list
            (cond
              [(empty? ls) #f]
              [(= 0 n) (rest ls)]
              [else (cons (first ls) (eliminate (rest ls) (sub1 n)))])))
    ; - IN -
    (make-database (eliminate (database-header db) i)
                   (map (lambda (r) (eliminate r i)) (database-content db)))))
          

(define (filter-row db col pred)
  ; Database String [X -> Boolean] -> Database
  ; remove rows that fail to meet certain conditions
  (local (
          (define i (get-column-index (database-header db) col))
          (define (selektor row n)
            ; [ListOf X] N -> [ListOf X]
            ; selects the ith element from a list
            (cond
              [(empty? row) #f]
              [(= 0 n) (first row)]
              [else (selektor (rest row) (sub1 n))])))
    ; - IN -
    (make-database (database-header db)
                   (filter (lambda (r) (pred (selektor r i))) (database-content db)))))


(define (get-column-index head col)
  ; Header N -> N
  ; returns the index of column called col
  (local (
          (define (index ls n)
            (cond
              [(empty? ls) #f]
              [(string=? (schema-name (first ls)) col) n]
              [else (index (rest ls) (add1 n))])))
    ; - IN -
    (index head 0)))


(define (true? b)
  ; Boolean -> Boolean
  ; returns true if #true
  (not (false? b)))



; ======================
; checks

(define name (make-schema "Name" string?))
(define age (make-schema "Age" number?))
(define present (make-schema "Present" boolean?))
(define attendance0 (make-database `(,name ,age ,present) '()))
(define valid-content1 '(("Pete" 27 #f)))
(define invalid-content1 '(("Pete" "27" #f)))
(define attendance1 (make-database `(,name ,age ,present) valid-content1))
(define valid-content2 '(("Jete" 49 #t)))
(define valid-content3 (append valid-content1 valid-content2))
(define invalid-content2 (append invalid-content1 valid-content2))
(check-expect (add-content valid-content1 attendance0)
              (make-database (database-header attendance0) valid-content1))
(check-expect (add-content invalid-content1 attendance0) attendance0)
(check-expect (add-content valid-content2 attendance1)
              (make-database (database-header attendance1)
                             (append (database-content attendance1) valid-content2)))
(check-expect (add-content invalid-content1 attendance1) attendance1)
(check-expect (add-content valid-content3 attendance0)
              (make-database (database-header attendance1) valid-content3))
(check-expect (add-content valid-content1 attendance1) attendance1)
(check-expect (add-content invalid-content2 attendance0)
              (make-database (database-header attendance1) valid-content2))
(check-expect (delete-column attendance1 "Age")
              (make-database `(,name ,present) '(("Pete" #f))))
(check-expect (filter-row attendance1 "Present" false?)
              (make-database `(,name ,age ,present) '(("Pete" 27 #f))))
(check-expect (filter-row attendance1 "Present" true?)
              (make-database `(,name ,age ,present) '()))


; ======================
; action!

(add-content data-wallop (add-content data-wallop attendance0))

(delete-column (add-content data-wallop attendance0) "Name")

(filter-row (add-content data-wallop attendance0) "Present" true?)

(filter-row (add-content data-wallop attendance0) "Age" (lambda (a) (> a 30)))