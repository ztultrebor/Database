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
  ; content Database -> Boolean
  ; checks the data integrity of the database without correction
  (local (
          (define preds (map spec-pred (database-schema db)))
          (define l-preds (length preds)))
    ; - IN -
    (andmap
     (lambda (row)
       (and
        (= (length row) l-preds)
        (andmap (lambda (prd elem) (prd elem)) preds row)))
     (database-content db))))


(define (join db1 db2)
  ; Database Database -> Database
  ; joins two databases together, providede the have the same structure
  (local (
          (define schema1 (database-schema db1))
          (define schema2 (database-schema db2))
          (define content1 (database-content db1))
          (define content2 (database-content db2)))
    ; - IN -
    (if (equal? schema1 schema2)
        (make-database
         schema1
         (append content1
                 (filter (lambda (r) (not (member? r content1))) content2)))
        (error "these databases are incompatible"))))


(define (select-columns db locols)
  ; Database [ListOf String] -> Database
  ; remove column named col
  (local (
          (define sch (database-schema db))
          (define (list-index el lst n)
            ; X [ListOf X] N -> N
            (cond
              [(empty? lst) #f]
              [(string=? el (first lst)) n]
              [else (list-index el (rest lst) (add1 n))]))
          (define mask
            (map (lambda (col) (list-index col (map spec-name sch) 0)) locols))
          (define (reorganize lst)
            ; [ListOf X] -> [ListOf X]
            ; filter a list according to selected columns
            (foldr
             (lambda (i suffix) (cons (list-ref lst i) suffix)) '() mask)))
    ; - IN -
    (make-database (reorganize sch) (map reorganize (database-content db)))))


(define (filter-content db col pred)
  ; Database String [X -> Boolean] -> Database
  ; keep only rows that meet defined predicate conditions
  (local (
          (define sch (database-schema db))
          (define mask
            (map (lambda (n) (string=? n col)) (map spec-name sch))))
    ; - IN -
    (make-database
     sch
     (filter (lambda (row)
               (andmap (lambda (elem tf)
                         (if tf (pred elem) #t))
                       row mask))
             (database-content db)))))



; ======================
; checks


(define name (make-spec "Name" string?))
(define age (make-spec "Age" number?))
(define living (make-spec "Living" boolean?))
(define schema `(,name ,age ,living))
(define deleted-schema `(,name ,living))
(define reordered-schema `(,name ,living ,age))
(define content  '(("Job" 792 #f) ("Esai" 231 #t) ("Hemat" 666 #f)
                                  ("Babil" 4 #t)))
(define invalid-content '(("Job" 792 #f) ("Esai" 231 #t) ("Hemat" 666 #f)
                                         ("Babil" 4 #t) ("Akatesh" "792" #f)))
(define mismatch-content '(("Job" 792 #f) ("Esai" 231 #t) ("Hemat" 666 #f)
                                          ("Babil" 4 #t)
                                          ("Akatesh" 792 #f "wrong")))
(define deleted-content  '(("Job" #f) ("Esai" #t) ("Hemat" #f) ("Babil" #t)))
(define filtered-content  '(("Job" 792 #f) ("Hemat" 666 #f)))
(define content2 '(("Job" 792 #f) ("Hemat" 666 #f) ("Akatesh" 792 #f)))
(define joined-content '(("Job" 792 #f) ("Esai" 231 #t) ("Hemat" 666 #f)
                                        ("Babil" 4 #t) ("Akatesh" 792 #f)))
(define reordered-content  '(("Job" #f 792) ("Esai" #t 231) ("Hemat" #f 666)
                                  ("Babil" #t 4)))
(check-expect (check-integrity (make-database schema content)) #t)
(check-expect (check-integrity (make-database schema invalid-content)) #f)
(check-expect (check-integrity (make-database schema mismatch-content)) #f)
(check-expect (join (make-database schema content)
                    (make-database schema content2))
              (make-database schema joined-content))
(check-error (join (make-database schema content)
                   (make-database deleted-schema deleted-content))
             "these databases are incompatible")
(check-expect (select-columns (make-database schema content) '("Name" "Living"))
              (make-database deleted-schema deleted-content))
(check-expect (select-columns (make-database schema content)
                              '("Name" "Living" "Age"))
              (make-database reordered-schema reordered-content))
(check-expect (filter-content (make-database schema content) "Age"
                              (lambda (el) (> el 500)))
              (make-database schema filtered-content))



; ======================
; action!


