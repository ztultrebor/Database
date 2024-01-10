;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname db) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



; ======================
; data definitions


(define-struct database [header data])
; a Database is a [[ListOf Schema] [ListOf Row]]
#;
(define (fn-on-db db)
  (... (fn-on-sheader (database-header db))
       ... (fn-on-data (database-data db))))


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


; a Data is a [ListOf Row]
#;
(define (fn-on-data d)
  (cons (fn-on-row (first d)) (fn-on-data (rest d))))


; a Row is a [ListOf Any]
#;
(define (fn-on-row r)
  (cons (fn-on-any (first r)) (fn-on-row (rest r))))

; =====================
; constants

(define name (make-schema "Name" string?))
(define age (make-schema "Age" number?))
(define present (make-schema "Present" boolean?))
(define attendance0 (make-database `(,name ,age ,present) '()))
(define valid-data1 '(("Pete" 27 #f)))
(define invalid-data1 '(("Pete" "27" #f)))
(define attendance1 (make-database `(,name ,age ,present) valid-data1))
(define valid-data2 '(("Jete" 49 #t)))
(define valid-data3 (append valid-data1 valid-data2))
(define invalid-data2 (append invalid-data1 valid-data2))



; ====================
; functions


(define (add-data data db)
  ; Data Database -> Database
  ; adds a whole chunk of data to a database
  (local (
          (define preds (map (lambda (p) (schema-pred p)) (database-header db)))
          (define (correct? row)
            (and
             (= (length row) (length preds))
             (andmap (lambda (p el) (p el)) preds row)))
          (define great-filtered (filter (lambda (row) (correct? row)) data)))
    ; - IN -
    (make-database (database-header db)
                   (append (database-data db) great-filtered))))



; ======================
; checks


(check-expect (add-data valid-data1 attendance0)
              (make-database (database-header attendance0) valid-data1))
(check-expect (add-data invalid-data1 attendance0) attendance0)
(check-expect (add-data valid-data2 attendance1)
              (make-database (database-header attendance1)
                             (append (database-data attendance1) valid-data2)))
(check-expect (add-data invalid-data1 attendance1) attendance1)
(check-expect (add-data valid-data3 attendance0)
              (make-database (database-header attendance1) valid-data3))
(check-expect (add-data invalid-data2 attendance0)
              (make-database (database-header attendance1) valid-data2))


; ======================
; action!

attendance0
(add-row valid-data1 attendance0)
(add-row valid-data2 (add-row valid-data1 attendance0))