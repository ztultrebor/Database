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
(define valid-row1 '("Pete" 27 #f))
(define invalid-row1 '("Pete" "27" #f))
(define attendance1 (make-database `(,name ,age ,present) '(valid-row1)))
(define valid-row2 '("Jete" 49 #t))

; ====================
; functions

(define (add-row row db)
  ; Row Database -> Database
  ; adds a row to the database if the format is correct
  (local (
          (define preds (map (lambda (p) (schema-pred p)) (database-header db)))
          (define correct?
            (and
             (= (length row) (length preds))
             (andmap (lambda (p el) (p el)) preds row))))
          ; - IN -
    (make-database (database-header db)
                   (if correct?
                       (cons row (database-data db))
                       (database-data db)))))

; !!! add add multiple rows at a time
#;
(define (add-data data db)
  ; Data Database -> Database
  ; adds a whole chunk of data to a database
  (cons (fn-on-row (first d)) (fn-on-data (rest d))))

; ======================
; checks


(check-expect (add-row valid-row1 attendance0)
              (make-database (database-header attendance0) `(,valid-row1)))
(check-expect (add-row invalid-row1 attendance0) attendance0)
(check-expect (add-row valid-row2 attendance1)
              (make-database (database-header attendance1)
                             (cons valid-row2 (database-data attendance1))))
(check-expect (add-row invalid-row1 attendance1) attendance1)



; ======================
; action!

attendance0
(add-row valid-row1 attendance0)
(add-row valid-row2 (add-row valid-row1 attendance0))