#lang racket/base
(require racket/date
         racket/serialize)

(struct deadline 
  (military-hours minutes slack-minutes))
(struct assignment 
  (title optional? start-date due-date question-weight peer-review-weight questions) #:transparent)
(define (simple-date simple-month simple-day)
      (struct-copy date (current-date) [day simple-day] [month simple-month]))
(struct question 
  (type weight question answers) #:transparent)

(struct student (nickname firstname lastname email) #:prefab)                  

(provide (all-defined-out))