#lang racket
(require racket/date)

(struct deadline 
  (military-hours minutes slack-minutes))
(struct assignment 
  (title optional? start-date due-date question-weight peer-review-weight questions) #:transparent)
(define (simple-date simple-month simple-day)
      (struct-copy date (current-date) [day simple-day] [month simple-month]))
(struct question 
  (type weight question answers) #:transparent)

(provide (all-defined-out))