#lang racket/base
(require racket/date
         racket/serialize)

(struct assignment 
  (title id optional? start due question-weight peer-review-weight questions) #:transparent)
(struct question 
  (type weight question answers) #:transparent)

(struct student (nickname firstname lastname email) #:prefab)

(provide (all-defined-out))