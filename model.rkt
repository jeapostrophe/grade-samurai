#lang racket/base
(require racket/date
         racket/serialize)

(struct assignment 
  (title id optional? start due question-weight peer-review-weight questions) #:transparent)
(struct question 
  (type weight question answers) #:transparent)
(struct question-self-eval
  (self-score file line-number) #:prefab);TODO merge file and line-number

(struct student (nickname firstname lastname email) #:prefab)

(provide (all-defined-out))