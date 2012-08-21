#lang racket/base
(require racket/date
         racket/serialize)

(struct assignment (normal-weight optional-weight id due-secs eval-secs peer-secs questions) #:prefab)
(struct question (normal-weight optional-weight prompt type) #:prefab)

(struct question-self-eval
  (self-score file line-number) #:prefab);TODO merge file and line-number

(struct student (nickname firstname lastname email) #:prefab)

(provide (all-defined-out))
