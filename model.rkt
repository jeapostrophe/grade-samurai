#lang racket/base
(require racket/date
         racket/contract
         racket/serialize)

(struct assignment (normal-weight optional-weight id due-secs eval-secs peer-secs questions) #:prefab)
(struct question (normal-weight optional-weight prompt type) #:prefab)

(struct question-self-eval
        (self-score file line-number) #:prefab);TODO merge file and line-number

(struct peer-grading-struct
        (peer-id question-grades) #:prefab)

(struct student (nickname firstname lastname email) #:prefab)

(struct answer (timestamp comments)
        #:prefab)
(struct answer:bool answer (value)
        #:prefab)
(struct answer:numeric answer (value)
        #:prefab)

(provide/contract
 [struct assignment
         ([normal-weight number?]
          [optional-weight number?]
          [id string?]
          [due-secs number?]
          [eval-secs number?]
          [peer-secs (or/c false/c number?)]
          [questions (listof question?)])]
 [struct question
         ([normal-weight number?]
          [optional-weight number?]
          [prompt string?]
          [type (or/c 'bool 'numeric)])]
 [struct question-self-eval
         ([self-score number?]
          [file string?]
          [line-number string?])]
 [struct peer-grading-struct
         ([peer-id string?]
          [question-grades (listof question-self-eval?)])]
 [struct answer:bool
         ([timestamp exact-nonnegative-integer?]
          [comments string?]
          [value boolean?])]
 [struct answer:numeric
         ([timestamp exact-nonnegative-integer?]
          [comments string?]
          [value (between/c 0 1)])]
 [struct student
         ([nickname string?]
          [firstname string?]
          [lastname string?]
          [email string?])])
