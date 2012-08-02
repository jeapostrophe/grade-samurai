#lang racket/base
(require web-server/servlet-env
         racket/list
         racket/date
         racket/runtime-path
         "app.rkt"
         "model.rkt")

(define (simple-date month day year)
  (find-seconds 0 0 17 day month year))

(define musical-notation
  (assignment 
   "Musical Notation" "music" #f
   (simple-date 1 4 2012) (+ (current-seconds) 10) ;(simple-date 1 7 2013)
   (/ 3 20) (/ 1 20)
   (list (question 'free-response #f "In 850 words, or less, explain how musical notation is like a programming language."
                   empty))))

(define rinterp
  (assignment
   "Rudimentary Interpreter" "rinterp" #t
   (simple-date 1 23 2013) (simple-date 1 30 2013)
   (/ 3 40) (/ 1 40)
   (list
    (question 'line-num #f "Show a test case that is not a parse error but is a runtime error." empty)
    (question 'line-num #f "Show a test case where the previous error is called at runtime." empty))))

(define garbage-collectors
  (assignment 
   "Garbage Collectors" "collect" #f
   (simple-date 3 12 2013) (simple-date 3 27 2013)
   (/ 3 20) (/ 1 20)
   (list
    (question 'multiple-choice (/ 1 3) "Question text" '("Correct Answer" "Other Answer1" "Other Answer2"))
    (question 'line-num #f "Pick line number where blah blah occurs" empty))))

(define final
  (assignment
   "Final Exam" "final" #f
   #f (simple-date 4 15 2013)
   (/ 3 5) #f
   (list (question 'offline 1 "Final Exam Questions" empty))))

(define assignments (list musical-notation rinterp garbage-collectors final))

(require (planet jaymccarthy/ldap))
(define (authenticate-byu u p)
  (ldap-authenticate "ldap.byu.edu" 389 (format "uid=~a,ou=People,o=BYU.edu" u) p))

(define-runtime-path secret-salt-path "secret-salt")

(serve/servlet (make-start-handler
                #:admin-users-hash (hash "admin" "password")
                #:assignment-list assignments
                #:authenticate-users-with authenticate-byu
                #:username-request-text "NetId: "
                #:password-request-text "RouteY Password: "
                #:secret-salt-path secret-salt-path)
               #:port 9000
               #:listen-ip #f
               #:quit? #t
               #:launch-browser? #t
               #:servlet-regexp #rx""
               #:servlet-path "/")
