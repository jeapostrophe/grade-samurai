#lang racket/base
(require web-server/servlet-env
         racket/match
         racket/list
         racket/date
         racket/runtime-path
         "app.rkt"
         "model.rkt")

(define (simple-date month day year)
  (find-seconds 0 0 17 day month year))

(define musical-notation
  (assignment 3/20 0 "music" (+ (current-seconds) 10) (+ (current-seconds) 60) (+ (current-seconds) 100)
   (list 
    (question 1 0 "Explain how musical notation is like a programming language." 'bool)
    (question 1 0 "Be sure that you have." 'bool)
    (question 1 0 "What's your favorite number between 0 and 1?" 'numeric))))

(define rinterp
  (assignment 0 3/40 "rinterp" (simple-date 1 23 2013) (simple-date 1 30 2013) (simple-date 1 31 2013)
   (list
    (question 0 1 "Does your interpreter have a runtime error that is not a parse error?" 'bool))))

(define final
  (assignment 3/5 0 "final" (simple-date 4 15 2013) (simple-date 4 23 2013) #f
   (list (question 0 1 "What grade you think you deserve on the final?" 'numeric))))

(define assignments (list musical-notation rinterp final))

(require (planet jaymccarthy/ldap))
(define (authenticate-byu u p)
  (ldap-authenticate "ldap.byu.edu" 389 (format "uid=~a,ou=People,o=BYU.edu" u) p))

(define-runtime-path secret-salt-path "secret-salt")

(define-runtime-path db-path "db")

(module+ main
         (samurai-go!
          #:db db-path
          #:port 8000
          #:assignments assignments
          #:authenticate 
          (λ (u p)
            (match u
              ["admin"
               (and (string=? p "password")
                    'admin)]
                    ["test"
                     (and (string=? p "test")
                          'user)]
              [_ 
               (and (authenticate-byu u p)
                          'user)]))
          #:username-request-text "NetId: "
          #:password-request-text "RouteY Password: "))
