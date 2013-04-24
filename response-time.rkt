#lang racket/base
(require racket/cmdline
         racket/list
         racket/path
         racket/file)

(define root-path
  (command-line #:program "response-time"
                #:args (dir) dir))

(define (directory-list* d)
  (if (directory-exists? d)
    (directory-list d #:build? #t)
    empty))

(define users
  (directory-list* (build-path root-path "db" "users")))

(define assignments
  (append-map (λ (u)
                (directory-list* (build-path u "assignments")))
              users))

(define self-evals
  (append-map (λ (a) (directory-list* (build-path a "self-eval")))
              assignments))

(define (eval-time p)
  (vector-ref (struct->vector (file->value p)) 1))

(define times
  (for/list ([sp (in-list self-evals)])
    (define pp (build-path (path-only sp) ".." "prof-eval" (file-name-from-path sp)))
    (cond
      [(equal? "final" (path->string (second (reverse (explode-path sp)))))
       (* 60 2)]
      [(file-exists? pp)
       (- (eval-time pp)
          (eval-time sp))]
      [else
       0])))

(define (sum l)
  (for/sum ([e (in-list l)]) e))

(define (median l)
  (define sl (sort l <))
  (list-ref sl (quotient (length sl) 2)))

(define (->time t)
  (cond
    [(< t 60)
     (format "~a s" (real->decimal-string t))]
    [(< t (* 60 60))
     (format "~a m" (real->decimal-string (/ t 60)))]
    [else
     (format "~a h" (real->decimal-string (/ t (* 60 60))))]))

(define (stats l)
  (printf "How Many: ~a\n" (length l))
  (printf "     Min: ~a\n" (->time (apply min (filter positive? l))))
  (printf "     Max: ~a\n" (->time (apply max l)))
  (printf "  Median: ~a\n" (->time (median l)))
  (printf " Average: ~a\n" (->time (/ (sum l) (length l)))))

(stats times)
