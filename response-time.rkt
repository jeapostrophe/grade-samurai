#lang racket/base
(require racket/cmdline
         plot
         racket/function
         racket/list
         racket/path
         racket/file
         "model.rkt")

(define (directory-list* d)
  (if (directory-exists? d)
    (directory-list d #:build? #t)
    empty))

(define (go as root-path output-p)
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
      (define pp (build-path (path-only sp) ".." "prof-eval"
                             (file-name-from-path sp)))
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

  (define curve-k (* 60 60 2))
  (define (times->curve l)
    (define fl (filter number? l))
    (define k->count (make-hasheq))
    (for ([t (in-list fl)])
      (hash-update! k->count (quotient t curve-k) add1 0))
    k->count)

  (define e-times
    (times->curve
     (for/list ([sp (in-list self-evals)])
       (eval-time sp))))
  (define p-times
    (times->curve
     (for/list ([sp (in-list self-evals)])
       (define pp (build-path (path-only sp) ".." "prof-eval"
                              (file-name-from-path sp)))
       (and (file-exists? pp) (eval-time pp)))))

  (let ()
    (define all-times (append (hash-keys e-times)
                              (hash-keys p-times)))
    (define max-k (apply max all-times))
    (define min-k (apply min all-times))

    (define skip 1)
    (define (secs->n t)
      (* (- (quotient t curve-k) min-k) skip))

    ;; xxx also graph response times
    ;; xxx put on site
    (parameterize ([plot-width 800]
                   [plot-height 600]
                   [plot-y-transform cbrt-transform]
                   [discrete-histogram-skip skip]
                   [plot-x-ticks no-ticks])
      (plot-file
       #:legend-anchor 'top-left
       #:x-label "Time"
       #:y-label "Question Count"
       #:title "Activity"
       (list (stacked-histogram
              #:add-ticks? #f
              #:labels '("Turn-in" "Grading")
              (for/list ([n (in-range min-k max-k)])
                (vector (* n curve-k)
                        (list (hash-ref e-times n 0)
                              (hash-ref p-times n 0)))))
             (for/list ([a (in-list as)])
               
                (tick (secs->n (assignment-due-secs a))
                      #t
                      (format "~a"
                              (assignment-id a)))
                ;; xxx need a label
                (y-axis (secs->n (assignment-due-secs a))
                        #:ticks? #f
                        #:labels? #f)))
       output-p))))

(module+ main
  (require "courses/2013-winter-330/assignments.rkt")
  (command-line #:program "response-time"
                #:args (dir output-p)
                (go assignments dir output-p)))
