#lang racket/base
(require racket/contract
         unstable/debug
         web-server/servlet-env
         unstable/contract
         web-server/http
         web-server/http/bindings
         web-server/dispatchers/dispatch
         web-server/dispatch
         web-server/servlet/web
         web-server/formlets
         racket/file
         racket/list
         racket/path
         racket/match
         racket/date
         racket/runtime-path
         racket/string
         racket/function
         file/md5
         (only-in srfi/13 string-trim-both)
         "model.rkt"
         web-server/http/id-cookie)

(define DEBUG? #f)

(define (format-% v)
  (format "~a%" (real->decimal-string (* 100 v) 2)))

(define (string->lines s)
  (string-split s "\n"))

(define (contains-greater-than-80-char-line? file-content)
  (for/or ([l (in-list (string->lines (bytes->string/utf-8 file-content)))])
    ((string-length l) . > . 80)))

(module+ test
  (require rackunit)
  (check-equal? (contains-greater-than-80-char-line? #"
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
") #t)
  (check-equal? (contains-greater-than-80-char-line? #"
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklm
") #f))

(define (newline->br s)
  (define positions
    (regexp-match-positions* #rx"\n" s))
  (define-values (html pos)
    (for/fold ([html empty] [pos 0])
        ([pos-pair positions])
      (values
       (append
        html
        (list (substring s pos (car pos-pair))
              '(br)))
       (cdr pos-pair))))
  (append html (list (substring s pos (string-length s)))))

(define (string->linked-html s)
  (define positions
    (regexp-match-positions* #px"[l|L]\\d+" s))
  (define-values (html pos)
    (for/fold ([html empty] [pos 0])
        ([pos-pair positions])
      (values
       (append
        html
        (newline->br (substring s pos (car pos-pair)))
        (list
         `(a ([class "line-link"]
              [href
               ,(format "#LC~a"
                        (substring
                         (string-upcase
                          (substring s
                                     (car pos-pair)
                                     (cdr pos-pair)))
                         1))])
             ,(substring s (car pos-pair) (cdr pos-pair)))))
       (cdr pos-pair))))
  `(p ([class "comment"])
      ,@html ,@(newline->br (substring s pos (string-length s)))))

(module+ test
  (check-equal?
   (newline->br "foo\nbar")
   (list "foo" '(br) "bar"))
  (check-equal?
   (newline->br "foo\nbar\n")
   (list "foo" '(br) "bar" '(br) ""))
  (check-equal?
   (string->linked-html
    "L3 - You have a mistake here\n\nL55 - You have a mistake here until L67\n")
   `(p ([class "comment"])
       ""
       (a ([class "line-link"]
           [href "#LC3"])
          "L3")
       " - You have a mistake here"
       (br) "" (br) ""
       (a ([class "line-link"]
           [href "#LC55"])
          "L55")
       " - You have a mistake here until "
       (a ([class "line-link"]
           [href "#LC67"])
          "L67")
       "" (br) "")))

(define (make-parent-directory* p)
  (define parent (path-only p))
  (make-directory* parent))

(define (letter-grade ng)
  (cond
    [(> ng 0.93) "A"]
    [(> ng 0.90) "A-"]
    [(> ng 0.86) "B+"]
    [(> ng 0.83) "B"]
    [(> ng 0.80) "B-"]
    [(> ng 0.76) "C+"]
    [(> ng 0.73) "C"]
    [(> ng 0.70) "C-"]
    [(> ng 0.66) "D+"]
    [(> ng 0.63) "D"]
    [(> ng 0.60) "D-"]
    [else "F"]))

(define (path->last-part f)
  (define-values (base name must-be-dir?)
    (split-path f))
  (path->string name))

(define (directory-list* pth)
  (if (directory-exists? pth)
    (sort (map path->last-part (directory-list pth))
          string-ci<=?)
    empty))

(define-runtime-path source-dir ".")

(define (samurai-go!
         #:closed? closed?
         #:db db-path
         #:port port
         #:assignments pre-assignments
         #:authenticate authenticate-users
         #:professor-email professor-email
         #:username-request-text login-formlet-un-text
         #:password-request-text login-formlet-pw-text)

  (define (check-user u)
    (or (is-admin?)
        (equal? u (current-user))))

  (define (display-to-file* v pth)
    (GRADE-CACHE-CLEAR! (current-user))
    (make-parent-directory* pth)
    (display-to-file v pth #:exists 'replace))

  (define (write-to-file* v pth)
    (GRADE-CACHE-CLEAR! (current-user))
    (make-parent-directory* pth)
    (write-to-file v pth #:exists 'replace))

  (define assignments
    (sort pre-assignments
          <=
          #:key assignment-due-secs))

  (define secret-salt-path (build-path db-path "secret-salt"))

  (define (id->assignment a-id)
    (findf (λ (a) (string=? a-id (assignment-id a))) assignments))

  (define secret-salt
    (make-secret-salt/file secret-salt-path))

  (define (is-admin?)
    (eq? 'admin (current-user-type)))

  (define (page/root req)
    (send/back
     (if (is-admin?)
       (redirect-to (main-url page/admin/students))
       (redirect-to (main-url page/student (current-user))))))

  (define (page/login req [last-error #f])
    (define login-formlet
      (formlet
       (table
        (tr (td ,login-formlet-un-text)
            (td ,{(to-string (required (text-input))) . => . username}))
        (tr (td ,login-formlet-pw-text)
            (td ,{(to-string (required (password-input))) . => . password})))
       (values username password)))
    (define log-req
      (send/suspend
       (λ (k-url)
         (template
          #:breadcrumb (list (cons "Home" (main-url page/root))
                             (cons "Login" #f))
          `(div ([id "login"])
                (form ([action ,k-url] [method "post"])
                      ,@(formlet-display login-formlet)
                      (input ([type "submit"] [value "Log in"])))
                ,@(if last-error
                    `((h1 ([class "error"]) ,last-error))
                    '()))))))
    (define-values (username password)
      (formlet-process login-formlet log-req))

    (define authenticated?
      (authenticate-users username password))

    (cond
      [authenticated?
       (cond
         [(and closed?
               (not (eq? 'admin authenticated?))
               (not
                (directory-exists?
                 (user-path username))))
          (page/login
           req
           (format "The course is over, go away :)"))]
         [else
          (redirect-to (parameterize ([current-user username]
                                      [current-user-type authenticated?])
                         (main-url page/root))
                       #:headers
                       (list (cookie->header
                              (make-id-cookie
                               "name"
                               secret-salt
                               (format "~a:~a"
                                       authenticated?
                                       username)))))])]
      [else
       (page/login
        req
        (format "Invalid password for user (~S)" username))]))


  (define (default-text-input default-string)
    (to-string (default (string->bytes/utf-8 default-string)
                 (text-input #:value (string->bytes/utf-8 default-string)))))

  (define (page/account req)
    (define existing-info
      (student-info (current-user)))

    (define account-formlet
      (formlet
       (div ([id "form-inputs"])
            (table
             (tr (td "Legal First Name: ")
                 (td ,{(default-text-input (student-firstname existing-info))
                       . => . first-name}))
             (tr (td "Last Name: ")
                 (td ,{(default-text-input (student-lastname existing-info))
                       . => . last-name}))
             (tr (td "I Prefer to be Known As: ")
                 (td ,{(default-text-input (student-nickname existing-info))
                       . => . nick-name}))
             (tr (td "Email Address: ")
                 (td ,{(default-text-input (student-email existing-info))
                       . => . email}))
             (tr (td "Picture I can be recognized by: ")
                 (td ,{(file-upload) . => . photo}))))
       (values first-name last-name nick-name email photo)))

    (define account-form
      (send/suspend
       (λ (k-url)
         (template
          #:breadcrumb (list (cons "Home" (main-url page/student (current-user)))
                             (cons (current-user) #f)
                             (cons "Details" #f))
          `(div
            ([id "account"])
            (form ([action ,k-url]
                   [method "post"]
                   [enctype "multipart/form-data"])
                  ,@(formlet-display account-formlet)
                  (p "Instead of this: "
                     (img
                      ([src ,(main-url page/student/photo (current-user))]
                       [height "160"])))
                  (input ([type "submit"] [value "Update Info"]))))))))

    (define-values (first-name last-name nick-name email photo)
      (formlet-process account-formlet account-form))

    (write-to-file* (student nick-name first-name last-name email)
                    (user-info-path (current-user)))
    (when (binding:file? photo)
      (display-to-file* (binding:file-content photo)
                        (user-image-path (current-user))))

    (redirect-to (main-url page/root)))

  (define-values (main-dispatch main-url main-applies?)
    (dispatch-rules+applies
     [("")
      page/root]
     [("admin" "students")
      page/admin/students]
     [("admin" "assignments")
      page/admin/assignments]
     [("admin" "assignments" (string-arg))
      page/admin/assignments/view]
     [("admin" "grade-next")
      page/admin/grade-next]
     [("login")
      page/login]
     [("logout")
      page/logout]
     [("account")
      page/account]
     [("student" (string-arg))
      page/student]
     [("student" (string-arg) "photo.jpg")
      page/student/photo]
     [("student" (string-arg) "assignment" (string-arg) "files")
      page/student/assignment/files]
     [("student" (string-arg) "assignment" (string-arg) "files" "delete" (string-arg))
      page/student/assignment/files/delete]
     [("student" (string-arg) "assignment" (string-arg) "self" "edit")
      page/student/assignment/self/edit]
     [("student" (string-arg) "assignment" (string-arg) "self")
      page/student/assignment/self]
     [("student" (string-arg) "assignment" (string-arg) "peer" "edit")
      page/student/assignment/peer/edit]
     [("student" (string-arg) "assignment" (string-arg) "peer")
      page/student/assignment/peer]))

  (define default-peer
    "The Spanish Inquisition")
  (define (assignment-peer cu id)
    (if (file-exists? (assignment-peer-path cu id))
      (file->string (assignment-peer-path cu id))
      default-peer))
  (define (assignment-co-peer cu id)
    (or (for/or ([u (in-list (users))])
          (and (equal? cu (assignment-peer u id))
               u))
        default-peer))

  (define (users-path)
    (build-path db-path "users"))
  (define (users)
    (directory-list* (users-path)))
  (define (sorted-users)
    (sort (users)
          string-ci<=?
          #:key (compose student-lastname student-info)))

  (define (user-path u)
    (build-path (users-path) u))
  (define (user-info-path u)
    (build-path (user-path u) "info.rktd"))
  (define (user-image-path u)
    (build-path (user-path u) "photo.jpg"))
  (define (assignment-path u id)
    (build-path (user-path u) "assignments" id))
  (define (assignment-file-path u id)
    (build-path (assignment-path u id) "files"))
  (define (assignment-files u id)
    (directory-list* (assignment-file-path u id)))
  (define (assignment-peer-path u id)
    (build-path (assignment-path u id) "peer"))
  (define (assignment-question-student-grade-path u id i)
    (build-path (assignment-path u id) "self-eval" (number->string i)))
  (define (assignment-question-student-grade-path/peer u id i)
    (build-path (assignment-path u id) "peer-eval" (number->string i)))
  (define (assignment-question-prof-grade-path u id i)
    (build-path (assignment-path u id) "prof-eval" (number->string i)))

  (define ((make-assignment-question-student-grade
            assignment-question-student-grade-path)
           cu id i)
    (define p (assignment-question-student-grade-path cu id i))
    (and (file-exists? p) (file->value p)))

  (define assignment-question-student-grade
    (make-assignment-question-student-grade
     assignment-question-student-grade-path))
  (define assignment-question-student-grade/peer
    (make-assignment-question-student-grade
     assignment-question-student-grade-path/peer))
  (define assignment-question-prof-grade
    (make-assignment-question-student-grade
     assignment-question-prof-grade-path))

  (define (assignment-question-peer-grade cu id i)
    (define co-peer (assignment-co-peer cu id))
    (assignment-question-student-grade/peer co-peer id i))
  (define (assignment-question-peer-grade-path cu id i)
    (define co-peer (assignment-co-peer cu id))
    (assignment-question-student-grade-path/peer co-peer id i))

  ;; XXX CLEANUP this
  (define (assignment-question-student-bool-grade cu id i)
    (define v (assignment-question-student-grade cu id i))
    (if v
      (answer:bool-value v)
      'n/a))
  (define (assignment-question-student-bool-grade/peer cu id i)
    (define v (assignment-question-student-grade/peer cu id i))
    (if v
      (answer:bool-value v)
      'n/a))
  (define (assignment-question-student-numeric-grade/peer cu id i)
    (define v (assignment-question-student-grade/peer cu id i))
    (if v
      (answer:numeric-value v)
      #f))

  (define (assignment-question-prof-bool-grade cu id i)
    (define v (assignment-question-prof-grade cu id i))
    (if v
      (answer:bool-value v)
      'n/a))
  (define (assignment-question-prof-bool-grade/peer cu id i)
    (define peer (assignment-peer cu id))
    (assignment-question-prof-bool-grade peer id i))

  (define (assignment-question-prof-numeric-grade cu id i)
    (define v (assignment-question-prof-grade cu id i))
    (if v
      (answer:numeric-value v)
      #f))
  (define (assignment-question-prof-numeric-grade/peer cu id i)
    (define peer (assignment-peer cu id))
    (assignment-question-prof-numeric-grade peer id i))

  (define (is-optional-enabled? cu)
    (for/and ([a (in-list assignments)]
              #:unless (equal? "wlang1" (assignment-id a))
              #:when (zero? (assignment-normal-weight a))
              #:when (zero? (assignment-optional-weight a)))
      (define a-id (assignment-id a))
      (or
       ;; It is before the self assessment date
       (< (current-seconds) (assignment-eval-secs a))
       ;; It is after and...
       (and
        ;; they've evaluated themselves...
        (self-eval-completed? cu a)
        ;; and so have I...
        (prof-eval-completed? cu a)
        ;; and I said they did do it
        (assignment-question-prof-bool-grade cu a-id 0)))))

  (define (compute-question-grade cu optional? default-grade id i q)
    (match-define (question nw ow _ t) q)
    (define ow-p (if optional? ow 0))
    (define ps
      (match t
        ['numeric
         (or (assignment-question-prof-numeric-grade cu id i)
             default-grade)]
        ['bool
         (define student-correct?
           (assignment-question-student-bool-grade cu id i))
         (define prof-correct?
           (assignment-question-prof-bool-grade cu id i))
         (match* (student-correct? prof-correct?)
           [(  #t   #t) 10/10]
           [(  #t   #f) -1/10]
           [(  #f   #t)  1/10]
           [(  #f   #f)  0/10]
           [('n/a    _) default-grade]
           [(   _ 'n/a) default-grade])]))
    (* (+ nw ow-p) ps))

  (define (compute-peer-grade cu optional? default-grade id i q)
    (match-define (question nw ow _ t) q)
    (define ow-p (if optional? ow 0))
    (define ps
      (match t
        ['numeric
         (define prof
           (assignment-question-prof-numeric-grade/peer cu id i))
         (define student
           (assignment-question-student-numeric-grade/peer cu id i))
         (cond
           [(and prof student)
            (- 1 (abs (- prof student)))]
           [student
            1]
           [else
            default-grade])]
        ['bool
         (define prof (assignment-question-prof-bool-grade/peer cu id i))
         (define student (assignment-question-student-bool-grade/peer cu id i))
         (cond
           [(eq? 'n/a student)
            default-grade]
           [(eq? 'n/a prof)
            1]
           [(equal? prof student)
            1]
           [else
            0])]))
    (* (+ nw ow-p) ps))

  (define ((make-compute-question-grades compute-question-grade)
           cu optional? default-grade id qs)
    (for/sum
     ([q (in-list qs)]
      [i (in-naturals)])
     (compute-question-grade cu optional? default-grade id i q)))

  (define compute-question-grades
    (make-compute-question-grades compute-question-grade))
  (define compute-peer-grades
    (make-compute-question-grades compute-peer-grade))

  (define (compute-assignment-grade* cu a default-grade)
    (match-define (assignment nw ow id ds es ps qs) a)
    (define optional-enable?
      (is-optional-enabled? cu))
    (define ow-p
      (if optional-enable? ow 0))
    (define self-pts
      (compute-question-grades
       cu
       optional-enable? default-grade
       id qs))
    (define peer-pts
      (compute-peer-grades
       cu
       optional-enable? default-grade
       id qs))
    (define pre-score
      (if (number? ps)
        (* (+ ow-p nw)
           (+ (* 9/10 self-pts)
              (* 1/10 peer-pts)))
        (* (+ ow-p nw) self-pts)))
    (if (zero? nw)
      (max 0 pre-score)
      pre-score))

  (define (compute-assignment-grade^ cu a default-grade)
    (define optional-enable?
      (is-optional-enabled? cu))
    (define base
      (compute-assignment-grade*
       cu
       a
       (if (< (current-seconds) (assignment-due-secs a))
         default-grade
         0)))
    (if optional-enable?
      base
      (min 1 base)))

  (define (compute-assignment-grade cu a default-grade)
    (GRADE-CACHE-USE
     cu
     (list a default-grade)
     (λ () (compute-assignment-grade^ cu a default-grade))))

  (define (compute-assignment-grade/id cu a-id default-grade)
    (define a (id->assignment a-id))
    (compute-assignment-grade cu a default-grade))

  (define (compute-grade* cu default-grade)
    (for/sum ([a (in-list assignments)])
             (compute-assignment-grade cu a default-grade)))

  (define (maximum-grade-so-far)
    (define now (current-seconds))
    (for/sum ([a (in-list assignments)])
             (match-define (assignment nw ow id ds es ps qs) a)
             (if (< ds now)
               nw
               0)))

  (define GRADE-CACHE (make-hash))
  (define GRADE-CACHE-T
    (thread
     (λ ()
       (let loop ()
         (define now (current-seconds))
         (define all-due
           (append-map (λ (a)
                         (list (assignment-due-secs a)
                               (assignment-eval-secs a)
                               (assignment-peer-secs a)))
                       assignments))
         (define still-due
           (filter (λ (t)
                     (and t
                          (< now t)))
                   all-due))
         (define sorted-due
           (sort still-due <))
         (match sorted-due
           [(list)
            (void)]
           [(list* some-due _)
            (sleep (- some-due now))
            (for ([k (in-hash-keys GRADE-CACHE)])
              (hash-remove! GRADE-CACHE k))
            (loop)])))))
  (define (GRADE-CACHE-CLEAR! cu)
    (hash-remove! GRADE-CACHE cu))
  (define (GRADE-CACHE-USE cu path t)
    (define user-ht
      (hash-ref! GRADE-CACHE cu
                 (λ () (make-hash))))
    (hash-ref! user-ht path t))

  (define (compute-grade cu dg)
    (GRADE-CACHE-USE
     cu (list #f dg)
     (λ () (compute-grade* cu dg))))

  (define (assignment-file-display cu a-id)
    (define-values (html end-line-number)
      (for/fold ([html empty]
                 [line-offset 1])
          ([file (in-list (assignment-files cu a-id))])
        (define-values (table new-offset)
          (file->html-table
           a-id
           (build-path (assignment-file-path cu a-id) file)
           line-offset))
        (values (append html (list table)) new-offset)))

    `(div ([class "files"])
          ,@html))

  (define (side-by-side-render cu a-id rhs #:sticky? [sticky? #f])
    `(div ([class ,(format "side-by-side~a"
                           (if sticky?
                             " sticky"
                             ""))])
          (div ([class "left"]) (div ([class "side-by-side-inner"])
                                     ,(assignment-file-display cu a-id)))
          (div ([class "right"]) (div ([class "side-by-side-inner"])
                                      ,@rhs))))

  (define (format-grade cu default-grade)
    (show-grade
     (compute-grade cu default-grade)))

  (define (show-grade g)
    (define l (letter-grade g))
    `(span ([class ,(substring l 0 1)])
           ,(format "~a (~a)"
                    (format-% g)
                    l)))

  (define (page/student/assignment/self/edit req cu a-id)
    (check-user cu)
    (define assignment (id->assignment a-id))
    (define the-breadcrumb
      (list (cons "Home" (main-url page/student cu))
            (cons "Assignments" #f)
            (cons a-id #f)
            (cons "Self Evaluation" #f)
            (cons "Edit" #f)))

    (define (overdue-or thunk)
      (if (<= (assignment-eval-secs assignment) (current-seconds))
        (send/back
         (template
          #:breadcrumb the-breadcrumb
          "Self evaluation past due."))
        (thunk)))

    (overdue-or
     (λ ()
       (for ([question (assignment-questions assignment)]
             [i (in-naturals)])
         (unless
             (file-exists? (assignment-question-student-grade-path cu a-id i))
           (define answer
             (grade-question
              cu cu a-id question i
              #:breadcrumb the-breadcrumb
              #:last? #t
              #:their? #f))
           (overdue-or
            (λ ()
              (write-to-file*
               answer
               (assignment-question-student-grade-path cu a-id i))))))

       ;; XXX redirect to self eval view (and in the other place too)
       (template
        #:breadcrumb the-breadcrumb
        "Self evaluation completed."))))

  (define (format-answer which ans
                         #:delete? [delete? #f])
    (cond
      [ans
       `(div ([class ,(format "answer ~a" which)])
             (p ,(format "~a evaluation is: ~a"
                         which
                         (match ans
                           [(answer:bool _ _ completed?)
                            (if completed?
                              "Yes"
                              "No")]
                           [(answer:numeric _ _ value)
                            (format-% value)]))
                ,(if delete?
                   `(span " "
                          (a ([href ,delete?])
                             "(rescind)"))
                   ""))
             ,(string->linked-html (answer-comments ans)))]
      [else
       `(div ([class ,(format "answer incomplete ~a" which)])
             (p ,(format "~a evaluation is not completed." which)))]))

  (define (page/assignment/generalized/html embed/url the-cu a-id #:peer [peer #f])
    (define assignment (id->assignment a-id))
    (define optional-enable? (is-optional-enabled? the-cu))
    (define cu (or peer the-cu))
    (side-by-side-render
     the-cu a-id
     (for/list ([q (in-list (assignment-questions assignment))]
                [i (in-naturals)])
       (define (delete-file-url pth)
         (define (okay?)
           (and
            (< (current-seconds)
               (if peer
                 (assignment-peer-secs assignment)
                 (assignment-eval-secs assignment)))
            (file-exists? pth)
            (not
             (file-exists?
              (assignment-question-prof-grade-path cu a-id i)))))
         (and
          (okay?)
          (embed/url
           (λ (req)
             (and
              (okay?)
              (delete-file pth)
              (redirect-to
               (if peer
                 (main-url page/student/assignment/peer/edit the-cu a-id)
                 (main-url page/student/assignment/self/edit the-cu a-id))))))))
       (define different?
         (answer-different? (assignment-question-prof-grade cu a-id i)
                            (if peer
                              (assignment-question-peer-grade cu a-id i)
                              (assignment-question-student-grade cu a-id i))))
       (match-define (question nw ow prompt type) q)
       (define ow-p (if optional-enable? ow 0))
       `(div ([class ,(format "answers~a"
                              (if different?
                                " diff"
                                ""))])
             (p (span ([class "weight"])
                      ,(format-% (+ nw ow-p)))
                ,prompt
                ,(format " (~a)" i))
             ,(format-answer
               #:delete? (and (not peer)
                              (delete-file-url
                               (assignment-question-student-grade-path
                                cu a-id i)))
               (if peer "Peer's Self" "Self")
               (assignment-question-student-grade cu a-id i))
             ,(format-answer
               (if peer "Peer's Professor" "Professor")
               (assignment-question-prof-grade cu a-id i))
             ,(format-answer
               #:delete? (and peer (delete-file-url
                                    (assignment-question-peer-grade-path
                                     cu a-id i)))
               (if peer "Your" "Peer's")
               (assignment-question-peer-grade cu a-id i))
             (a ([href ,(format "mailto:~a?subject=~a&body=~a"
                                professor-email
                                (format
                                 "330 - Dispute - ~a - ~a - ~a (~a) - ~a"
                                 the-cu a-id
                                 prompt
                                 i
                                 (if peer
                                   "Peer"
                                   "Self"))
                                "You jerk! I disagree with your evaluation of this question! Here's why:\n\n[FILL IN YOUR OBJECTION HERE]")])
                "Dispute!")))))

  (define (answer-different? x y)
    (match* (x y)
      [((answer:bool _ _ x) (answer:bool _ _ y))
       (not (equal? x y))]
      [((answer:numeric _ _ x) (answer:numeric _ _ y))
       (not (equal? x y))]
      [(#f _)
       #f]
      [(_ #f)
       #f]
      [(_ _)
       #t]))

  (define (page/student/assignment/self req cu a-id)
    (check-user cu)
    (define assignment (id->assignment a-id))
    (send/suspend/dispatch
     (λ (embed/url)
       (template
        #:breadcrumb
        (list (cons "Home" (main-url page/student cu))
              (cons "Assignments" #f)
              (cons a-id #f)
              (cons "Self Evaluation" #f))
        (page/assignment/generalized/html embed/url cu a-id)))))

  (define (page/student/assignment/peer req cu a-id)
    (check-user cu)
    (define assignment (id->assignment a-id))
    (define peer (assignment-peer cu a-id))
    (define the-breadcrumb
      (list (cons "Home" (main-url page/student cu))
            (cons "Assignments" #f)
            (cons a-id #f)
            (cons "Peer Evaluation" #f)))
    (cond
      [(equal? default-peer peer)
       (template
        #:breadcrumb the-breadcrumb
        `(div ([class "notice"]) "Your peer has not been assigned."))]
      [(not (peer-eval-completed? cu assignment))
       (template
        #:breadcrumb the-breadcrumb
        `(div ([class "notice"]) "Your peer eval is incomplete."))]
      [else
       (send/suspend/dispatch
        (λ (embed/url)
          (template
           #:breadcrumb the-breadcrumb
           (page/assignment/generalized/html embed/url cu a-id #:peer peer))))]))

  (define (page/student/assignment/peer/edit req cu a-id)
    (check-user cu)
    (define assignment (id->assignment a-id))
    (define the-breadcrumb
      (list (cons "Home" (main-url page/student cu))
            (cons "Assignments" #f)
            (cons a-id #f)
            (cons "Peer Evaluation" #f)
            (cons "Edit" #f)))

    (define (pick-a-person a-id)
      (define student-ids (users))
      (define finished-self-eval
        (remove cu
                (filter (λ (student-id)
                          (self-eval-completed? student-id assignment))
                        student-ids)))
      (define already-assigned
        (map (λ (student-id)
               (assignment-peer student-id a-id))
             student-ids))
      (define candidates
        (remove* already-assigned
                 finished-self-eval))
      (define the-peer
        (match (shuffle candidates)
          [(list* peer _)
           peer]
          [(list)
           (match (shuffle finished-self-eval)
             [(list* peer _)
              peer]
             [(list)
              (send/back
               (template
                #:breadcrumb the-breadcrumb
                "Peer evaluation is impossible, as no peers are available."))])]))
      (display-to-file* the-peer (assignment-peer-path cu a-id))
      the-peer)

    (define peer-id
      (if (file-exists? (assignment-peer-path cu a-id))
        (assignment-peer cu a-id)
        (pick-a-person a-id)))

    (define (overdue-or thunk)
      (if (<= (assignment-peer-secs assignment) (current-seconds))
        (send/back
         (template
          #:breadcrumb the-breadcrumb
          "Peer evaluation past due."))
        (thunk)))

    (overdue-or
     (λ ()
       (for ([question (assignment-questions assignment)]
             [i (in-naturals)])
         (when
             (and
              ;; I have not yet graded
              (not
               (file-exists?
                (assignment-question-student-grade-path/peer cu a-id i)))
              ;; They have grade
              (file-exists?
               (assignment-question-student-grade-path peer-id a-id i)))
           (define grade
             (grade-question
              cu peer-id a-id question i
              #:peer? #t
              #:breadcrumb
              (list (cons "Home" (main-url page/student cu))
                    (cons "Assignments" #f)
                    (cons a-id #f)
                    (cons "Peer Evaluation" #f)
                    (cons "Edit" #f))))
           (overdue-or
            (λ ()
              (write-to-file*
               grade
               (assignment-question-student-grade-path/peer cu a-id i))))))

       (template
        #:breadcrumb the-breadcrumb
        "Peer evaluation completed, or not available (as peer has not finished their grading.)"))))

  (define (grade-question cu stu a-id question i
                          #:breadcrumb bc
                          #:their? [their? #t]
                          #:last? [last? #f]
                          #:peer? [peer? #f]
                          #:extra [extra empty])

    (define last-i
      (and last?
           (for/or ([j (in-range i -1 -1)])
             (define g
               (assignment-question-student-grade stu a-id j))
             (and g
                  (not (string=? "" (answer-comments g)))
                  j))))
    (define last
      (and last-i
           (assignment-question-student-grade stu a-id last-i)))

    (match (assignment-question-student-grade stu a-id i)
      [(answer:bool _ _ #f)
       (answer:bool (current-seconds) "" #f)]
      [_

       (define boolean-formlet
         (formlet
          (p ,{(radio-group '(#t #f)
                            #:checked? (λ (x) x)
                            #:display (λ (x) (if x "Yes" "No")))
               . => . credit})
          credit))

       (define request
         (send/suspend
          (λ (k-url)
            (template
             #:breadcrumb bc
             (side-by-side-render
              #:sticky? #t
              stu
              a-id
              (append
               extra
               (list
                (if last
                  (format-answer
                   "Last answer"
                   (assignment-question-student-grade stu a-id last-i))
                  "")
                `(p ,(question-prompt question)
                    ,(format " (~a)" i))
                (if their?
                  (format-answer
                   "Their"
                   (assignment-question-student-grade stu a-id i))
                  "")
                (if peer?
                  (format-answer
                   "Peer"
                   (assignment-question-peer-grade stu a-id i))
                  "")
                (if their?
                  `(p "What do you think they earned?")
                  "")
                `(form ([action ,k-url] [method "post"])
                       ,(match (question-type question)
                          ['bool
                           `(p (button ([name "submit"]
                                        [type "submit"]
                                        [value "bool_Y"])
                                       "Yes")
                               (button ([name "submit"]
                                        [type "submit"]
                                        [value "bool_N"])
                                       "No"))]
                          ['numeric
                           `(p (button ([name "submit"]
                                        [type "submit"]
                                        [value "num_0"])
                                       "0")
                               (button ([name "submit"]
                                        [type "submit"]
                                        [value "num_0.5"])
                                       "0.5")
                               (button ([name "submit"]
                                        [type "submit"]
                                        [value "num_1"])
                                       "1")
                               (input ([name "numeric"]
                                       [type "text"]
                                       [value "[0, 1]"]))
                               (button ([name "submit"]
                                        [type "submit"]
                                        [value "num_input"])
                                       "Other"))])
                       (p "Provide evidence to justify that score.")
                       (textarea ([name "comments"]
                                  [rows "8"]
                                  [cols "60"])
                                 ,(if last
                                    (answer-comments last)
                                    ""))
                       (p "(If you need to refer to line numbers, prefix a number with L. For example, use L32 or l32 to refer to line 32)")))))))))

       (define bs
         (request-bindings/raw request))
       (define comment
         (bytes->string/utf-8
          (binding:form-value
           (bindings-assq #"comments" bs))))
       (define score
         (match
             (binding:form-value
              (bindings-assq #"submit" bs))
           [(or #"Yes" #"bool_Y")
            #t]
           [(or #"No" #"bool_N")
            #f]
           [(or #"0" #"num_0")
            0]
           [(or #"0.5" #"num_0.5")
            1/2]
           [(or #"1" #"num_1")
            1]
           [(or #"Other" #"num_input")
            (string->number
             (bytes->string/utf-8
              (binding:form-value
               (bindings-assq #"numeric" bs))))]))

       ((match (question-type question)
          ['bool
           answer:bool]
          ['numeric
           answer:numeric])
        (current-seconds) comment
        score)]))

  (define (file->html-table a-id file-path line-offset)
    (define file-lines
      (string-split (bytes->string/utf-8 (file->bytes file-path))
                    #px"\r\n?|\n"
                    #:trim? #f))
    (values
     `(div ([class "file"])
           (div ([class "meta"]) ,(format "~a" (path->last-part file-path)))
           (div
            ([class "data type-text"])
            (table
             ([class "lines"]
              [cellspacing "0"]
              [cellpadding "0"])
             (tbody
              ,@(for/list ([line-num (in-naturals line-offset)]
                           [line (in-list file-lines)])
                  `(tr ([id ,(format "LC~a" line-num)]
                        [rel ,(format "#LC~a" line-num)])
                       (td (pre ,(number->string line-num)))
                       (td (pre ,line))))))))
     (+ line-offset (length file-lines))))

  (define (page/logout req)
    (redirect-to
     (main-url page/root)
     #:headers
     (list (cookie->header (logout-id-cookie "name")))))

  (define (secs->time-text secs)
    (define s
      (if (secs . <= . 1)
        1
        secs))
    (when (s . < . 0)
      (set! s 1))
    (define unit
      (findf (λ (unit-pair) (s . >= . (car unit-pair)))
             `((,(* 60 60 24 7) . "week")
               (,(* 60 60 24) . "day")
               (,(* 60 60) . "hour")
               (60 . "minute")
               (1 . "second"))))
    (format "~a ~a~a"
            (quotient s (car unit))
            (cdr unit)
            (if (= 1 (quotient s (car unit)))
              ""
              "s")))

  (define a-day (* 60 60 24))
  (define 2-days (* a-day 2))
  (define (cond-hyperlink done? available closed text1 link1 text2 link2)
    (cond
      [(or (not available) (not closed))
       ""]
      [DEBUG?
       `(p (a ([href ,link1]) ,text1) (br)
           (a ([href ,link2]) ,text2))]
      [(< (current-seconds) available)
       text1]
      [(or done? (> (current-seconds) closed))
       `(a ([href ,link2]) ,text2)]
      [else
       `(p (a ([href ,link1]) ,text1) (br)
           (a ([href ,link2]) ,text2))]))

  (define (page/student req cu)
    (check-user cu)
    (GRADE-CACHE-CLEAR! cu)    
    ;; TODO base off which phases they can still do
    (define-values (past upcoming)
      (partition
       (λ (a)
         (if (assignment-peer-secs a)
           (or (peer-eval-completed? cu a)
               (> (current-seconds) (assignment-peer-secs a)))
           (or (self-eval-completed? cu a)
               (> (current-seconds) (assignment-eval-secs a)))))
       assignments))

    (define optional-enable? (is-optional-enabled? cu))

    ;; TODO render offline assignments (like the final) differently
    (define (render-assignment a)
      (define next-due
        (cond
          [(or (peer-eval-completed? cu a)
               (and (assignment-peer-secs a)
                    (> (current-seconds) (assignment-peer-secs a))))
           #f]
          [(or (self-eval-completed? cu a)
               (> (current-seconds) (assignment-eval-secs a)))
           (assignment-peer-secs a)]
          [(> (current-seconds) (assignment-due-secs a))
           (assignment-eval-secs a)]
          [else
           (assignment-due-secs a)]))

      `(table
        ([class ,(format "assignment ~a ~a"
                         (cond
                           [(not (zero? (assignment-optional-weight a)))
                            "optional"]
                           [(not (zero? (assignment-normal-weight a)))
                            "normal"]
                           [else
                            "optenable"])
                         (if (peer-eval-completed? cu a)
                           "completed"
                           "incomplete"))])
        (tr (td ,(assignment-id a))
            (td ,(format-%
                  (+ (assignment-normal-weight a)
                     (if optional-enable?
                       (assignment-optional-weight a)
                       0))))
            (td ,(cond
                   [next-due
                    (format
                     "Due ~a in ~a"
                     (date->string (seconds->date next-due) #t)
                     (secs->time-text (- next-due (current-seconds))))]
                   [(and (self-eval-completed? cu a)
                         (not (prof-eval-completed? cu a)))
                    "Completed, waiting on professor evaluation."]
                   [else
                    `(span "Completed: "
                           ,(format-%
                             (compute-assignment-grade/id
                              cu
                              (assignment-id a)
                              0)))])))
        (tr (td ,(cond-hyperlink
                  #f
                  (current-seconds) (assignment-due-secs a)
                  "Turn in Files"
                  (main-url page/student/assignment/files
                            cu (assignment-id a))
                  "View Files"
                  (main-url page/student/assignment/files cu (assignment-id a))))
            (td ,(cond-hyperlink
                  (self-eval-completed? cu a)
                  (assignment-due-secs a) (assignment-eval-secs a)
                  "Self Evaluation"
                  (main-url page/student/assignment/self/edit
                            cu (assignment-id a))
                  "Self Evaluation Details"
                  (main-url page/student/assignment/self
                            cu (assignment-id a))))
            (td ,(cond-hyperlink
                  (peer-eval-completed? cu a)
                  (assignment-eval-secs a) (assignment-peer-secs a)
                  "Grade a Peer"
                  (main-url page/student/assignment/peer/edit
                            cu (assignment-id a))
                  "Grade a Peer Details"
                  (main-url page/student/assignment/peer
                            cu (assignment-id a)))))))
    (send/back
     (template
      #:breadcrumb (list (cons "Home" #f))
      (class-average-table cu)
      `(div ([class "assignments upcoming"])
            (h1 "Future")
            ,@(map render-assignment upcoming))
      `(div ([class "assignments past"])
            (h1 "Past")
            ,@(map render-assignment past)))))

  (define (page/student/assignment/files/delete req cu a-id file-to-delete)
    (define assignment (id->assignment a-id))
    (when (< (current-seconds) (assignment-due-secs assignment))
      (define file-path
        (build-path (assignment-file-path cu a-id) file-to-delete))
      (when (file-exists? file-path)
        (delete-file file-path)))
    (redirect-to (main-url page/student/assignment/files cu a-id)))

  (define (page/student/assignment/files req cu a-id)
    (define assignment (id->assignment a-id))
    (define new-file-request
      (send/suspend
       (λ (k-url)
         (define seconds-left
           (- (assignment-due-secs assignment) (current-seconds)))
         (define files (assignment-files cu a-id))
         (define closed? (< seconds-left 0))
         (template
          #:breadcrumb (list (cons "Home" (main-url page/student cu))
                             (cons "Assignments" #f)
                             (cons a-id #f)
                             (cons "Files" #f))
          (side-by-side-render
           cu
           a-id
           (list*
            `(p ([class "notice"])
                ,(format "File Management for ~a ~a" a-id
                         (if closed?
                           "is closed"
                           (format "closes in ~a"
                                   (secs->time-text seconds-left)))))
            (if (empty? files)
              `(p ([class "notice"]) "No files uploaded yet for this assignment")
              `(table ([class "upload-table"])
                      (tr (th "Filename") (th "Delete?"))
                      ,@(map
                         (λ (filename)
                           `(tr (td ,filename)
                                (td ,(if closed?
                                       "X"
                                       `(a ([href ,(main-url
                                                    page/student/assignment/files/delete
                                                    cu a-id
                                                    filename)])
                                           "X")))))
                         files)))
            (list
             `(h1 "Upload a file from your computer:")
             `(form ([action ,(if closed? "#" k-url)]
                     [method "post"]
                     [enctype "multipart/form-data"])
                    (input ([type "file"]
                            [name "new-file"]))
                    nbsp
                    (input ([type "submit"]
                            [value "Upload File"]
                            ,@(if closed?
                                `([disabled "disabled"])
                                `()))))
             `(br)
             `(h1 "Upload plain-text and give it a file name:")
             `(form ([action ,(if closed? "#" k-url)]
                     [method "post"])
                    (p "Filename: "
                       (input ([type "text"]
                               [name "filename"])))
                    (p (textarea ([name "file-content"]
                                  [rows "20"]
                                  [cols "60"])))
                    (p (input ([type "submit"]
                               [value "Add File"]
                               ,@(if closed?
                                   `([disabled "disabled"])
                                   `()))))))))))))
    (define new-file-binding
      (cond
        [(bindings-assq #"new-file"
                        (request-bindings/raw new-file-request))
         => (λ (x) x)]
        [else
         (define bs (request-bindings/raw new-file-request))
         (binding:file
          #"new-file"
          (binding:form-value
           (bindings-assq #"filename" bs))
          empty
          (binding:form-value
           (bindings-assq #"file-content" bs)))]))

    (define file-content (binding:file-content new-file-binding))
    (when (contains-greater-than-80-char-line? file-content)
      (error 'upload-file
             "Cannot upload files with lines greater than 80 characters"))
    (make-directory* (assignment-file-path cu a-id))
    (when (< (current-seconds) (assignment-due-secs assignment))
      (display-to-file #:exists 'replace
                       file-content
                       (build-path (assignment-file-path cu a-id)
                                   (bytes->string/utf-8
                                    (binding:file-filename new-file-binding)))))
    (redirect-to (main-url page/student/assignment/files cu a-id)))

  (define jquery-url
    "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js")

  (define (template #:breadcrumb bc
                    . bodies)
    (response/xexpr
     `(html
       (head (title ,@(add-between (map car bc) " > "))
             (script ([src "/sorttable.js"]) " ")
             (script ([src ,jquery-url]) " ")
             (script ([src "/line-highlight.js"]
                      [type "text/javascript"]) " ")
             (link ([rel "stylesheet"]
                    [type "text/css"]
                    [href "/style.css"])))
       (body
        (div ([class "breadcrumb"])
             ,@(for/list ([b (in-list bc)]
                          [i (in-naturals)])
                 (match-define (cons name url) b)
                 (cond
                   [url
                    `(span (a ([href ,url]) ,name) " / ")]
                   [(= i (sub1 (length bc)))
                    `(span ([class "this"]) ,name)]
                   [else
                    `(span ([class "not-this"]) ,name " / ")]))
             ,(if (current-user)
                `(span ([id "logout"])
                       ,(if (is-admin?)
                          (current-user)
                          `(a ([href ,(main-url page/account)])
                              ,(current-user)))
                       " | "
                       (a ([href ,(main-url page/logout)]) "logout"))
                ""))
        (div ([class "content"])
             ,@bodies)
        ,(footer)))))

  (define (page/student/photo req student)
    (define user-img-path (user-image-path student))
      (define user-email
        (student-email (student-info student)))
      (if (file-exists? user-img-path)
        (response/full
         200 #"Okay"
         (current-seconds) #"image/jpg"
         empty
         (list (file->bytes user-img-path)))
        (redirect-to
         (format "http://www.gravatar.com/avatar/~a?s=160&d=mm"
                 (md5 (string-downcase (string-trim-both user-email)))))))

  (define (user-info-complete? cu)
    (match-define (student nick first last email)
                  (student-info cu))
    (and (not (string=? nick ""))
         (not (string=? first ""))
         (not (string=? last ""))
         (not (string=? email ""))
         (file-exists? (user-image-path cu))))

  (define (student-info u)
    (define p (user-info-path u))
      (if (file-exists? p)
        (file->value p)
        (student "" "" "" "")))
  (define (student-display-name u)
    (match-define (student nick first last _) (student-info u))
    (format "~a \"~a\" ~a (~a)"
            first nick last u))

  (define (page/admin/grade-next req [redirect? #f])
    (unless (is-admin?)
      (send/back
       (template
        #:breadcrumb (list (cons "Professor" (main-url page/root))
                           (cons "Grading" #f))
        "Only the admin can view this page.")))
    ;; XXX CLEANUP Mimic this structure for students self & peer
    (match
        (for*/or ([a (in-list assignments)]
                  [u (in-list (users))]
                  #:when
                  (self-eval-completed? u a)
                  #:unless
                  (prof-eval-completed? u a))
          (cons a u))
      [(cons a u)
       (define id (assignment-id a))
       (define qs (assignment-questions a))
       (define the-info (student-info u))
       (match-define
        (cons q i)
        (for/or ([q (in-list qs)]
                 [i (in-naturals)]
                 #:unless
                 (file-exists?
                  (assignment-question-prof-grade-path u id i)))
          (cons q i)))

       (define ans
         (grade-question
          u u id q i
          #:breadcrumb
          (list (cons "Professor" #f)
                (cons "Students" #f)
                (cons "Grading" #f)
                (cons (student-display-name u) #f)
                (cons id #f))
          #:peer? #t
          #:extra
          (list
           `(div ([class "student-info"])
                 (img ([src ,(main-url page/student/photo u)]
                       [height "160"]))
                 (br)
                 ,(student-display-name u)))))

       (write-to-file*
          ans
          (assignment-question-prof-grade-path u id i))
         (GRADE-CACHE-CLEAR! (assignment-peer u id))

       (page/admin/grade-next req #t)]
      [#f
       (if redirect?
         (redirect-to (main-url page/admin/grade-next))
         (send/back
          (template
           #:breadcrumb
           (list (cons "Professor" #f)
                 (cons "Students" (main-url page/admin/students))
                 (cons "Grading" #f))
           "All grading is done! Great!")))]))

  (define (class-average-table cu)
    (define mins
      (map (λ (u)
             (compute-grade u 0))
           (users)))
    (define max-so-far
      (let ([x (maximum-grade-so-far)])
        (if (zero? x) 1 x)))
    (define so-fars
      (map (λ (min-grade)
             (/ min-grade max-so-far))
           mins))
    (define maxs
      (map (λ (u)
             (compute-grade u 1))
           (users)))
    `(table ([class "class_grades"])
            (tr (th "Minimum Final Grade")
                (th "Grade So Far")
                (th "Maximum Final Grade"))
            ,(cond
               [cu
                (define min-grade (compute-grade cu 0))
                `(tr (td ,(show-grade min-grade))
                     (td ,(show-grade (/ min-grade max-so-far)))
                     (td ,(format-grade cu 1)))]
               [else
                ""])
            (tr (td ,(stat-table mins))
                (td ,(stat-table so-fars))
                (td ,(stat-table maxs)))))

  (define (stat-table l [format? #t])
    (define format
      (if format?
        show-grade
        format-%))
    `(table ([class "grade-stats"])
            (tr (th "Class Min")
                (th "Mean")
                (th "Median")
                (th "Max"))
            (tr (td ,(format (list-min l)))
                (td ,(format (average l)))
                (td ,(format (median l)))
                (td ,(format (list-max l))))))

  (define (list-min l)
    (if (empty? l)
      1
      (apply min l)))
  (define (average l)
    (if (empty? l)
      0
      (/ (apply + l)
         (length l))))
  (define (median l)
    (if (empty? l)
      0
      (list-ref (sort l <)
                (floor (/ (length l) 2)))))
  (define (list-max l)
    (if (empty? l)
      0
      (apply max l)))

  (define (page/admin/students req)
    (unless (is-admin?)
      (page/root req))

    (define max-so-far
      (let ([x (maximum-grade-so-far)])
        (if (zero? x) 1 x)))

    (send/back
     (template
      #:breadcrumb (list (cons "Professor" #f)
                         (cons "Students" #f))
      admin-buttons
      (class-average-table #f)
      `(table ([id "grades"] [class "sortable"])
              (thead
               (tr (th "Student")
                   (th "Min")
                   (th "So Far")
                   (th "Max")
                   (th "Last")
                   (th "Ungraded")))
              (tbody
               ,@(for/list ([u (in-list (sorted-users))])
                   (define min-grade (compute-grade u 0))

                     (define last*
                       (match-lambda
                        [(list) "N/A"]
                        [(? list? l) (last l)]))

                     `(tr
                       (td (a ([href
                                ,(format "mailto:~a"
                                         (student-email
                                          (student-info u)))])
                              "@")
                           " "
                           (a ([href
                                ,(main-url page/student u)])
                              "#")
                           " "
                           ,(student-display-name u))
                       (td ,(show-grade min-grade))
                       (td ,(show-grade (/ min-grade max-so-far)))
                       (td ,(format-grade u 1))
                       (td
                        ,(last*
                          (for/list
                              ([a (in-list assignments)]
                               #:when (self-eval-completed? u a))
                            (assignment-id a))))
                       (td
                        ,@(for/list
                              ([a (in-list assignments)]
                               #:when (self-eval-completed? u a)
                               #:unless (prof-eval-completed? u a))
                            (format "~a " (assignment-id a)))))))))))

  (define admin-buttons
    `(div ([id "grade-button"])
          (a ([href ,(main-url page/admin/students)]) "Students")
          nbsp
          (a ([href ,(main-url page/admin/assignments)]) "Assignments")
          nbsp
          (a ([href ,(main-url page/admin/grade-next)]) "Grade")))

  (define (page/admin/assignments/view req a-id)
    (unless (is-admin?)
      (page/root req))

    (define a (id->assignment a-id))

    (send/back
     (template
      #:breadcrumb
      (list (cons "Professor" #f)
            (cons "Assignments" (main-url page/admin/assignments))
            (cons a-id #f))
      admin-buttons
      `(table ([class "adetails sortable"])
              (thead
               (tr
                (th "#")
                (th "Prompt")
                (th "Min")
                (th "Mean")
                (th "Median")
                (th "Max")
                (th "Deets")))
              (tbody
               ,@(for/list ([q (in-list (assignment-questions a))]
                            [i (in-naturals)])
                   (match-define (question nw ow prompt type) q)

                   (define gs
                     (for/list ([u (in-list (users))]
                                #:when
                                (self-eval-completed? u a))
                       (/ (compute-question-grade u #t 0 a-id i q)
                          (+ nw ow))))
                   (define grade->count
                     (for/fold ([h (hash)])
                         ([g (in-list gs)])
                       (hash-update h g add1 0)))
                   (define deets-l
                     (sort (hash->list grade->count)
                           > #:key cdr))
                   (define deets
                     (for/fold ([d empty])
                         ([g*c (in-list deets-l)])
                       (append d
                               (list
                                `(span ([class "adeets"])
                                       (span ([class "per"])
                                             ,(format-% (car g*c)))
                                       (span ([class "count"])
                                             ,(number->string (cdr g*c))))))))

                   `(tr
                     (td ,(format "~a" i))
                     (td ,prompt
                         " ("
                         ,(format-% (+ nw ow))
                         ")")
                     ,@(rest (fourth (stat-table gs #f)))
                     (td ,@deets))))))))

  (define (page/admin/assignments req)
    (unless (is-admin?)
      (page/root req))

    (define now (current-seconds))

    (send/back
     (template
      #:breadcrumb
      (list (cons "Professor" #f)
            (cons "Assignments" #f))
      admin-buttons
      (class-average-table #f)
      `(table
        ([id "assignments"])
        (thead
         (tr (th "Assignment")
             (th "Turn-in")
             (th "Self-eval")
             (th "Prof-eval")
             (th "Min")
             (th "Mean")
             (th "Median")
             (th "Max")))
        (tbody
         ,@(for/list ([a (in-list assignments)])
             (define a-id (assignment-id a))
             (define turned-in-files
               (for/list ([u (in-list (users))]
                          #:unless
                          (empty?
                           (assignment-files u a-id)))
                 u))
             (define (did-self-eval-completed)
               (for/list ([u (in-list (users))]
                          #:when
                          (self-eval-completed? u a))
                 u))
             (define (did-prof-eval-completed)
               (for/list ([u (in-list (users))]
                          #:when
                          (prof-eval-completed? u a))
                 u))
             (define a-link
               `(a ([href ,(main-url page/admin/assignments/view
                                     a-id)])
                   ,a-id))
             (cond
               [(< now (assignment-due-secs a))
                `(tr (td ,a-link)
                     (td ,(number->string (length turned-in-files)))
                     (td ([colspan "7"]) ""))]
               [(and #f (< now (assignment-eval-secs a)))
                `(tr (td ,a-link)
                     (td ,(number->string (length turned-in-files)))
                     (td ,(number->string
                           (length (did-self-eval-completed))))
                     (td ([colspan "6"]) ""))]
               [else
                (define (normalize g)
                  (define w
                    (+ (assignment-normal-weight a)
                       (assignment-optional-weight a)))
                  (if (zero? w)
                    1
                    (/ g w)))
                (define grades
                  (for/list ([u (in-list (users))]
                             #:when
                             (self-eval-completed? u a))
                    (normalize (compute-assignment-grade u a 0))))
                `(tr
                  (td ,a-link)
                  (td ,(number->string (length turned-in-files)))
                  (td ,(number->string
                        (length (did-self-eval-completed))))
                  (td ,(number->string
                        (length (did-prof-eval-completed))))
                  ,@(rest (fourth (stat-table grades))))])))))))

  (define ((make-prof-eval-completed? assignment-question-prof-grade-path)
           cu a)
    (define id (assignment-id a))
    (define qs (assignment-questions a))
    (for/and ([q (in-list qs)]
              [i (in-naturals)])
      (file-exists?
       (assignment-question-prof-grade-path cu id i))))
  (define peer-eval-completed?
    (make-prof-eval-completed? assignment-question-student-grade-path/peer))
  (define self-eval-completed?
    (make-prof-eval-completed? assignment-question-student-grade-path))
  (define prof-eval-completed?
    (make-prof-eval-completed? assignment-question-prof-grade-path))

  (define current-user (make-parameter #f))
  (define current-user-type (make-parameter #f))

  (define (footer)
    `(div ([id "footer"])
          "Powered by "
          (a ([href "http://racket-lang.org/"]) "Racket") ". "
          "Written by "
          (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy")
          ","
          (a ([href "http://trevoroakes.com/"]) "Trevor Oakes") " and "
          "BYU PLT."
          (br)
          (span ([id "timestamp"])
                ,(date->string (seconds->date (current-seconds)) #t))))

  (define (require-login-then-dispatch req)
    (cond
      [(main-applies? req)
       (define maybe-id (request-id-cookie "name" secret-salt req))
       (match maybe-id
         [#f (page/login req)]
         [(regexp #rx"^(.+):(.+)$" (list _ (app string->symbol kind) id))
          (parameterize ([current-user id]
                         [current-user-type kind])
            (if (or (user-info-complete? id)
                    (is-admin?))
              (main-dispatch req)
              (page/account req)))])]
      [else (next-dispatcher)]))

  (serve/servlet
   require-login-then-dispatch
   #:port port
   #:listen-ip #f
   #:command-line? #t
   #:ssl? #t
   #:ssl-cert (build-path db-path 'up "server-cert.pem")
   #:ssl-key (build-path db-path 'up "private-key.pem")
   #:quit? #f
   #:launch-browser? #f
   #:extra-files-paths (list (build-path source-dir "static"))
   #:servlet-regexp #rx""
   #:servlet-path "/"))

(provide/contract
 [samurai-go!
  (-> #:db path-string?
      #:closed? boolean?
      #:port port-number?
      #:assignments (listof assignment?)
      #:authenticate (-> string? string? (or/c 'admin 'user #f))
      #:professor-email string?
      #:username-request-text string?
      #:password-request-text string?
      void)])
