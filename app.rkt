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
         "../m8b/id-cookie.rkt")

(define DEBUG? #t)

;; XXX TODO There are bunch of "simple" display XXXs in the code, such
;; as display the student's username and photo in the admin
;; grading. T-bone, please deal with those.

;; XXX TODO Style
;; XXX TODO Fix bread crumbs in template calls

;; XXX TODO Ask questions simultaneously and/or have better keyboarding
;; XXX TODO Allowing comments on self-eval answers after admin

;; XXX TODO find uses of real->decimal string and unify to one
;; function for displaying a %

;; XXX TODO Enforcing optional-enable
;; XXX TODO Dealing with your-split (wlang1/wlang2)

(define (string->lines s)
  (string-split s "\n"))

(define (contains-greater-than-80-char-line? file-content)
  (for/or ([l (in-list (string->lines (bytes->string/utf-8 file-content)))])
    ((string-length l) . >= . 80)))

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

(define (make-parent-directory* p)
  (define parent (path-only p))
  (make-directory* parent))
(define (display-to-file* v pth)
  (make-parent-directory* pth)
  (display-to-file v pth #:exists 'replace))
(define (write-to-file* v pth)
  (make-parent-directory* pth)
  (write-to-file v pth #:exists 'replace))

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
      (map path->last-part (directory-list pth))
      empty))

(define-runtime-path source-dir ".")

(define (samurai-go!
         #:db db-path
         #:port port
         #:assignments pre-assignments
         #:authenticate authenticate-users
         #:username-request-text login-formlet-un-text
         #:password-request-text login-formlet-pw-text)

  (define assignments
    (sort pre-assignments
          <=
          #:key assignment-due-secs))

  (define secret-salt-path (build-path db-path "secret-salt"))

  (define (id->assignment a-id)
    (findf (λ (a) (string=? a-id (assignment-id a))) assignments))

  (define secret-salt
    (begin
      (unless (file-exists? secret-salt-path)
        (display-to-file*
         (list->bytes (build-list 128 (λ (i) (random 256))))
         secret-salt-path))
      (file->bytes secret-salt-path)))

  (define (is-admin?)
    (eq? 'admin (current-user-type)))

  (define (page/root req)
    (send/back
     (if (is-admin?)
       (redirect-to (main-url page/admin))
       (redirect-to (main-url page/main)))))

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
          #:breadcrumb (list (cons "Login" #f))
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
       (redirect-to (parameterize ([current-user username]
                                   [current-user-type authenticated?])
                      (if (or (file-exists? (user-info-path))
                              (is-admin?))
                        (main-url page/root)
                        (main-url page/account)))
                   #:headers
                   (list (cookie->header
                          (make-id-cookie secret-salt
                                          (format "~a:~a"
                                                  authenticated?
                                                  username)))))]
      [else (page/login req (format "Invalid password for user (~S)" username))]))


  (define (default-text-input default-string)
    (to-string (default (string->bytes/utf-8 default-string)
                 (text-input #:value (string->bytes/utf-8 default-string)))))

  (define (page/account req)
    (define existing-info
      (cond
        [(file-exists? (user-info-path))
         (file->value (user-info-path))]
        [else (student "" "" "" "")]))

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
          #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Account Admin" #f))
          `(div ([id "account"])
                (h1 ,(format "Account Admin for ~a" (current-user)))
                (form ([action ,k-url]
                       [method "post"]
                       [enctype "multipart/form-data"])
                      ,@(formlet-display account-formlet)
                      (p "Instead of this: "
                         (img
                          ([src ,(main-url page/student/photo (current-user))]
                           [width "160"] [height "160"])))
                      (input ([type "submit"] [value "Update Info"]))))))))

    (define-values (first-name last-name nick-name email photo)
      (formlet-process account-formlet account-form))

    (write-to-file* (student nick-name first-name last-name email)
                    (user-info-path))
    (when (binding:file? photo)
      (display-to-file* (binding:file-content photo)
                        (user-image-path)))

    (redirect-to (main-url page/root)))

  (define (page/student req student)
    (cond
      [(or (is-admin?)
           (string=? (current-user) student))
       (send/back
        (template
         #:breadcrumb (list (cons "Home" (main-url page/root)) (cons student #f))
         `(h1 "Student Page for " ,student)))]
      [else
       (redirect-to (main-url page/root))]))

  (define-values (main-dispatch main-url main-applies?)
    (dispatch-rules+applies
     [("")
      page/root]
     [("main")
      page/main]
     [("admin")
      page/admin]
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
     [("assignment" (string-arg) "files" "edit")
      page/assignment/files/edit]
     [("assignment" (string-arg) "files" "delete" (string-arg))
      page/assignment/files/delete]
     [("assignment" (string-arg) "files") 
      page/assignment/files]
     [("assignment" (string-arg) "self" "edit")
      page/assignment/self/edit]
     [("assignment" (string-arg) "self")
      page/assignment/self]
     [("assignment" (string-arg) "peer" "edit")
      page/assignment/peer/edit]
     [("assignment" (string-arg) "peer")
      page/assignment/peer]))

  (define (page/assignment/files req a-id)
    (template
     #:breadcrumb (list (cons "Home" (main-url page/root)) (cons "View Files" #f))
     (assignment-file-display a-id)))  

  (define default-peer
    "The Spanish Inquisition")
  (define (assignment-peer id)
    (if (file-exists? (assignment-peer-path id))
      (file->string (assignment-peer-path id))
      default-peer))
  (define (assignment-co-peer id)
    (or (for/or ([u (in-list (users))])
          (and (equal? id
                       (parameterize ([current-user u])
                         (assignment-peer id)))
               u))
        default-peer))  

  (define (users-path)
    (build-path db-path "users"))
  (define (users)
    (directory-list* (users-path)))

  (define (user-path)
    (build-path (users-path) (current-user)))
  (define (user-info-path)
    (build-path (user-path) "info.rktd"))
  (define (user-image-path)
    (build-path (user-path) "photo.jpg"))
  (define (assignment-path id)
    (build-path (user-path) "assignments" id))
  (define (assignment-file-path id)
    (build-path (assignment-path id) "files"))
  (define (assignment-files id)
    (directory-list* (assignment-file-path id)))
  (define (assignment-peer-path id)
    (build-path (assignment-path id) "peer"))
  (define (assignment-question-student-grade-path id i)
    (build-path (assignment-path id) "self-eval" (number->string i)))
  (define (assignment-question-student-grade-path/peer id i)
    (build-path (assignment-path id) "peer-eval" (number->string i)))
  (define (assignment-question-prof-grade-path id i)
    (build-path (assignment-path id) "prof-eval" (number->string i)))

  (define ((make-assignment-question-student-grade
            assignment-question-student-grade-path)
           id i)
    (define p (assignment-question-student-grade-path id i))
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

  (define (assignment-question-peer-grade id i)
    (define co-peer (assignment-co-peer id))
    (parameterize ([current-user co-peer])
      (assignment-question-student-grade/peer id i)))

  ;; XXX cleanup this
  (define (assignment-question-student-bool-grade id i)
    (define v (assignment-question-student-grade id i))
    (if v
      (answer:bool-value v)
      'n/a))
  (define (assignment-question-student-bool-grade/peer id i)
    (define v (assignment-question-student-grade/peer id i))
    (if v
      (answer:bool-value v)
      'n/a))
  (define (assignment-question-student-numeric-grade/peer id i)
    (define v (assignment-question-student-grade/peer id i))
    (if v
      (answer:numeric-value v)
      #f))

  (define (assignment-question-prof-bool-grade id i)
    (define v (assignment-question-prof-grade id i))
    (if v
      (answer:bool-value v)
      'n/a))
  (define (assignment-question-prof-bool-grade/peer id i)
    (define peer (assignment-peer id))
    (parameterize ([current-user peer])
      (assignment-question-prof-bool-grade id i)))

  (define (assignment-question-prof-numeric-grade id i)
    (define v (assignment-question-prof-grade id i))
    (if v
      (answer:numeric-value v)
      #f))
  (define (assignment-question-prof-numeric-grade/peer id i)
    (define peer (assignment-peer id))
    (parameterize ([current-user peer])
      (assignment-question-prof-numeric-grade id i)))

  (define (compute-question-grade optional? default-grade id i q)
    (match-define (question nw ow _ t) q)
    (define ow-p (if optional? ow 0))
    (define ps
      (match t
        ['numeric
         (or (assignment-question-prof-numeric-grade id i)
             default-grade)]
        ['bool
         (define student-correct?
           (assignment-question-student-bool-grade id i))
         (define prof-correct?
           (assignment-question-prof-bool-grade id i))
         (match* (student-correct? prof-correct?)
           [(  #t   #t) 10/10]
           [(  #t   #f) -1/10]
           [(  #f   #t)  1/10]
           [(  #f   #f)  0/10]
           [('n/a    _) default-grade]
           [(   _ 'n/a) default-grade])]))
    (* (+ nw ow-p) ps))

  (define (compute-peer-grade optional? default-grade id i q)
    (match-define (question nw ow _ t) q)
    (define ow-p (if optional? ow 0))
    (define ps
      (match t
        ['numeric
         (define prof
           (assignment-question-prof-numeric-grade/peer id i))
         (define student
           (assignment-question-student-numeric-grade/peer id i))
         (if (and prof student)
           (- 1 (abs (- prof student)))
           default-grade)]
        ['bool
         (define prof (assignment-question-prof-bool-grade/peer id i))
         (define student (assignment-question-student-bool-grade/peer id i))
         (cond
           [(or (eq? 'n/a prof)
                (eq? 'n/a student))
            default-grade]
           [(equal? prof student)
            1]
           [else
            0])]))
    (* (+ nw ow-p) ps))

  (define ((make-compute-question-grades compute-question-grade)
           optional? default-grade id qs)
    (for/sum
     ([q (in-list qs)]
      [i (in-naturals)])
     (compute-question-grade optional? default-grade id i q)))

  (define compute-question-grades
    (make-compute-question-grades compute-question-grade))
  (define compute-peer-grades
    (make-compute-question-grades compute-peer-grade))

  (define (compute-grade default-grade)
    (for/sum
     ([a (in-list assignments)])
     (match-define (assignment nw ow id ds es ps qs) a)
     (define self-pts
       (compute-question-grades
        ;; XXX incorporate optional-enable
        #t default-grade
        id qs))
     (define peer-pts
       (compute-peer-grades
        #t default-grade
        id qs))
     (if (number? ps)
       (* (+ ow nw)
          (+ (* 9/10 self-pts)
             (* 1/10 peer-pts)))
       (* (+ ow nw) self-pts))))

  (define (assignment-file-display a-id)
    `(div ([id "files"])
          ,@(map
             (λ(file)
               ((curry file->html-table a-id)
                (build-path (assignment-file-path a-id) file)))
             (assignment-files a-id))))

  (define (format-grade default-grade)
    (define g (compute-grade default-grade))
    `(span ([class ,(substring (letter-grade g) 0 1)])
           ,(format "~a% (~a)"
                    (real->decimal-string (* 100 g) 4)
                    (letter-grade g))))

  (define boolean-formlet
    (formlet
     (p ,{(radio-group '(#t #f)
                       #:checked? (λ (x) x)
                       #:display (λ (x) (if x "Yes" "No")))
          . => . credit})
     credit))
  (define numeric-formlet
    (formlet
     (p ,{(to-number input-string) . => . percent}
        "(Enter a number between 0 and 1)")
     percent))

  (define evidence-formlet
    (formlet
     (div (p "Provide evidence to justify that score.")
          ,{(to-string (required (textarea-input #:rows 8 #:cols 80))) . => . comment}
          (p "(If you need to refer to line numbers, prefix a number with L. For example, use L32 or l32 to refer to line 32)"))
     comment))

  (define (page/assignment/self/edit req a-id)
    (define assignment (id->assignment a-id))
    (define (ask-question q)
      (define question-formlet
        (formlet
         (div
          ,{(match (question-type q)
              ['bool boolean-formlet]
              ['numeric numeric-formlet])
            . => . score}
          ,{evidence-formlet . => . comment})
         (values score comment)))
      (define-values (score explanation)
        (formlet-process
         question-formlet
         (send/suspend
          (λ (k-url)
            (template
             #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Self Evaluation" #f))
             `(div
               (table
                (tr
                 (td
                  ,(assignment-file-display a-id))
                 (td
                  (p ,(question-prompt q))
                  (form ([action ,k-url] [method "post"])
                        ,@(formlet-display question-formlet)
                        (input ([type "submit"] [value "Submit"]))))))))))))
      ((match (question-type q)
         ['bool
          answer:bool]
         ['numeric
          answer:numeric])
       (current-seconds) explanation
       score))

    (define (overdue-or thunk)
      (if (<= (assignment-eval-secs assignment) (current-seconds))
        (send/back
         (template
          #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Self Evaluation" #f))
          "Self evaluation past due."))
        (thunk)))

    (overdue-or
     (λ ()
       (for ([question (assignment-questions assignment)]
             [i (in-naturals)])
         (unless
             (file-exists? (assignment-question-student-grade-path a-id i))
           (define answer (ask-question question))
           (overdue-or
            (λ ()
              (write-to-file*
               answer
               (assignment-question-student-grade-path a-id i))))))

       (template
        #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Self Evaluation" #f))
        "Self evaluation completed."))))

  (define (format-answer which ans)
    (cond
      [ans
       `(div ([class ,(format "answer ~a" which)])
             (p ,(format "The ~a evaluation is: ~a"
                         which
                         (match ans
                           [(answer:bool _ _ completed?)
                            (if completed?
                              "Yes"
                              "No")]
                           [(answer:numeric _ _ value)
                            (real->decimal-string value 4)])))
             ;; XXX add line links
             (pre ,(answer-comments ans)))]
      [else
       `(div ([class ,(format "answer incomplete ~a" which)])
             (p ,(format "Your ~a evaluation is not completed." which)))]))

  (define (page/assignment/self req a-id)
    (define assignment (id->assignment a-id))
    (template
     #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Self Evaluation" #f))
     `(div
       (table
        (tr
         (td
          ,(assignment-file-display a-id))
         (td
          ,@(for/list ([q (in-list (assignment-questions assignment))]
                       [i (in-naturals)])
              (match-define (question nw ow prompt type) q)
              `(div (p (span ([class "weight"])
                             ;; XXX incorporate optional-enable
                             ,(real->decimal-string (+ nw ow) 4))
                       ,prompt)
                    ,(format-answer
                      "Self"
                      (assignment-question-student-grade a-id i))
                    ,(format-answer
                      "Professor"
                      (assignment-question-prof-grade a-id i))
                    ,(format-answer
                      "Peer"
                      (assignment-question-peer-grade a-id i))))))))))

  ;; XXX abstract this and above
  (define (page/assignment/peer req a-id)
    (define assignment (id->assignment a-id))
    (define peer (assignment-peer a-id))
    (if (equal? default-peer peer)
      (template
       #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Peer Evaluation" #f))
       `(div "Your peer has not been assigned."))
      (template
       #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Peer Evaluation" #f))
       `(div
         (table
          (tr
           (td
            ,(parameterize ([current-user peer])
               (assignment-file-display a-id)))
           (td
            ,@(for/list ([q (in-list (assignment-questions assignment))]
                         [i (in-naturals)])
                (match-define (question nw ow prompt type) q)
                `(div
                  (p (span ([class "weight"])
                           ;; XXX incorporate optional-enable
                           ,(real->decimal-string (+ nw ow) 4))
                     ,prompt)
                  ,(format-answer
                    "Peer's Self"
                    (parameterize ([current-user peer])
                      (assignment-question-student-grade a-id i)))
                  ,(format-answer
                    "Peer's Professor"
                    (parameterize ([current-user peer])
                      (assignment-question-prof-grade a-id i)))
                  ,(format-answer
                    "Peer"
                    (assignment-question-student-grade/peer a-id i)))))))))))

  (define (page/assignment/peer/edit req a-id)
    (define assignment (id->assignment a-id))
    (define (pick-a-person)
      (define student-ids (users))
      (define finished-self-eval
        (map
         car
         (filter
          (λ (u-f)(file-exists? (cdr u-f)))
          (map
           (λ (un)
             (cons un
                   (assignment-question-student-grade-path a-id 0)))
           student-ids))))
      (define already-assigned
        (map
         (λ (u-f) (file->value (car u-f)))
         (filter (λ (u-f)(file-exists? (cdr u-f)))
                 (map (λ (un) (cons un (assignment-peer-path a-id)))
                      student-ids))))
      (define candidates
        (remove (current-user)
                (remove* already-assigned finished-self-eval)))
      (match (shuffle candidates)
        [(list* peer _)
         (display-to-file* peer (assignment-peer-path a-id))
         peer]
        [(list)
         (send/back
          (template
           #:breadcrumb (list (cons "Home" (main-url page/main)) 
                              (cons "Peer Evaluation" #f))
           "Peer evaluation is impossible, as no peers are available."))]))

    (define peer-id
      (if (file-exists? (assignment-peer-path a-id))
        (assignment-peer a-id)
        (pick-a-person)))

    (define (overdue-or thunk)
      (if (<= (assignment-peer-secs assignment) (current-seconds))
        (send/back
         (template
          #:breadcrumb (list (cons "Home" (main-url page/main)) 
                             (cons "Peer Evaluation" #f))
          "Peer evaluation past due."))
        (thunk)))

    (overdue-or
     (λ ()
       (for ([question (assignment-questions assignment)]
             [i (in-naturals)])
         (unless
             (or
              (file-exists?
               (assignment-question-student-grade-path/peer a-id i))
              (not
               (file-exists?
                (parameterize ([current-user peer-id])
                  (assignment-question-student-grade-path a-id i)))))
           (define grade
             (grade-question
              peer-id a-id question
              (parameterize ([current-user peer-id])
                (assignment-question-student-grade a-id i))))
           (overdue-or
            (λ ()
              (write-to-file*
               grade
               (assignment-question-student-grade-path/peer a-id i))))))

       (template
        #:breadcrumb (list (cons "Home" (main-url page/main)) 
                           (cons "Peer Evaluation" #f))
        "Peer evaluation completed."))))

  (define (grade-question stu a-id question q-self-eval)
    (define score-formlet
      (match (question-type question)
        ['numeric
         numeric-formlet]
        ['bool
         boolean-formlet]))
    (define the-formlet
      (formlet
       (div
        ,(format-answer "Their" q-self-eval)
        (p "What do you think they earned?")
        ,{score-formlet . => . peer-score}
        ,{evidence-formlet . => . comment})
       (values peer-score comment)))
    (define-values (score comment)
      (formlet-process
       the-formlet
       (send/suspend
        (λ (k-url)
          (template
           #:breadcrumb (list (cons "Home" (main-url page/root)) 
                              (if (is-admin?)
                                  (cons "Grade" #f)
                                  (cons "Self Evaluation" #f)))
           `(div
             (table
              (tr
               (td
                ,(parameterize ([current-user stu])
                   (assignment-file-display a-id)))
               (td
                (p ,(question-prompt question))
                (form ([action ,k-url] [method "post"])
                      ,@(formlet-display the-formlet)
                      (input ([type "submit"] [value "Submit"]))))))))))))
    ((match (question-type question)
       ['bool
        answer:bool]
       ['numeric
        answer:numeric])
     (current-seconds) comment
     score))

  (define (file->html-table a-id file)
    (define file-lines
      (string-split (bytes->string/utf-8 (file->bytes file))
                    #px"\r\n?|\n"
                    #:trim? #f))
    (define (line->line-content-div line line-num)
      `(div ([id ,(format "~aLC~a" file line-num)][class "line"])
            ,((λ (l)(if (string=? "" l) '(br) l)) line)))

    `(div ([class "file"])
          (div ([class "meta"]) ,(format "~a" (path->last-part file)))
          (div ([class "data type-text"])
               (table ([class "lines"][cellspacing "0"][cellpadding "0"])
                      (tbody
                       (tr
                        (td
                         (pre ([class "line_numbers"]
                               [style "margin: 0pt; padding-right: 10px;"])
                              ,@(map
                                 (λ (n)
                                   `(span ([id ,(format "~aL~a" file n)]
                                           [rel ,(format "#~aL~a" file n)])
                                          ,(number->string n) (br)))
                                 (build-list (length file-lines) add1))))
                        (td ([width "100%"])
                            (div ([class "highlight"])
                                 (pre
                                  ,@(map line->line-content-div file-lines
                                         (build-list (length file-lines)
                                                     add1)))))))))))

  (define (page/logout req)
    (redirect-to
     (main-url page/root)
     #:headers
     (list (cookie->header logout-id-cookie))))

  (define (secs->time-text secs)
      (define s (if (secs . <= . 1) 1 secs))
      (if (s . < . 0) (set! s 1) void)
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
  
  (define (page/main req)
    (when (is-admin?)
      (page/root req))
    (define a-day (* 60 60 24))
    (define 2-days (* a-day 2))
    (define (cond-hyperlink available closed text1 link1 text2 link2)
      (cond
        [(or (not available) (not closed))
         ""]
        [DEBUG?
         `(p (a ([href ,link1]) ,text1) (br)
             (a ([href ,link2]) ,text2))]
        [(< (current-seconds) available)
         text1]
        [(> (current-seconds) closed)
         `(a ([href ,link2]) ,text2)]
        [else
         `(a ([href ,link1]) ,text1)]))
    ;; TODO base off which phases they can still do
    (define-values (upcoming past)
      (partition
       (λ (a) (((assignment-due-secs a) . + . 2-days)
               . > . (current-seconds)))
       (sort assignments < #:key assignment-due-secs)))
   
    ;; TODO render offline assignments (like the final) differently
    (define (render-assignment a)
      (define next-due
        (cond
          [(peer-eval-completed? a)
           #f]
          [(self-eval-completed? a)
           (assignment-peer-secs a)]
          [(> (current-seconds) (assignment-due-secs a))
           (assignment-eval-secs a)]
          [else
           (assignment-due-secs a)]))

      `(table ([class ,(cond
                         [(not (zero? (assignment-optional-weight a)))
                          "optional"]
                         [(not (zero? (assignment-normal-weight a)))
                          "normal"]
                         [else
                          "optenable"])])
              (tr (td ,(assignment-id a))
                  (td ,(real->decimal-string
                        (* 100
                           (+ (assignment-normal-weight a)
                              (assignment-optional-weight a)))
                        4)
                      "%")
                  (td ,(if next-due
                         (format
                          "Due ~a in ~a"
                          (date->string (seconds->date next-due))
                          (secs->time-text (- next-due (current-seconds))))
                         "Completed")))
              (tr (td ,(cond-hyperlink
                        (current-seconds) (assignment-due-secs a)
                        "Turn in Files"
                        (main-url page/assignment/files/edit
                                  (assignment-id a))
                        "View Files"
                        (main-url page/assignment/files (assignment-id a))))
                  (td ,(cond-hyperlink
                        (assignment-due-secs a) (assignment-eval-secs a)
                        "Self Evaluation"
                        (main-url page/assignment/self/edit
                                  (assignment-id a))
                        "Self Evaluation Details"
                        (main-url page/assignment/self
                                  (assignment-id a))))
                  (td ,(cond-hyperlink
                        (assignment-eval-secs a) (assignment-peer-secs a)
                        "Grade a Peer"
                        (main-url page/assignment/peer/edit
                                  (assignment-id a))
                        "Grade a Peer Details"
                        (main-url page/assignment/peer
                                  (assignment-id a)))))))
    (send/back
     (template
      #:breadcrumb (list (cons "Student Main Page" #f))
      ;; TODO
      `(div ([id "class-score"])
            (table (tr (th "Mininum Final Grade")
                       (th "Maximum Final Grade"))
                   (tr (td ,(format-grade 0))
                       (td ,(format-grade 1)))))
      `(div ([id "upcoming-assignments"])
            ,@(map render-assignment upcoming))
      `(div ([id "past-assignments"])
            ,@(map render-assignment past)))))

  (define (page/assignment/files/delete req a-id file-to-delete)
    (define assignment
      (findf (λ (a) (string=? a-id (assignment-id a))) assignments))
    (when (< (current-seconds) (assignment-due-secs assignment))
      (define file-path
        (build-path (assignment-file-path a-id) file-to-delete))
      (when (file-exists? file-path)
        (delete-file file-path)))
    (redirect-to (main-url page/assignment/files/edit a-id)))

  (define (page/assignment/files/edit req a-id)
    (define assignment
      (findf (λ (a) (string=? a-id (assignment-id a))) assignments))
    (define (extract-binding:file req)
      (bindings-assq #"new-file" (request-bindings/raw req)))
    (define new-file-binding
      (extract-binding:file
       (send/suspend
        (λ (k-url)
          (define seconds-left
            (- (assignment-due-secs assignment) (current-seconds)))
          (template
           #:breadcrumb (list (cons "Home" (main-url page/main)) 
                                    (cons (format "Manage Files - ~a" a-id) #f))
           ;; XXX use standard time duration display
           `(p ,(format "File Management for ~a ~a" a-id
                        (if (seconds-left . < . 0)
                          "is closed"
                          (format "closes in ~a" (secs->time-text seconds-left)))))
           `(table
             (tr (th "Filename") (th "Delete?"))
             ,@(map
                (λ (file-path)
                  `(tr (td ,(path->string file-path))
                       (td (a ([href ,(main-url 
                                       page/assignment/files/delete a-id
                                       (path->string file-path))])
                              "X"))))
                (assignment-files a-id)))
           ;; XXX Add a textarea box
           `(form ([action ,k-url]
                   [method "post"]
                   [enctype "multipart/form-data"])
                  (table
                   (tr (td (input ([type "file"]
                                   [name "new-file"])))
                       (td (input ([type "submit"]
                                   [value "Add File"])))))))))))
    (define file-content (binding:file-content new-file-binding))
    (when (contains-greater-than-80-char-line? file-content)
      (error 'upload-file
             "Cannot upload files with lines greater than 80 characters"))
    (when (< (current-seconds) (assignment-due-secs assignment))
      (display-to-file
       file-content
       (build-path (assignment-file-path a-id)
                   (bytes->string/utf-8
                    (binding:file-filename new-file-binding)))))
    (redirect-to (main-url page/assignment/files/edit a-id)))

  (define (template #:breadcrumb bc
                    . bodies)
    (response/xexpr
     `(html (head (title ,@(add-between (map car bc) " > "))
                  #;(script ([src "/sorttable.js"]) " ")
                  (link ([rel "stylesheet"]
                         [type "text/css"]
                         [href "/style.css"])))
            (body
             (div ([class "breadcrumb"])
                  ,@(for/list ([b (in-list bc)])
                      (match-define (cons name url) b)
                      (if url
                        `(span (a ([href ,url]) ,name) " / ")
                        `(span ([class "this"]) ,name)))
                  ,(if (current-user)
                     `(span ([id "logout"])
                            ,(current-user) " | "
                            (a ([href ,(main-url page/logout)]) "logout"))
                     ""))
             (div ([class "content"])
                  ,@bodies
                  ,(footer))))))

  (define (page/student/photo req student)
    (parameterize ([current-user student])
      (define user-img-path (user-image-path))
      (define user-inf-path (user-info-path))
      (define user-email
        (if (file-exists? user-inf-path)
            (student-email (file->value user-inf-path))
            ""))
      (if (file-exists? user-img-path)
          (response/full
           200 #"Okay"
           (current-seconds) #"image/jpg"
           empty
           (list (file->bytes user-img-path)))
          (redirect-to
           (format "http://www.gravatar.com/avatar/~a?s=160&d=mm"
                   (md5 (string-downcase (string-trim-both user-email))))))))

  (define (page/admin/grade-next req)
    (unless (is-admin?)
      (send/back
       (template
        #:breadcrumb (list (cons "Home" (main-url page/main)) (cons "Grade Next" #f))
        "Only the admin can view this page.")))
    ;; XXX Mimic this structure for students self & peer
    (match
        (for*/or ([a (in-list assignments)]
                  [u (in-list (users))]
                  #:when 
                  (parameterize ([current-user u])
                    (self-eval-completed? a))
                  #:unless
                  (parameterize ([current-user u])
                    (prof-eval-completed? a)))
          (cons a u))
      [(cons a u)
       (parameterize ([current-user u])
         (define id (assignment-id a))
         (define qs (assignment-questions a))
         (define student-info (file->value (user-info-path)))
         (match-define
          (cons q i)
          (for/or ([q (in-list qs)]
                   [i (in-naturals)]
                   #:unless
                   (file-exists?
                    (assignment-question-prof-grade-path id i)))
            (cons q i)))
         
         (define score-formlet
           (match (question-type q)
             ['numeric numeric-formlet]
             ['bool boolean-formlet]))
         (define the-formlet
           (formlet
            (div
             ,(format-answer 
               "Their"
               (assignment-question-student-grade id i))
             ,(format-answer
               "Peer"
               (assignment-question-peer-grade id i))
             (p "What do you think they earned?")
             ,{score-formlet . => . prof-score}
             ,{evidence-formlet . => . comment})
            (values prof-score comment)))

         (define-values (score comment)
           (formlet-process
            the-formlet
            (send/suspend
             (λ (k-url)
               ;; XXX displays the current user wrong because of the
               ;; parameterize above
               (template
                #:breadcrumb (list (cons "Admin" (main-url page/admin)) (cons "Grade" #f))
                `(div
                  (img ([src ,(main-url page/student/photo (current-user))]
                           [width "80"] [height "80"]))
                  (p ,(format "~a ~a" 
                              (student-nickname student-info)
                              (student-lastname student-info)))
                  (table
                   (tr
                    (td
                     ,(assignment-file-display id))
                    (td
                     (p ,(question-prompt q))
                     (form 
                      ([action ,k-url] [method "post"])
                      ,@(formlet-display the-formlet)
                      (input ([type "submit"] [value "Submit"]))))))))))))
         (define ans
           ((match (question-type q)
              ['bool
               answer:bool]
              ['numeric
               answer:numeric])
            (current-seconds) comment
            score))

         (write-to-file*
          ans
          (assignment-question-prof-grade-path id i))

         (redirect-to
          (main-url page/admin/grade-next)))]
      [#f
       (send/back
        (template
         #:breadcrumb (list (cons "Home" (main-url page/admin)) 
                            (cons "Grade Next" #f))
         "All grading is done! Great!"))]))

  (define (page/admin req)
    (unless (is-admin?)
      (page/root req))

    (send/back
     (template
      #:breadcrumb (list (cons "Home" #f))
      `(a ([href ,(main-url page/admin/grade-next)]) "Grade")
      `(table
        (thead
         (tr (th "User")
             (th "Min")
             (th "Max")
             (th "Ungraded")))
        (tbody
         ,@(for/list ([u (in-list (users))])
             (parameterize ([current-user u])
               `(tr (td ,u)
                    (td ,(format-grade 0))
                    (td ,(format-grade 1))
                    (td
                     ,@(for/list
                           ([a (in-list assignments)]
                            #:when (self-eval-completed? a)
                            #:unless (prof-eval-completed? a))
                         (format "~a " (assignment-id a))))))))))))

  (define ((make-prof-eval-completed? assignment-question-prof-grade-path)
           a)
    (define id (assignment-id a))
    (define qs (assignment-questions a))
    (for/and ([q (in-list qs)]
              [i (in-naturals)])
      (file-exists?
       (assignment-question-prof-grade-path id i))))
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
          (a ([href "http://trevoroakes.com/"]) "Trevor Oakes") " and "
          (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy") ". "))

  (define (tabs header . the-tabs)
    (define found-selected? #f)
    (define tab-seq
      (build-vector
       (/ (length the-tabs) 2)
       (lambda (i)
         (define id (symbol->string (gensym)))
         (define label (list-ref the-tabs (* 2 i)))
         (define body (list-ref the-tabs (add1 (* 2 i))))
         (define no-content?
           (and (string? body)
                (string=? "" body)))
         (define selected?
           (and (not found-selected?)
                (not no-content?)))
         (when selected?
           (set! found-selected? #t))
         (vector id selected? no-content? label body))))
    `(div
      ([class "tabbed"])
      (div
       ([class "tab-header"])
       (div ([class "tab-uheader"]) ,header)
       (ul
        ,@(for/list ([v (in-vector tab-seq)])
            (match-define
             (vector id selected? no-content? label body)
             v)
            (define direct-link
              (match body
                [(cons #f url) url]
                [_ #f]))
            `(li ([id ,(format "li~a" id)]
                  ,@(if selected? `([class "tab-selected"]) empty))
                 ,(cond
                    [no-content?
                     label]
                    [direct-link
                     `(a ([href ,direct-link]) ,label)]
                    [else
                     `(a ([href
                           ,(format
                             "javascript:~a~a;"
                             (for/fold ([s ""])
                                 ([v (in-vector tab-seq)])
                               (match-define
                                (vector id selected? no-content? label _) v)
                               (format "ToggleOff(~S);~a" id s))
                             (format "ToggleOn(~S)" id))])
                         ,label)])))))
      ,@(for/list ([v (in-vector tab-seq)])
          (match-define (vector id selected? no-content? _ body) v)
          (define direct-link
            (match body
              [(cons #f url) url]
              [_ #f]))
          `(div ([id ,id]
                 [style ,(if selected?
                           "display: block"
                           "display: none")]
                 [class "tab-content"])
                ,(if direct-link "" body)))))

  (define (require-login-then-dispatch req)
    (cond
      [(main-applies? req)
       (define maybe-id (request-valid-id-cookie secret-salt req))
       (match maybe-id
         [#f (page/login req)]
         [(regexp #rx"^(.+):(.+)$" (list _ (app string->symbol kind) id))
          (parameterize ([current-user id]
                         [current-user-type kind])
            (main-dispatch req))])]
      [else (next-dispatcher)]))

  (serve/servlet
   require-login-then-dispatch
   #:port port
   #:listen-ip #f
   #:command-line? #t
   #:quit? #f
   #:launch-browser? #f
   #:extra-files-paths (list (build-path source-dir "static"))
   #:servlet-regexp #rx""
   #:servlet-path "/"))

(provide/contract
 [samurai-go!
  (-> #:db path-string?
      #:port port-number?
      #:assignments (listof assignment?)
      #:authenticate (-> string? string? (or/c 'admin 'user #f))
      #:username-request-text string?
      #:password-request-text string?
      void)])
