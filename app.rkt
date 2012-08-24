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

;; XXX Style - better colors for grades
;; XXX style - make the question follow the scroll bar when looking at the code

;; XXX TODO Ask questions simultaneously and/or have better keyboarding
;; XXX TODO Allowing comments on self-eval answers after admin

;; XXX TODO Enforcing optional-enable
;; XXX TODO Dealing with your-split (wlang1/wlang2)

;; XXX TODO Adding a file without browsing first throws exception
;; XXX TODO File with lines > 80 characters throws exception, should be an actual error page

(define (format-% v)
  (format "~a%" (real->decimal-string (* 100 v) 2)))

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
      (sort (map path->last-part (directory-list pth))
            string-ci<=?)
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
       (redirect-to (parameterize ([current-user username]
                                   [current-user-type authenticated?])
                      (main-url page/root))
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
          #:breadcrumb (list (cons "Home" (main-url page/main))
                             (cons (current-user) #f)
                             (cons "Details" #f))
          `(div ([id "account"])
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
                    (user-info-path))
    (when (binding:file? photo)
      (display-to-file* (binding:file-content photo)
                        (user-image-path)))

    (redirect-to (main-url page/root)))  

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
     [("student" (string-arg) "photo.jpg")
      page/student/photo]
     [("assignment" (string-arg) "files")
      page/assignment/files]
     [("assignment" (string-arg) "files" "delete" (string-arg))
      page/assignment/files/delete]
     [("assignment" (string-arg) "self" "edit")
      page/assignment/self/edit]
     [("assignment" (string-arg) "self")
      page/assignment/self]
     [("assignment" (string-arg) "peer" "edit")
      page/assignment/peer/edit]
     [("assignment" (string-arg) "peer")
      page/assignment/peer]))

  (define default-peer
    "The Spanish Inquisition")
  (define (assignment-peer id)
    (if (file-exists? (assignment-peer-path id))
      (file->string (assignment-peer-path id))
      default-peer))
  (define (assignment-co-peer id)
    (or (for/or ([u (in-list (users))])
          (and (equal? (current-user)
                       (parameterize ([current-user u])
                         (assignment-peer id)))
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

  (define (compute-assignment-grade a default-grade)
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
      (* (+ ow nw) self-pts)))

  (define (compute-assignment-grade/id a-id default-grade)
    (compute-assignment-grade (id->assignment a-id) default-grade))

  (define (compute-grade default-grade)
    (for/sum ([a (in-list assignments)])
             (compute-assignment-grade a default-grade)))

  (define (assignment-file-display a-id)
    (define-values (html end-line-number)
      (for/fold ([html empty]
                 [line-offset 1])
                ([file (in-list (assignment-files a-id))])
        (define-values (table new-offset) 
          (file->html-table 
           a-id 
           (build-path (assignment-file-path a-id) file)
           line-offset))
        (values (append html (list table)) new-offset)))
    
    `(div ([class "files"])
          ,@html))

  (define (format-grade default-grade)
    (define g (compute-grade default-grade))
    (define l (letter-grade g))
    `(span ([class ,(substring l 0 1)])
           ,(format "~a (~a)"
                    (format-% g)
                    l)))

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
    (define the-breadcrumb
      (list (cons "Home" (main-url page/main))
            (cons "Assignments" #f)
            (cons a-id #f)
            (cons "Self Evaluation" #f)
            (cons "Edit" #f)))
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
             #:breadcrumb
             the-breadcrumb
             `(div ([class "eval"])
               (table
                (tr
                 (td ([class "files-cell"])
                  ,(assignment-file-display a-id))
                 (td ([class "prompt-cell"])
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
          #:breadcrumb the-breadcrumb
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
        #:breadcrumb the-breadcrumb
        "Self evaluation completed."))))

  (define (format-answer which ans)
    (define (string->linked-html s)
      (define positions
        (regexp-match-positions* #px"[l|L]\\d+" s))
      (define-values (html pos)
        (for/fold ([html empty] [pos 0])
          ([pos-pair positions])
          (values 
           (append 
            html 
            (list 
             (substring s pos (car pos-pair))
             `(a ([class "line-link"][href 
                   ,(format "#LC~a" 
                            (substring
                             (string-upcase 
                              (substring s 
                                         (car pos-pair)
                                         (cdr pos-pair)))
                             1))])
                 ,(substring s (car pos-pair) (cdr pos-pair)))))
                  (cdr pos-pair))))
      `(p ,@html ,(substring s pos (string-length s))))
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
                            (format-% value)])))
             (script ([src "/line-highlight.js"][type "text/javascript"]) " ")
             ,(string->linked-html (answer-comments ans)))]
      [else
       `(div ([class ,(format "answer incomplete ~a" which)])
             (p ,(format "~a evaluation is not completed." which)))]))

  (define (page/assignment/generalized/html a-id #:peer [peer #f])
    (define assignment (id->assignment a-id))
    (parameterize ([current-user (or peer (current-user))])
      `(div ([class "eval"])
        (div ([class "files-cell"])
             ,(assignment-file-display a-id))
        (div ([class "prompt-cell"])
             ,@(for/list ([q (in-list (assignment-questions assignment))]
                          [i (in-naturals)])
                 (match-define (question nw ow prompt type) q)
                 `(div ([class "answers"])
                       (p (span ([class "weight"])
                                ;; XXX incorporate optional-enable
                                ,(format-% (+ nw ow)))
                          ,prompt)
                       ,(format-answer
                         (if peer "Peer's Self" "Self")
                         (assignment-question-student-grade a-id i))
                       ,(format-answer
                         (if peer "Peer's Professor" "Professor")
                         (assignment-question-prof-grade a-id i))
                       ,(format-answer
                         (if peer "Your" "Peer's")
                         (assignment-question-peer-grade a-id i))))))))
    
  
  (define (page/assignment/self req a-id)
    (define assignment (id->assignment a-id))
    (template
     #:breadcrumb 
     (list (cons "Home" (main-url page/main))
            (cons "Assignments" #f)
            (cons a-id #f)
            (cons "Self Evaluation" #f))
     (page/assignment/generalized/html a-id)))

  (define (page/assignment/peer req a-id)
    (define assignment (id->assignment a-id))
    (define peer (assignment-peer a-id))
    (define the-breadcrumb
      (list (cons "Home" (main-url page/main))
            (cons "Assignments" #f)
            (cons a-id #f)
            (cons "Peer Evaluation" #f)))
    (if (equal? default-peer peer)
      (template
       #:breadcrumb the-breadcrumb
       `(div ([class "notice"]) "Your peer has not been assigned."))
      (template
       #:breadcrumb the-breadcrumb
       (page/assignment/generalized/html a-id #:peer peer))))

  (define (page/assignment/peer/edit req a-id)
    (define assignment (id->assignment a-id))
    (define the-breadcrumb
      (list (cons "Home" (main-url page/main))
            (cons "Assignments" #f)
            (cons a-id #f)
            (cons "Peer Evaluation" #f)
            (cons "Edit" #f)))
    (define (pick-a-person)
      (define student-ids (users))
      (define finished-self-eval
        (remove (current-user)
                (filter (λ (student-id)
                          (parameterize ([current-user student-id])
                            (self-eval-completed? assignment)))
                        student-ids)))
      (define already-assigned
        (map (λ (student-id)
               (parameterize ([current-user student-id])
                 (assignment-peer a-id)))
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
       (display-to-file* the-peer (assignment-peer-path a-id))        
      the-peer)

    (define peer-id
      (if (file-exists? (assignment-peer-path a-id))
        (assignment-peer a-id)
        (pick-a-person)))

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
                (assignment-question-student-grade-path/peer a-id i)))
              ;; They have grade
              (file-exists?
               (parameterize ([current-user peer-id])
                  (assignment-question-student-grade-path a-id i))))
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
        #:breadcrumb the-breadcrumb
        "Peer evaluation completed, or not available (as peer has not finished their grading.)"))))

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
                              (cons "Assignments" #f)
                              (cons a-id #f)
                              (cons "Peer Evaluation" #f)
                              (cons "Edit" #f))
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

  (define (file->html-table a-id file-path line-offset)
    (define file-lines
      (string-split (bytes->string/utf-8 (file->bytes file-path))
                    #px"\r\n?|\n"
                    #:trim? #f))
    (define (line->line-content-div line line-num)
      `(div ([id ,(format "LC~a" line-num)]
             [class "line"]
             [rel ,(format "#LC~a" line-num)])
            ,((λ (l)(if (string=? "" l) '(br) l)) line)))

    (values 
     `(div ([class "file"])
           (div ([class "meta"]) ,(format "~a" (path->last-part file-path)))
           (div 
            ([class "data type-text"])
            (table 
             ([class "lines"][cellspacing "0"][cellpadding "0"])
             (tbody
              (tr
               (td
                (pre ([class "line_numbers"]
                      [style "margin: 0pt; padding-right: 10px;"])
                     ,@(map
                        (λ (n)
                          `(span ([id ,(format "L~a" n)])
                                 ,(number->string n) (br)))
                        (build-list (length file-lines) 
                                    (curry + line-offset)))))
               (td ([width "100%"])
                   (div (pre
                         ,@(map line->line-content-div file-lines
                                (build-list (length file-lines)
                                            (curry + line-offset)))))))))))
     (+ line-offset (length file-lines))))
    
  (define (page/logout req)
    (redirect-to
     (main-url page/root)
     #:headers
     (list (cookie->header logout-id-cookie))))

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
    (define-values (past upcoming)
      (partition
       (λ (a) 
         (if (assignment-peer-secs a)
           (or (peer-eval-completed? a)
               (> (current-seconds) (assignment-peer-secs a)))
           (or (self-eval-completed? a)
               (> (current-seconds) (assignment-eval-secs a)))))
       assignments))
   
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

      `(table 
        ([class ,(format "assignment ~a ~a"
                         (cond
                           [(not (zero? (assignment-optional-weight a)))
                            "optional"]
                           [(not (zero? (assignment-normal-weight a)))
                            "normal"]
                           [else
                            "optenable"])
                         (if (peer-eval-completed? a)
                           "completed"
                           "incomplete"))])
        (tr (td ,(assignment-id a))
            (td ,(format-%
                  (+ (assignment-normal-weight a)
                     (assignment-optional-weight a))))
            (td ,(cond
                   [next-due
                   (format
                    "Due ~a in ~a"
                    (date->string (seconds->date next-due))
                    (secs->time-text (- next-due (current-seconds))))]
                   [(not (prof-eval-completed? a))
                    "Completed, waiting on professor evaluation."]
                   [else
                   `(span "Completed: "
                          ,(format-%
                            (compute-assignment-grade/id 
                             (assignment-id a)
                             0)))])))
        (tr (td ,(cond-hyperlink
                  (current-seconds) (assignment-due-secs a)
                  "Turn in Files"
                  (main-url page/assignment/files
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
      #:breadcrumb (list (cons "Home" #f))
      ;; TODO
      `(div ([id "class-score"])
            (table (tr (th "Mininum Final Grade")
                       (th "Maximum Final Grade"))
                   (tr (td ,(format-grade 0))
                       (td ,(format-grade 1)))))
      `(div ([class "assignments upcoming"])
            (h1 "Future")
            ,@(map render-assignment upcoming))
      `(div ([class "assignments past"])
            (h1 "Past")
            ,@(map render-assignment past)))))

  (define (page/assignment/files/delete req a-id file-to-delete)
    (define assignment (id->assignment a-id))
    (when (< (current-seconds) (assignment-due-secs assignment))
      (define file-path
        (build-path (assignment-file-path a-id) file-to-delete))
      (when (file-exists? file-path)
        (delete-file file-path)))
    (redirect-to (main-url page/assignment/files a-id)))

  (define (page/assignment/files req a-id)
    (define assignment (id->assignment a-id))
    (define new-file-request
      (send/suspend
       (λ (k-url)
         (define seconds-left
           (- (assignment-due-secs assignment) (current-seconds)))
         (define files (assignment-files a-id))
         (define closed? (< seconds-left 0))
         (template
          #:breadcrumb (list (cons "Home" (main-url page/main)) 
                             (cons "Assignments" #f)
                             (cons a-id #f)
                             (cons "Files" #f))
          `(div ([class "eval"])
                (table
                 (tr
                  (td ([class "files-cell"])
                      ,(assignment-file-display a-id))
                  (td ([class "prompt-cell"])
                      (p ([class "notice"])
                         ,(format "File Management for ~a ~a" a-id
                                  (if closed?
                                    "is closed"
                                    (format "closes in ~a" 
                                            (secs->time-text seconds-left)))))
                      ,(if (empty? files)
                         `(p ([class "notice"]) "No files uploaded yet for this assignment")
                         `(table ([class "upload-table"])
                                 (tr (th "Filename") (th "Delete?"))
                                 ,@(map
                                    (λ (filename)
                                      `(tr (td ,filename)
                                           (td ,(if closed? 
                                                  "X"
                                                  `(a ([href ,(main-url 
                                                               page/assignment/files/delete a-id
                                                               filename)])
                                                      "X")))))
                                    files)))
                      ;; XXX Add a textarea box
                      ,@(if closed?
                          empty
                          (list 
                           `(form ([action ,k-url]
                                   [method "post"]
                                   [enctype "multipart/form-data"])
                                  (input ([type "file"]
                                          [name "new-file"]))
                                  (input ([type "submit"]
                                          [value "Upload File"])))
                           `(br)
                           `(form ([action ,k-url]
                                   [method "post"])
                                  (input ([type "text"]
                                          [name "filename"]))
                                  (textarea ([name "file-content"]
                                             [rows "40"]
                                             [cols "80"]))
                                  (input ([type "submit"]
                                          [value "Add File"])))))))))))))
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
    (make-directory* (assignment-file-path a-id))
    (when (< (current-seconds) (assignment-due-secs assignment))
      (display-to-file #:exists 'replace
       file-content
       (build-path (assignment-file-path a-id)
                   (bytes->string/utf-8
                    (binding:file-filename new-file-binding)))))
    (redirect-to (main-url page/assignment/files a-id)))

  (define (template #:breadcrumb bc
                    . bodies)
    (response/xexpr
     `(html (head (title ,@(add-between (map car bc) " > "))
                  #;(script ([src "/sorttable.js"]) " ")
                  (script ([src "https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js"]) " ")
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
                            (a ([href ,(main-url page/account)]) ,(current-user)) " | "
                            (a ([href ,(main-url page/logout)]) "logout"))
                     ""))
             (div ([class "content"])
                  ,@bodies)
             ,(footer)))))

  (define (page/student/photo req student)
    (parameterize ([current-user student])
      (define user-img-path (user-image-path))
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
                   (md5 (string-downcase (string-trim-both user-email))))))))

  (define (user-info-complete?)
    (match-define (student nick first last email) 
                  (student-info (current-user)))
    (and (not (string=? nick ""))
         (not (string=? first ""))
         (not (string=? last ""))
         (not (string=? email ""))
         (file-exists? (user-image-path))))

  (define (student-info u) 
    (parameterize ([current-user u])
      (define p (user-info-path))
      (if (file-exists? p)
        (file->value p)
        (student "" "" "" ""))))
  (define (student-display-name u)
    (match-define (student nick first last _) (student-info u))
    (format "~a \"~a\" ~a"
            first nick last))

  (define (page/admin/grade-next req)
    (unless (is-admin?)
      (send/back
       (template
        #:breadcrumb (list (cons "Admin" (main-url page/main))
                           (cons "Grading" #f))
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
       (define id (assignment-id a))
       (define qs (assignment-questions a))
       (define the-info (student-info u))
       (match-define
        (cons q i)
        (for/or ([q (in-list qs)]
                 [i (in-naturals)]
                 #:unless
                 (file-exists?       
                  (parameterize ([current-user u])
                    (assignment-question-prof-grade-path id i))))
          (cons q i)))
       
       (define score-formlet
         (match (question-type q)
           ['numeric numeric-formlet]
           ['bool boolean-formlet]))
       (define the-formlet
         (parameterize ([current-user u])
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
            (values prof-score comment))))

       (define-values (score comment)
         (formlet-process
          the-formlet
          (send/suspend
           (λ (k-url)
             (template
              #:breadcrumb (list (cons "Admin" (main-url page/admin))
                                 (cons "Grading" #f)
                                 (cons (student-display-name u) #f)
                                 (cons id #f))
              `(div ([class "eval"])            
                (table 
                 (tr
                  (td ([class "files-cell"])
                   ,(parameterize ([current-user u])
                      (assignment-file-display id))
                   (td ([class "prompt-cell"])
                       (div ([class "student-info"])
                            (img ([src ,(main-url page/student/photo u)]
                                  [height "80"])) 
                            (br)
                            ,(student-display-name u))
                       (p ,(question-prompt q))
                       (form 
                        ([action ,k-url] [method "post"])
                        ,@(formlet-display the-formlet)
                        (input ([type "submit"] [value "Submit"])))))))))))))
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
        (parameterize ([current-user u])
          (assignment-question-prof-grade-path id i)))

       (redirect-to
        (main-url page/admin/grade-next))]
      [#f
       (send/back
        (template
         #:breadcrumb (list (cons "Admin" (main-url page/admin)) 
                            (cons "Grading" #f))
         "All grading is done! Great!"))]))

  (define (page/admin req)
    (unless (is-admin?)
      (page/root req))

    (send/back
     (template
      #:breadcrumb (list (cons "Admin" #f))
      `(div ([id "grade-button"])
            (a ([href ,(main-url page/admin/grade-next)]) "Grade"))
      `(table ([id "grades"])
        (thead
         (tr (th "Student")
             (th "Min")
             (th "Max")
             (th "Ungraded")))
        (tbody
         ,@(for/list ([u (in-list (sorted-users))])
             (parameterize ([current-user u])
               `(tr 
                 (td ,(student-display-name u))
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
          (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy")
          ","
          (a ([href "http://trevoroakes.com/"]) "Trevor Oakes") " and "
          "BYU PLT."
          (br)
          (span ([id "timestamp"]) 
                ,(date->string (seconds->date (current-seconds)) #t))))

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
            (if (or (user-info-complete?)
                    (is-admin?))
              (main-dispatch req)
              (page/account req)))])]
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
