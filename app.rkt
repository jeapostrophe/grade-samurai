#lang racket/base
(require web-server/http
         web-server/http/bindings
         web-server/dispatchers/dispatch
         web-server/dispatch
         web-server/servlet/web
         web-server/formlets
         racket/file
         racket/list
         racket/match
         racket/date
         racket/runtime-path
         racket/string
         racket/function
         file/md5
         (only-in srfi/13 string-trim-both)
         "model.rkt"
         "../m8b/id-cookie.rkt")

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

(define-runtime-path source-dir ".")

(define (make-start-handler
         #:admin-users-hash
         [admin-users (hash)]
         #:assignment-list 
         [assignments empty]
         #:authenticate-users-with
         [authenticate-users (λ (un pw) #f)]
         #:username-request-text
         [login-formlet-un-text "Username: "]
         #:password-request-text
         [login-formlet-pw-text "Password: "]
         #:secret-salt-path 
         [secret-salt-path "secret-salt"])
  (begin
    (define (id->assignment a-id)
      (findf (λ (a) (string=? a-id (assignment-id a))) assignments))
    (define (is-admin-username? un) (hash-has-key? admin-users un)); TODO move to model?
    (define (authenticate-admin un pw); TODO move to model?
      (if (is-admin-username? un)
          (string=? pw (hash-ref admin-users un))
          #f))
    (define secret-salt (file->bytes secret-salt-path))
    
    (define (show-root req)
      (if (is-admin-username? (current-user)) 
          (render-admin)
          (render-main)))
    
    (define (login req [last-error #f])
      (define login-formlet
        (formlet
         (table
          (tr (td ,login-formlet-un-text)
              (td ,{(to-string (required (text-input))) . => . username}))
          (tr (td ,login-formlet-pw-text)
              (td ,{(to-string (required (password-input))) . => . password})))
         (values username password)))
      (define log-req ;TODO better name
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
        (if (is-admin-username? username)
            (authenticate-admin username password)
            (authenticate-users username password)))
      
      (if authenticated?
          (redirect-to (main-url show-root)
                       #:headers
                       (list (cookie->header (make-id-cookie secret-salt username))))
          (login req (format "Invalid password for user (~S)" username))))
    
    (define (default-text-input default-string)
        (to-string (default (string->bytes/utf-8 default-string) 
                     (text-input #:value (string->bytes/utf-8 default-string)))))
    
    (define (manage-account req)
      
      (define user-dir (build-path source-dir "db" (current-user)))
      
      (define existing-info 
        (cond
          [(file-exists? (build-path user-dir "info.rktd"))
           (file->value (build-path user-dir "info.rktd"))]
          [else (student "" "" "" "")]))
      
      (define account-formlet
        (formlet
         (div ([id "form-inputs"])
              (table
               (tr (td "Legal First Name: ")
                   (td ,{(default-text-input (student-firstname existing-info)) . => . first-name}))
               (tr (td "Last Name: ")
                   (td ,{(default-text-input (student-lastname existing-info)) . => . last-name}))
               (tr (td "I Prefer to be Known As: ")
                   (td ,{(default-text-input (student-nickname existing-info)) . => . nick-name}))
               (tr (td "Email Address: ")
                   (td ,{(default-text-input (student-email existing-info)) . => . email}))
               (tr (td "Picture I can be recognized by: ")
                   (td ,{(file-upload) . => . photo}))))
         (values first-name last-name nick-name email photo)))
      
      (define account-form
        (send/suspend
         (λ (k-url)
           (template
            #:breadcrumb (list (cons "Account Admin" #f))
            `(div ([id "account"])
                  (h1 ,(format "Account Admin for ~a" (current-user)))
                  (form ([action ,k-url] [method "post"] [enctype "multipart/form-data"])
                        ,@(formlet-display account-formlet)
                        (p "Instead of this: "
                           (img ([src ,(main-url view-student-photo (current-user))]
                                 [width "160"] [height "160"])))
                        (input ([type "submit"] [value "Update Info"]))))))))
      
      (define-values (first-name last-name nick-name email photo) 
        (formlet-process account-formlet account-form))
      
      (make-directory* user-dir)
      (write-to-file (student nick-name first-name last-name email) 
                     (build-path user-dir "info.rktd") #:exists 'replace)
      (if (binding:file? photo)
          (display-to-file (binding:file-content photo)
                           (build-path user-dir "photo.jpg") #:exists 'replace)
          (void))
      
      (redirect-to (main-url show-root))) ;TODO redirect to student page?
    
    
    (define (view-student req student)
      (cond 
        [(or (is-admin-username? (current-user))
             (string=? (current-user) student))
         (send/suspend/dispatch
          (λ (embed/url)
            (response/xexpr
             `(html (head (title ,student))
                    (body (h1 "Student Page for " ,student))))))]
        [else 
         (send/suspend/dispatch 
          (λ (e) 
            (response/xexpr
             `(html (head (title ,student))
                    (body (h1 "You do not have permission to view other student's pages."))))))]))
    
    (define-values (main-dispatch main-url main-applies?)
      (dispatch-rules+applies
       [("") show-root]
       [("login") login]
       [("logout") logout]
       [("account") manage-account]
       [("student" (string-arg)) view-student]
       [("student" (string-arg) "photo") view-student-photo]
       [("assignment" (string-arg) "manage-files") manage-files]
       [("assignment" (string-arg) "manage-files" "delete" (string-arg)) delete-a-file]
       [("assignment" (string-arg) "self-eval") evaluate-self]
       [("assignment" (string-arg) "peer-eval") evaluate-peer]))
    
    (define (evaluate-self req a-id);TODO put files in iframe - actually ask questions
      (define assignment (id->assignment a-id))
      (define files (directory-list (build-path source-dir "db" (current-user) a-id "uploads")))
      (define (ask-question q)
        (define file-lines-formlet
          (formlet
           (table (tr (td (p "Which file do you want to highlight?")
                          ,{(radio-group (append (map path->string files) `("None"))) . => . file}))
                  (tr (td "Space Separated Line Numbers")
                      (td ,{(default-text-input "") . => . line-nums})))
           (values file line-nums)))
        (define-values (file line-nums)
          (formlet-process
           file-lines-formlet
           (send/suspend
            (λ (k-url)
              (template #:breadcrumb (list (cons "Self Evaluation - File" #f));TODO
                        `(div
                          (h1 "Relevant Line Selection")
                          (p "Pick the lines that demonstrate that you deserve credit for the following question:")
                          (p ,(question-question q))
                          (form ([action ,k-url] [method "post"]) 
                                ,@(formlet-display file-lines-formlet)
                                (input ([type "submit"])))
                          (div ([id "files"])
                               ,@(map (λ(file)
                                        ((curry file->html-table a-id) 
                                         (build-path source-dir "db" (current-user) a-id "uploads" file))) 
                                      files))))))))
        
        (define self-score-formlet
          (formlet 
           (div ,{(radio-group '(1 0) 
                               #:display (λ (x) (if (= 1 x) "Yes" "No"))) 
                  . => . credit?})
           credit?))
        (define-values (self-score)
          (formlet-process
           self-score-formlet
           (send/suspend
            (λ  (k-url)
              (template #:breadcrumb (list (cons "Self Evaluation" #f))
                        `(div (h1 "Self Evalution")
                              (p ,(question-question q))
                              (p "Do the selected lines demonstrate that you deserve credit for this question?")
                              ;TODO show those lines selected
                              (form ([action ,k-url] [method "post"])
                                    ,@(formlet-display (formlet 
                                                        (div ,{(radio-group '(1 0) 
                                                                            #:display (λ (x) (if (= 1 x) "Yes" "No"))) 
                                                               . => . credit?})
                                                        credit?))
                                    (input ([type "submit"])))))))))
        (question-self-eval self-score file (map string->number (string-split line-nums))))
      
      (write-to-file #:exists 'replace
       (for/list ([question (assignment-questions assignment)])
         (ask-question question))
       (build-path source-dir "db" (current-user) a-id "self-eval.rktd"))
      (redirect-to (main-url show-root)))
    
    
    
    
    (define (evaluate-peer a-id req)
      empty)
    
    (define (file->html-table a-id file)
      (define (file->name f)
        (define-values (base name must-be-dir?)
          (split-path f))
        (path->string name))
      (define file-lines (string-split (bytes->string/utf-8 (file->bytes file))
                                       #px"\r\n?|\n"
                                       #:trim? #f))
      (define (line->line-content-div line line-num)
        `(div ([id ,(format "~aLC~a" file line-num)][class "line"]) 
              ,((λ (l)(if (string=? "" l) '(br) l)) line)))
      
      `(div ([class "file"])
            (div ([class "meta"]) ,(format "~a" (file->name file)))
            (div ([class "data type-text"])
                 (table ([class "lines"][cellspacing "0"][cellpadding "0"])
                        (tbody
                         (tr
                          (td
                           (pre ([class "line_numbers"][style "margin: 0pt; padding-right: 10px;"])
                                ,@(map (λ (n) `(span ([id ,(format "~aL~a" file n)]
                                                      [rel ,(format "#~aL~a" file n)]) 
                                                     ,(number->string n) (br)))
                                       (build-list (length file-lines) add1))))
                          (td ([width "100%"])
                              (div ([class "highlight"])
                                   (pre
                                    ,@(map line->line-content-div file-lines 
                                           (build-list (length file-lines) add1)))))))))))
    
    (define (display-files student a-id select)
      #;(define files (filter (λ (p) (file-exists? p))
                              #;(directory-list (source-dir) "db" student a-id "uploads" #:build #t)
                              (directory-list (build-path source-dir "db" student a-id "uploads"))))
      (define files (directory-list (build-path source-dir "db" student a-id "uploads")))
      
      (send/back
       (response/xexpr
        `(html (head (title ,(format "Files for ~a" a-id))
                     (body (h1 ,(format "Files for ~a" a-id))
                           (div ([id "files"])
                                ,@(map file->html-table files))))))))
    
    
    
    (define (logout req)
      (redirect-to
       (main-url show-root)
       #:headers
       (list (cookie->header logout-id-cookie))))
    
    (define (render-main)
      (define a-day (* 60 60 24))
      (define 2-days (* a-day 2))
      (define (cond-hyperlink available closed text1 link1 text2 link2)
        (cond
          [(< (current-seconds) available)
           text1]
          [(> (current-seconds) closed)
           `(a ([href ,link2]) ,text2)]
          [else
           `(a ([href ,link1]) ,text1)]))
      (define-values (upcoming past)
        (partition (λ (a) (((assignment-due a) . + . 2-days) . > . (current-seconds)))
                   (sort assignments < #:key assignment-due)))
      (define (secs->time-text s);TODO fix nonplurals
        (define unit
          (findf (λ (unit-pair) (s . >= . (car unit-pair)))
                 `((,(* 60 60 24 7) . "week") (,(* 60 60 24) . "day") (,(* 60 60) . "hour") (60 . "minute") (1 . "second"))))
        (format "~a ~as" (quotient s (car unit)) (cdr unit)))
      (define (self-eval-completed? a-id user) #f);TODO
      (define (render-assignment a);TODO render offline assignments (like the final) differently
        (let ([next-due
               (+ (assignment-due a)
                  (cond
                    [(self-eval-completed? (assignment-id a) (current-user))
                     2-days]
                    [(> (current-seconds) (assignment-due a))
                     a-day]
                    [else
                     0]))])
          `(table (tr (td ,(assignment-title a)) 
                      (td "0%");TODO
                      (td ,(format 
                            "Due ~a in ~a"
                            (date->string (seconds->date next-due))
                            (secs->time-text (- next-due (current-seconds))))))
                  (tr (td ,(cond-hyperlink (current-seconds) (assignment-due a)
                                           "Turn in Files" (main-url manage-files (assignment-id a))
                                           "View Files" (main-url show-root)#|TODO|#))
                      (td ,(cond-hyperlink (assignment-due a) (+ a-day (assignment-due a))
                                           "Self Evaluation" (main-url evaluate-self (assignment-id a));TODO only if there is 1+ files
                                           "Self Evaluation Details" (main-url show-root)#|TODO|#)) 
                      (td ,(cond-hyperlink (+ a-day (assignment-due a)) (+ 2-days (assignment-due a))
                                           "Grade a Peer" (main-url evaluate-peer (assignment-id a));TODO only if done self-eval and 1+ files
                                           "Grade a Peer Details" (main-url show-root)#|TODO|#))))))
      (send/suspend/dispatch
       (λ (embed/url)
         (response/xexpr
          `(html (head (title "Student Main Page"))
                 (body (h1 "Student Main Page")
                       (div ([id "class-score"]);TODO
                            (table (tr (th "Mininum Final Grade") (th "Expected Grade") (th "Maximum Final Grade"))
                                   (tr (td "0%") (td "75%") (td "100%"))))
                       (div ([id "upcoming-assignments"])
                            ,@(map render-assignment upcoming))
                       (div ([id "past-assignments"])
                            ,@(map render-assignment past))))))))
    
    (define (delete-a-file req a-id file-to-delete)
      (define assignment (findf (λ (a) (string=? a-id (assignment-id a))) assignments))
      (cond
        [(< (current-seconds) (assignment-due assignment))
         (define file-path (build-path source-dir "db" (current-user) a-id "uploads" file-to-delete))
         (if (file-exists? file-path)
             (delete-file file-path)
             (void))]
        [else (void)])
      (redirect-to (main-url manage-files a-id)))
    
    (define (manage-files req a-id)
      (define assignment (findf (λ (a) (string=? a-id (assignment-id a))) assignments))
      (define files-dir (build-path source-dir "db" (current-user) a-id "uploads"))
      (make-directory* files-dir)
      (define (extract-binding:file req)
        (bindings-assq #"new-file" (request-bindings/raw req)))
      (define new-file-binding
        (extract-binding:file
         (send/suspend
          (λ (k-url)
            (response/xexpr
             `(html (head (title ,(format "Manage Files - ~a" a-id)))
                    (body (h1 ,(format "Manage Files for ~a" a-id))
                          (p ,(let ([seconds-left (- (assignment-due assignment) (current-seconds))])
                                (format "File Management for ~a ~a" a-id
                                        (if (seconds-left . < . 0)
                                            "is closed"
                                            (format "closes in ~a seconds" seconds-left)))))
                          (table
                           (tr (th "Filename") (th "Delete?"))
                           ,@(map (λ (file-path)
                                    `(tr (td ,(path->string file-path))
                                         (td (a ([href ,(main-url delete-a-file a-id (path->string file-path))]) "X"))))
                                  (directory-list files-dir)))
                          (form ([action ,k-url] [method "post"] [enctype "multipart/form-data"])
                                (table (tr (td (input ([type "file"] [name "new-file"]))) 
                                           (td (input ([type "submit"][value "Add File"])))))))))))))
      (define file-content (binding:file-content new-file-binding))
      (when (contains-greater-than-80-char-line? file-content)
        (error 'upload-file 
               "Cannot upload files with lines greater than 80 characters"))
      (when (< (current-seconds) (assignment-due assignment))
        (display-to-file
         file-content
         (build-path files-dir
                     (bytes->string/utf-8
                      (binding:file-filename new-file-binding)))))
      (redirect-to (main-url manage-files a-id)))
    
    
    (define (template #:breadcrumb bc
                      . bodies)
      (response/xexpr
       `(html (head (title ,@(add-between (map car bc) " / "))
                    #;(script ([src "/sorttable.js"]) " ")
                    #;(link ([rel "stylesheet"] [type "text/css"] [href "/render.css"])))
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
                                #|,@(if (next-applicant?)
                                  (list `(a ([href ,(top-url next-app)]) "next") " | ")
                                  empty)|#
                                ;(a ([href ,(top-url archive)]) "archive") " | "
                                (a ([href ,(main-url logout)]) "logout"))
                         ""))
               (div ([class "content"])
                    ,@bodies
                    ,(footer))))))
    
    (define (require-login-then-dispatch req)
      (cond 
        [(main-applies? req)
         (define maybe-id (request-valid-id-cookie secret-salt req))
         (match maybe-id
           [#f (login req)]
           [id (parameterize ([current-user id]) 
                 (main-dispatch req))])]
        [else (next-dispatcher)])))
  require-login-then-dispatch)

(define (view-student-photo req student)
  (define user-img-path (build-path source-dir "db" student "photo.jpg"))
  (define user-info-path (build-path source-dir "db" student "info.rktd"))
  (define user-email 
    (if (file-exists? user-info-path)
        (student-email (file->value user-info-path))
        ""))
  (if (file-exists? user-img-path)
      (response/full
       200 #"Okay"
       (current-seconds) #"image/jpg"
       empty
       (list (file->bytes user-img-path)))
      (redirect-to (format "http://www.gravatar.com/avatar/~a?s=160&d=mm" 
                           (md5 (string-downcase (string-trim-both user-email)))))))

(define (render-admin)
  (send/suspend/dispatch
   (λ (embed/url)
     (response/xexpr
      `(html (head (title "Admin Page"))
             (body (h1 "Admin Page")))))))

(define current-user (make-parameter #f))

(define (footer)
  `(div ([id "footer"])
        "Powered by " (a ([href "http://racket-lang.org/"]) "Racket") ". "
        "Written by " (a ([href "http://trevoroakes.com/"]) "Trevor Oakes") " and " 
        (a ([href "http://faculty.cs.byu.edu/~jay"]) "Jay McCarthy") ". "))

(define (tabs header . the-tabs)
  (define found-selected? #f)
  (define tab-seq
    (build-vector (/ (length the-tabs) 2)
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
  `(div ([class "tabbed"])
        (div ([class "tab-header"])
             (div ([class "tab-uheader"]) ,header)
             (ul
              ,@(for/list ([v (in-vector tab-seq)])
                  (match-define (vector id selected? no-content? label body) v)
                  (define direct-link
                    (match body
                      [(cons #f url) url]
                      [_ #f]))
                  `(li ([id ,(format "li~a" id)] ,@(if selected? `([class "tab-selected"]) empty))
                       ,(cond
                          [no-content?
                           label]
                          [direct-link
                           `(a ([href ,direct-link]) ,label)]
                          [else
                           `(a ([href ,(format "javascript:~a~a;"
                                               (for/fold ([s ""]) ([v (in-vector tab-seq)])
                                                 (match-define (vector id selected? no-content? label _) v)
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

(provide make-start-handler)
