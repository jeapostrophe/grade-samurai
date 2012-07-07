#lang racket/base
(require web-server/http
         web-server/dispatchers/dispatch
         web-server/dispatch
         web-server/servlet/web
         web-server/formlets
         racket/file
         racket/list
         racket/match
         racket/runtime-path
         file/md5
         srfi/13
         "model.rkt"
         "../m8b/id-cookie.rkt")

(define-runtime-path source-dir ".")

(define (make-start-handler
         #:admin-users-hash
         [admin-users (hash)]
         #:assignment-list 
         [assignments '()]
         #:authenticate-users-with
         [authenticate-users (λ (un pw) #f)]
         #:username-request-text
         [login-formlet-un-text "Username: "]
         #:password-request-text
         [login-formlet-pw-text "Password: "]
         #:submissions-close-at 
         [submissions-close-at (deadline 23 59 0)]
         #:secret-salt-path 
         [secret-salt-path "secret-salt"])
  (begin
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
    
    
    (define (manage-account req)
      
      (define user-dir (build-path source-dir "db" (current-user)))
      
      (define existing-info 
        (cond
          [(file-exists? (build-path user-dir "info.rktd"))
           (file->value (build-path user-dir "info.rktd"))]
          [else (student "" "" "" "")]))
      
      (define (default-text-input default-string)
        (to-string (default (string->bytes/utf-8 default-string) (text-input #:value (string->bytes/utf-8 default-string)))))
      
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
                   (td ,{(file-upload) . => . photo})))
              (p "Instead of this: " 
                 (img ([src ,(format "/students/~a/photo" (current-user))]))))
         (values first-name last-name nick-name email photo)))
      
      (define account-form
        (send/suspend
         (λ (k-url)
           (template
            #:breadcrumb (list (cons "Account Admin" #f))
            `(div ([id "account"])
                  (h1 ,(format "Account Admin for ~a" (current-user)))
                  (form ([action ,k-url] [method "post"])
                        ,@(formlet-display account-formlet)
                        (input ([type "submit"] [value "Update Info"]))))))))
      
      (define-values (first-name last-name nick-name email photo) 
        (formlet-process account-formlet account-form))
      
      (make-directory* user-dir)
      (call-with-output-file (build-path user-dir "info.rktd") #:exists 'replace
        (λ (info-out) 
          (write (student nick-name first-name last-name email) info-out)))
      (cond 
        [(binding:file? photo)
         (call-with-output-file (build-path user-dir "photo.jpg") #:exists 'replace
           (λ (photo-out)
             (write-bytes (binding:file-content photo) photo-out)))]
        [(binding:form? photo)
         (displayln "binding:form")
         (call-with-output-file (build-path user-dir "photo.jpg") #:exists 'replace
           (λ (photo-out)
             (write-bytes (binding:form-value photo) photo-out)))])
      
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
       [("students" (string-arg)) view-student]
       [("students" (string-arg) "photo") view-student-photo]))
    
    (define (logout req)
      (redirect-to
       (main-url show-root)
       #:headers
       (list (cookie->header logout-id-cookie))))
    
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

(define (render-main)
  (send/suspend/dispatch
   (λ (embed/url)
     (response/xexpr
      `(html (head (title "Student Main Page"))
             (body (h1 "Student Main Page")))))))

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
