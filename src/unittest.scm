
(module unittest *

  (import 
    scheme 
    (chicken base) 
    (chicken condition) 
    (chicken pretty-print) 
    (chicken port) 
    (chicken string) 
    srfi-1 
    srfi-19
    sxml-transforms)

  (define highlight-version "11.11.1")

  (define (sxml-tree title . body)
    `((html (@ (xmlns "http://www.w3.org/1999/xhtml")
               (xml:lang "en") 
               (lang "en"))
            (head
              (meta (@ (name "viewport") (content "width=device-width,initial-scale=1")))
              (link (@ (rel "stylesheet") 
                       (href "https://www.w3schools.com/w3css/4/w3.css") 
                       #;(href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css") 
                       #;(href "https://classless.de/classless.css") 
                       #;(href "https://www.w3.org/StyleSheets/Core/Steely") 
                       (type "text/css")))
              #;(link (@ (rel "stylesheet") 
                         (href "https://www.w3schools.com/lib/w3-theme-blue-grey.css") 
                         (type "text/css")))
              (link (@ (rel "stylesheet") 
                       (href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/" ,highlight-version "/styles/default.min.css") 
                       (type "text/css")))
              (style "code, pre, tt, kbd, samp, .w3-code { font-family: Monaco, 'Ubuntu Mono', monospace; }"
                     "html, body, h1, h2, h3, h4, h5, h6 { font-family: 'Lucida Sans', 'Ubuntu Sans', sans-serif; }")
              (script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/" ,highlight-version "/highlight.min.js")))
              ,@(map (lambda (lang) 
                       `(script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/" ,highlight-version "/languages/" ,lang ".min.js")))) 
                  '(scheme python))
              (script "hljs.highlightAll();")
              (title ,title))
            (body (@ (class "w3-content") (style "max-width:61.8%")) ,@body
	    (hr)
            (footer (@ (class "w3-container w3-center"))
                    (small
                      (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/"))
                         (img (@ (alt "Creative Commons License") (style "border-width:0")
                                 (src "https://mirrors.creativecommons.org/presskit/icons/cc.svg"))))
                      (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/"))
                         (img (@ (alt "Creative Commons License") (style "border-width:0")
                                 (src "https://mirrors.creativecommons.org/presskit/icons/by.svg"))))
                      (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/"))
                         (img (@ (alt "Creative Commons License") (style "border-width:0")
                                 (src "https://mirrors.creativecommons.org/presskit/icons/sa.svg"))))
                      (br)
                      (p "This work is licensed under a "
                         (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/")) 
                            "Creative Commons Attribution-ShareAlike 4.0 International License")
			 (br)
			 (small ,(date->string (current-date))))))))))

  (define sxml-handler-container (lambda (tag body) `(div (@ (class "w3-container")) ,@body)))

  (define sxml-handler-code/lang
    (lambda (tag body)
      (let ((lang (car body))
	    (code (cdr body)))
        `(div (@ (class "w3-card w3-round"))
              (header (@ (class "w3-container w3-border w3-round w3-light-gray w3-right")) ,lang " code")
              (div (@ (class "w3-container")) 
		   (pre 
		     (code (@ (class "w3-code w3-border w3-round language-" ,lang)) ,code)))))))

  (define sxml-handler-code/scheme
    (lambda (tag body)
      (let* ((expr (if (eq? (length body) 1) (car body) (cons 'begin body))))
	(sxml-handler-code/lang 'code/lang (list 'scheme (call-with-output-string (lambda (p) (pretty-print expr p))))))))

  (define sxml-handler-code/scheme-file
    (lambda (tag body)
      (sxml-handler-code/scheme 'code/scheme (list (with-input-from-file (car body) (lambda () (read)))))))

  (define sxml-handler-cite/a (lambda (tag body) `(cite (a (@ (href ,(car body))) ,@(cdr body)))))

  (define (SXML->file! tree filename)
    (with-output-to-file (conc filename ".html")
      (lambda ()
        (display "<!doctype html>")
        (SXML->HTML
          (pre-post-order*
            tree
            `((container . ,sxml-handler-container)
              (code/lang . ,sxml-handler-code/lang)
              (code/scheme . ,sxml-handler-code/scheme)
              (code/scheme-file . ,sxml-handler-code/scheme-file)
              (cite/a . ,sxml-handler-cite/a)
              ,@alist-conv-rules*))))))

  (define-record unittest/testcase name log)

  (define (unittest/wasrun name) (make-unittest/testcase name '()))

  (define (unittest/testcase-logcons! testcase msg)
    (unittest/testcase-log-set! testcase (cons msg (unittest/testcase-log testcase))))

  (define (unittest/testcase-run testcase result sut)
    (let* ((methods (cdr sut))
	   (setup (alist-ref 'setup methods))
	   (teardown (alist-ref 'teardown methods))
	   (testcase-name (unittest/testcase-name testcase)))
      (unittest/result-started! result)
      (let-values ((args (if setup ((car setup) testcase) (values)))
		   ((f code) (apply values (alist-ref testcase-name methods))))
        ;(condition-case (apply (car (alist-ref testcase-name methods)) testcase args)
        (condition-case (apply f testcase args)
          (c (exn unittest-assert-equal) 
             (unittest/result-failed! result 
                                      (cons testcase-name (get-condition-property c 'unittest-assert-equal 'comparison))))
          (c (exn)
             (unittest/result-failed! result 
                                      (list testcase-name (call-with-output-string
                                                            (lambda (port) (print-error-message c port))))))
          (c () (unittest/result-failed! result (list testcase-name c))))
        (when teardown (apply (car teardown) testcase args))
	`((h2 (code ,testcase-name))
	  (code/scheme ,code)))))

  (define-syntax define-suite
    (syntax-rules ()
      ((_ sutname ((casename formal ...) body ...) ...)
       (define sutname `(sutname (casename ,(lambda (formal ...) body ...) 
					   ,(quote (define (casename formal ...) body ...))) ...)))))

  (define-syntax lettest
    (syntax-rules ()
      ((_ ((test nameexp) ...) body ...)
       (let ((test (unittest/wasrun nameexp)) ...) body ...))))

  (define-record unittest/result ran failed)

  (define (unittest/result-summary result)
    `((ran ,(unittest/result-ran result))
      (failed ,(length (unittest/result-failed result))
              ,@(unittest/result-failed result))))

  (define (unittest/result-started! result)
    (unittest/result-ran-set! result (add1 (unittest/result-ran result))))

  (define (unittest/result-failed! result exn)
    (unittest/result-failed-set! result (cons exn (unittest/result-failed result))))

  (define (⊦ pred? a b) (unless (pred? a b) (signal (unittest/condition-expected-actual a b))))
  (define (⊦= a b) (⊦ equal? a b))
  (define (⊦≠ a b) (⊦ (complement equal?) a b))
  (define (⊨ a) (⊦= #t a))
  (define (⊭ a) (⊦= #f a))

  (define-syntax letsuite
    (syntax-rules ()
      ((_ ((name '(method ...)) ...) body ...)
       (letrec ((name (lettest ((method 'method) ...) (list method ...))) ...)
         body ...))))

  (define (unittest/testsuite-run suite r sut)
    (map (lambda (testcase) 
	   (unittest/testcase-run testcase r sut)) 
	 suite))

  (define (unittest/✓ sut)
    (let* ((r (make-unittest/result 0 '()))
	   (sut-name (car sut))
	   (sut-methods (cdr sut))
           (F (lambda (x)
                (let ((name (car x)))
                  (and (not (eq? name 'setup)) 
                       (not (eq? name 'teardown))
                       (not (eq? name 'doc))))))
           (methods (filter F sut-methods))
           (s (map (lambda (pair) (lettest ((t (car pair))) t)) methods)))
      (let ((docs (unittest/testsuite-run s r sut))
	    (res (unittest/result-summary r))
	    (sxml (alist-ref 'doc sut-methods)))
        (if sxml
            (SXML->file! (sxml-tree sut-name 
				    `((h1 (code ,sut-name) " test suite") 
				      (code/scheme ,res)
				      ,@((car sxml) r)
				      (hr)
				      ,@docs))
			 sut-name)
            (pretty-print res))
        r)))

  (define (unittest/condition-expected-actual a b)
    (condition `(exn message "assert-equal failed") 
	       `(unittest-assert-equal comparison ((expected ,a) (got ,b)))))

  (define-syntax ⊦⧳
    (syntax-rules ()
      ((_ ((exn ...) ...) body ...)
       (condition-case (begin body ...)
         ((exn ...) (void)) ...
         ))))

  #;(c () (signal (condition '(exn message "⊦⧳ uncaught condition.")
                             `(uncaught-condition ,c))))

  )








