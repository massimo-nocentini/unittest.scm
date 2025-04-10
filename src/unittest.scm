
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

  (define (sxml-tree title body)
    (let ((maintitle (if (pair? title) (car title) title))
          (abstract (if (pair? title) (cdr title) '())))
      `((html (@ (xmlns "http://www.w3.org/1999/xhtml")
                 (xml:lang "en") 
                 (lang "en"))
              (head
                (meta (@ (name "viewport") (content "width=device-width,initial-scale=1")))
                (link (@ (rel "stylesheet") 
                         (href "https://www.w3schools.com/w3css/5/w3.css") 
                         (type "text/css")))
                #;(link (@ (rel "stylesheet") 
                           (href "https://www.w3schools.com/lib/w3-theme-blue-grey.css") 
                           (type "text/css")))
                (link (@ (rel "stylesheet") 
                         (href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/" ,highlight-version "/styles/default.min.css") 
                         (type "text/css")))
                (style "code, pre, tt, kbd, samp, .w3-code { font-family: monospace; }"
                       "html, body, h1, h2, h3, h4, h5, h6 { font-family: serif; }")
                (script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/" ,highlight-version "/highlight.min.js")))
                ,@(map (lambda (lang) 
                         `(script (@ (src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/" ,highlight-version "/languages/" ,lang ".min.js")))) 
                    '(scheme python))
                (script "hljs.highlightAll();")
                (title ,maintitle))
              (body (@ (class "w3-content") (style "max-width:61.8%"))
                    (header (@ (class "w3-container w3-center"))
                            (h1 (b ,maintitle))
                            (p ,@abstract)
                            (small
			      (small (p ,(date->string (current-date))))
                              (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/"))
                                 (img (@ (alt "Creative Commons License") (style "border-width:0")
                                         (src "https://mirrors.creativecommons.org/presskit/icons/cc.svg"))))
                              (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/"))
                                 (img (@ (alt "Creative Commons License") (style "border-width:0")
                                         (src "https://mirrors.creativecommons.org/presskit/icons/by.svg"))))
                              (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/"))
                                 (img (@ (alt "Creative Commons License") (style "border-width:0")
                                         (src "https://mirrors.creativecommons.org/presskit/icons/sa.svg"))))
                              (p "This work is licensed under a "
                                 (a (@ (rel "license") (href "http://creativecommons.org/licenses/by-sa/4.0/")) 
                                    "Creative Commons Attribution-ShareAlike 4.0 International License"))))
                    (structure/toc)
                    ,@body
                    (hr)
                    (structure/citations))))))

  (define (SXML->HTML->file! tree filename)
    (with-output-to-file (conc filename ".html")
      (lambda ()
        (letrec ((sections '())
                 (citations '())
                 (sxml-handler-container (lambda (tag body) `(div (@ (class "w3-container")) ,@body)))
                 (sxml-handler-code/pre (lambda (tag body) `(pre (code (@ (class "w3-code w3-round")) ,@body))))
                 (sxml-handler-code/lang (lambda (tag body)
                                           (let ((lang (car body))
                                                 (code (cdr body)))
                                             `(div (@ (class "w3-card w3-round"))
                                                   (header (@ (class "w3-container w3-border w3-round w3-light-gray w3-right")) ,lang " code")
                                                   (pre (@ (class "w3-container"))
                                                        (code (@ (class "w3-code w3-round language-" ,lang)) ,code))))))
                 (sxml-handler-code/scheme (lambda (tag body)
                                             (let* ((expr (if (eq? (length body) 1) (car body) (cons 'begin body))))
                                               (sxml-handler-code/lang
                                                 'code/lang 
                                                 (list 'scheme (call-with-output-string
                                                                 (lambda (p) (pretty-print expr p))))))))
                 (sxml-handler-code/scheme-file (lambda (tag body)
                                                  (sxml-handler-code/scheme 'code/scheme 
                                                                            (list (with-input-from-file (car body) (lambda () (read)))))))
                 (sxml-handler-di (lambda (tag body)
                                    (let ((dt (car body))
                                          (dd (cdr body)))
                                      `(div (@ (class "w3-row")) 
                                            (dt (@ (class "w3-bold")) ,dt)
                                            (dd ,@dd)))))
                 (sxml-handler-structure/section (lambda (tag body)
                                                   (let* ((witness (gensym 'section))
                                                          (i (if (null? sections) 0 (caar sections)))
                                                          (nexti (add1 i)))
                                                     (set! sections (cons (list nexti witness body) sections))
                                                     `(section (@ (id ,witness)) (header (h1 ,nexti ". ",@body))))))
                 (sxml-handler-structure/toc (lambda (tag body)
                                               `(div (@ (class "w3-container"))
                                                     (header (b "Table of contents"))
                                                     (ol ,@(map (lambda (each) `(li (a (@ (href "#" ,(cadr each))) ,@(caddr each))))
                                                             (reverse sections))))))
                 (sxml-handler-structure/citations (lambda (tag body)
                                                     `(div (@ (class "w3-container"))
                                                           (header (b "References"))
                                                           (ul ,@(map (lambda (tuple)
                                                                        (let ((href (third tuple))
                                                                              (rest (fourth tuple)))
                                                                          `(li (@ (id ,(second tuple))) "[" ,(first tuple) "] "
                                                                               (a (@ (href ,href)) ,@(if (null? rest) (list href) rest)))))
                                                                   (reverse citations))))))
                 (sxml-handler-cite/a (lambda (tag body) 
                                        (let* ((i (if (null? citations) 0 (caar citations)))
                                               (nexti (add1 i))
                                               (witness (gensym 'cite))
                                               (href (car body))
                                               (rest (cdr body)))
                                          (set! citations (cons (list nexti witness href rest) citations))
                                          `(cite "[" (a (@ (href "#" ,witness)) ,nexti) "]"))))
                 (sxml-handler-cite/quote (lambda (tag body) 
					    (let ((author (car body))
						  (quotation (cdr body)))
					`(blockquote (@ (class "w3-panel w3-leftbar w3-round") (style "max-width:61.8%"))
						(span (@ (style "font-size:261%;opacity:0.2")) (& "#10077"))
						(br)
						(i ,@quotation)
						(footer (p (@ (class "w3-right-align")) ,author))))))
                 (sxml-handler-math/display (lambda (tag body) `(math (@ (display "block")) ,@body)))
                 (sxml-handler-math/frac (lambda (tag body) `(mfrac ,(car body) ,(cadr body))))
                 (sxml-handler-math/m (lambda (tag body)
                                        (let ((v (car body)))
                                          (cond
                                            ((rational? v) (sxml-handler-math/frac 'frac (list (numerator v) (denominator v))))
                                            ((number? v) `(mn ,v))
                                            ((symbol? v) `(mi ,v))
                                            ((pair? v) `(mrow ,@(map (lambda (w) (sxml-handler-math/m 'm (list w))) v)))
                                            (else `(mtext ,v)))))))
          (display "<!doctype html>")
          (SRV:send-reply
            (pre-post-order*
              (pre-post-order*
                (pre-post-order*
                  tree
                  (append `((container . ,sxml-handler-container)
                            (code/lang . ,sxml-handler-code/lang)
                            (code/pre . ,sxml-handler-code/pre)
                            (code/scheme . ,sxml-handler-code/scheme)
                            (code/scheme-file . ,sxml-handler-code/scheme-file)
                            (cite/a . ,sxml-handler-cite/a)
                            (cite/quote . ,sxml-handler-cite/quote)
                            (structure/section . ,sxml-handler-structure/section)
                            (displaymath . ,sxml-handler-math/display)
                            (m . ,sxml-handler-math/m)
                            (frac . ,sxml-handler-math/frac)
                            (di . ,sxml-handler-di))
                          alist-conv-rules*))
                (append `((structure/toc . ,sxml-handler-structure/toc)
                          (structure/citations . ,sxml-handler-structure/citations))
                        alist-conv-rules*))
              universal-conversion-rules*))))))

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
        (let* ((witness (gensym))
               (no-outsrt "")
               (pair (condition-case (let* ((res (void))
                                            (outstr (with-output-to-string (lambda () (set! res (apply f testcase args))))))
                                       (cons res outstr))
                       (c (exn unittest-assert-equal) 
                          (begin
                            (unittest/result-failed!
                              result (cons testcase-name (get-condition-property c 'unittest-assert-equal 'comparison)))
                            (cons witness no-outsrt)))
                       (c (exn)
                          (begin
                            (unittest/result-failed!
                              result (list testcase-name (call-with-output-string
                                                           (lambda (port) (print-error-message c port)))))
                            (cons witness no-outsrt)))
                       (c () (begin
                               (unittest/result-failed! result (list testcase-name c))
                               (cons witness no-outsrt)))))
               (v (car pair))
               (outstr (cdr pair))
               (hasdoc (and (pair? v) (eq? (car v) 'doc))))
          (when teardown (apply (car teardown) testcase args))
          `((structure/section (code ,testcase-name)
                               ": " 
                               ,(if (eq? v witness) 
                                    '(span (@ (class "w3-text-red")) fail) 
                                    '(span (@ (class "w3-text-green")) pass)))
            ,@(if hasdoc (cdr v) '())
            (code/scheme ,(if hasdoc (butlast code) code))
            ,@(if (not (equal? outstr no-outsrt)) 
                  `((p "Captured stdout:")
                    (code/pre ,outstr))
                  '()))))))

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
        (SXML->HTML->file! (sxml-tree sut-name 
                                      `((h1 (code ,sut-name) " test suite") 
                                        (code/scheme ,res)
                                        ,@(if sxml ((car sxml) r) '())
                                        (hr)
                                        ,@docs))
                           (symbol-append 'testsuite- sut-name))
        (pretty-print res)
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

  )



