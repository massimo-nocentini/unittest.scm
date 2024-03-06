
(module unittest *

	(import scheme srfi-1 (chicken base) (chicken condition) (chicken pretty-print) (chicken port))

	(define-record unittest/testcase name log)

	(define (unittest/wasrun name) (make-unittest/testcase name '()))

	(define (unittest/testcase-logcons! testcase msg)
	  (unittest/testcase-log-set! testcase (cons msg (unittest/testcase-log testcase))))

	(define (unittest/testcase-run testcase result methods)
          (let ((setup (alist-ref 'setup methods))
		        (teardown (alist-ref 'teardown methods))
                (testcase-name (unittest/testcase-name testcase)))
            (unittest/result-started! result)
            (let-values ((args (if setup ((car setup) testcase) (values))))
              (condition-case (apply (car (alist-ref testcase-name methods)) testcase args)
                (c (exn unittest-assert-equal) (unittest/result-failed! result (cons testcase-name (get-condition-property c 'unittest-assert-equal 'comparison))))
                (c (exn) (unittest/result-failed! result (list testcase-name (get-condition-property c 'exn 'message))))
                (c () (unittest/result-failed! result (list testcase-name c))))
              (when teardown (apply (car teardown) testcase args)))))
        
	(define-syntax define-sut
          (syntax-rules ()
            ((_ sutname ((casename formal ...) body ...) ...)
             (define sutname `((casename ,(lambda (formal ...) body ...)) ...)))))

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

	(define-syntax letsuite
	  (syntax-rules ()
            ((_ ((name '(method ...)) ...) body ...)
             (letrec ((name (lettest ((method 'method) ...) (list method ...))) ...)
               body ...))))

	(define (unittest/testsuite-run suite r sut)
	  (for-each (lambda (testcase) (unittest/testcase-run testcase r sut)) suite))

	(define (unittest/✓-sut sut)
	  (let* ((r (make-unittest/result 0 '()))
		     (methods (filter (lambda (x) (and (not (eq? (car x) 'setup)) (not (eq? (car x) 'teardown)))) sut))
		     (s (map (lambda (pair) (lettest ((t (car pair))) t)) methods)))
        (unittest/testsuite-run s r sut)
        (pretty-print (unittest/result-summary r))
        r))

	(define (unittest/condition-expected-actual a b)
          (condition `(exn message "assert-equal failed") `(unittest-assert-equal comparison ((expected ,a) (got ,b)))))
	)

