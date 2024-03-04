
(module unittest *

	(import scheme srfi-1 (chicken base) (chicken condition) (chicken pretty-print) (chicken port))

	(define-record unittest/testcase name log)

	(define (unittest/wasrun name) (make-unittest/testcase name '()))

    (define (unittest/testcase-logcons! testcase msg)
      (unittest/testcase-log-set! testcase (cons msg (unittest/testcase-log testcase))))

	(define (unittest/testcase-run testcase result methods)
          (let ((setup (alist-ref 'setup methods))
		        (teardown (alist-ref 'teardown methods)))
            (unittest/result-started! result)
            (let-values ((args (if setup ((car setup) testcase) (values))))
                (handle-exceptions exn 
                    (unittest/result-failed! result (call-with-output-string (lambda (port) (print-error-message exn port))))
                    (apply (car (alist-ref (unittest/testcase-name testcase) methods)) testcase args))
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
        (failed ,(length (unittest/result-failed result)) ,@(unittest/result-failed result))))

    (define (unittest/result-started! result)
      (unittest/result-ran-set! result (add1 (unittest/result-ran result))))

    (define (unittest/result-failed! result exn)
      (unittest/result-failed-set! result (cons exn (unittest/result-failed result))))

    (define-syntax assert-equal
      (syntax-rules ()
        ((_ a b err ...) (assert (equal? a b) err ... `((expected ,a) (got ,b))))))

    (define-syntax letsuite
      (syntax-rules ()
        ((_ ((name '(method ...)) ...) body ...)
         (letrec ((name (lettest ((method 'method) ...) (list method ...))) ...)
            body ...))))

    (define (unittest/testsuite-run suite r sut)
      (map (lambda (testcase) (unittest/testcase-run testcase r sut)) suite))

    (define (unittest/test-sut sut)
      (let ((r (make-unittest/result 0 '()))
            (methods (filter (lambda (x) (and (not (eq? (car x) 'setup)) (not (eq? (car x) 'teardown)))) sut)))
        (let ((s (map (lambda (pair) (lettest ((t (car pair))) t)) methods)))
            (unittest/testsuite-run s r sut))
        r))
)

