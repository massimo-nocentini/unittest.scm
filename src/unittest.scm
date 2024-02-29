
(module unittest *

	(import scheme (chicken base))

	(define-record unittest/testcase name log)

	(define (unittest/wasrun name) (make-unittest/testcase name '()))

    (define (unittest/testcase-logcons! testcase msg)
      (unittest/testcase-log-set! testcase (cons msg (unittest/testcase-log testcase))))

	(define (unittest/testcase-run testcase methods)
          (let ((setup (alist-ref 'setup methods))
		(teardown (alist-ref 'teardown methods))) 
            (let-values ((args (if setup 
                                   ((car setup) testcase)
                                   (values))))
              (apply
                (car (alist-ref (unittest/testcase-name testcase) methods))
                testcase args))))

	(define-syntax define-sut
          (syntax-rules ()
            ((_ sutname ((casename formal ...) body ...) ...) 
             (define sutname `((casename ,(lambda (formal ...) body ...)) ...)))))

	(define-syntax lettest
          (syntax-rules ()
            ((_ ((test nameexp) ...) body ...)
             (let ((test (unittest/wasrun nameexp)) ...) body ...))))
	)

