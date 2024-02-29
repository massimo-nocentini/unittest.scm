
(module unittest *

    (import scheme (chicken base))

    (define-record unittest/testcase ran name wassetup)

    (define (unittest/wasrun name)
       (make-unittest/testcase #f name #f))

    (define (unittest/testcase-run testcase methods)
        (let ((setup (alist-ref 'setup methods))
              (teardown (alist-ref 'teardown methods))) 
            (when setup ((car setup) testcase)))
        ((car (alist-ref (unittest/testcase-name testcase) methods)) testcase))

    (define-syntax define-sut
        (syntax-rules ()
            ((_ sutname ((casename formal ...) body ...) ...) 
             (define sutname `((casename ,(lambda (formal ...) body ... (void))) ...)))))

    (define-syntax lettest
        (syntax-rules ()
            ((_ ((test nameexp) ...) body ...)
             (let ((test (unittest/wasrun nameexp)) ...) body ...))))
)

