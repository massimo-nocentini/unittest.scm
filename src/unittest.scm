
(module unittest *

    (import scheme (chicken base))

    (define-record unittest/testcase ran name)

    (define (unittest/wasrun name)
       (make-unittest/testcase #f name))

    (define (unittest/testcase-run testcase methods)
       ((car (alist-ref (unittest/testcase-name testcase) methods)) testcase))

    (define-syntax define-sut
        (syntax-rules (setup teardown)
            ((_ sutname ((casename tc) body ...) ...) 
             (define sutname `((casename ,(lambda (tc) body ... (void))) ...)))))
)

