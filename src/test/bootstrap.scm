
(import unittest)

(define-sut wasrun-sut
    ((setup tc)       (unittest/testcase-wassetup-set! tc #t))
    ((test-method tc) (unittest/testcase-ran-set! tc #t)))

(define-sut bootstrap-sut
    ((test-running tc) (lettest ((t 'test-method))
                        (assert (not (unittest/testcase-ran t)))
                        (unittest/testcase-run t wasrun-sut)
                        (assert (unittest/testcase-ran t))))
    ((test-setup tc) (lettest ((t 'test-method))
                        (unittest/testcase-run t wasrun-sut)
                        (assert (unittest/testcase-wassetup t))))                     
)

(lettest ((tr 'test-running)
          (ts 'test-setup))
    (unittest/testcase-run tr bootstrap-sut)
    (unittest/testcase-run ts bootstrap-sut))