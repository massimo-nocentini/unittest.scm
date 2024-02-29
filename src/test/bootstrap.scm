
(import unittest)

(define-sut wasrun-sut
    ((setup tc)         (unittest/testcase-wassetup-set! tc #t))
    ((test-method tc _) (unittest/testcase-ran-set! tc #t)))

(define-sut bootstrap-sut
    ((setup tc) (lettest ((t 'test-method)) t))
    ((test-running tc t) 
                        (assert (not (unittest/testcase-ran t)))
                        (unittest/testcase-run t wasrun-sut)
                        (assert (unittest/testcase-ran t)))
    ((test-setup tc t) 
                        (unittest/testcase-run t wasrun-sut)
                        (assert (unittest/testcase-wassetup t))))


(lettest ((tr 'test-running)
          (ts 'test-setup))
    (unittest/testcase-run tr bootstrap-sut)
    (unittest/testcase-run ts bootstrap-sut))