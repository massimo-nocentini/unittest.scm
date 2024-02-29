
(import unittest)

(define-sut wasrun-sut
    ((test-method tc) (unittest/testcase-ran-set! tc #t)))

(define-sut bootstrap-sut
    ((test-running tc)
        (let ((wr (unittest/wasrun 'test-method)))
            (assert (not (unittest/testcase-ran wr)))
            (unittest/testcase-run wr wasrun-sut)
            (assert (unittest/testcase-ran wr)))))

(unittest/testcase-run (unittest/wasrun 'test-running) bootstrap-sut)
