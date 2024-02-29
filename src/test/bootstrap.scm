
(import unittest)

(define-sut wasrun-sut
  ((setup tc)         (unittest/testcase-logcons! tc 'setup))
  ((test-method tc _) (unittest/testcase-logcons! tc 'test-method)))

(define-sut bootstrap-sut
  ((setup tc) (lettest ((t 'test-method)) t))
  ((test-running tc t)
     (assert (equal? '() (unittest/testcase-log t)))
     (unittest/testcase-run t wasrun-sut)
     (assert (equal? '(test-method setup) (unittest/testcase-log t)))))

(lettest ((tr 'test-running))
	 (unittest/testcase-run tr bootstrap-sut))
