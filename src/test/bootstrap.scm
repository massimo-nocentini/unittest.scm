
(import unittest (chicken pretty-print))

(define-sut wasrun-sut
  ((setup tc)         (unittest/testcase-logcons! tc 'setup))
  ((teardown tc _)    (unittest/testcase-logcons! tc 'teardown))
  ((test-method tc _) (unittest/testcase-logcons! tc 'test-method))
  ((test-broken tc _) (error 'broken)))

(define-sut bootstrap-sut
  ((setup tc) (lettest ((t 'test-method)) (values t (make-unittest/result 0 0))))
  ((test-running tc t r)
     (assert-equal '() (unittest/testcase-log t))
     (unittest/testcase-run t r wasrun-sut)
     (assert-equal '(teardown test-method setup) (unittest/testcase-log t)))
  ((test-result tc t r)
        (unittest/testcase-run t r wasrun-sut)
        (assert-equal '((ran 1) (failed 0)) (unittest/result-summary r)))
  ((test-failed tc _ r)
        (lettest ((t 'test-broken))
            (unittest/testcase-run t r wasrun-sut)
            (assert-equal '((ran 1) (failed 1)) (unittest/result-summary r))))
  ((test-failed-result tc _ r)    
      (unittest/result-started! r)
      (unittest/result-failed! r)
      (assert-equal '((ran 1) (failed 1)) (unittest/result-summary r)))
  ((test-suite tc _ r)
    (letsuite ((suite (test-running test-failed)))
      (suite r bootstrap-sut)
      (assert-equal '((ran 2) (failed 0)) (unittest/result-summary r))))

)

(let ((r (make-unittest/result 0 0))
      (expected '((ran 5) (failed 0))))
    (letsuite ((suite (test-running test-result test-failed test-failed-result test-suite)))
      (suite r bootstrap-sut)
      (assert-equal expected (unittest/result-summary r))
      (pretty-print (unittest/result-summary r))))