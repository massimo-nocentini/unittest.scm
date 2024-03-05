
(import unittest (chicken pretty-print) (chicken condition))

(define-sut wasrun-sut
  ((setup tc)         (unittest/testcase-logcons! tc 'setup))
  ((teardown tc _)    (unittest/testcase-logcons! tc 'teardown))
  ((test-method tc _) (unittest/testcase-logcons! tc 'test-method))
  ((test-broken tc _) (signal (unittest/condition-expected-actual 'useless '_))))

(define-sut bootstrap-sut
  ((setup tc) (lettest ((t 'test-method)) (values t (make-unittest/result 0 '()))))
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
            (assert-equal '((ran 1) (failed 1 (test-broken (expected useless) (got _)))) (unittest/result-summary r))))
  ((test-failed-result tc _ r)
      (unittest/result-started! r)
      (unittest/result-failed! r 'no-reason)
      (assert-equal '((ran 1) (failed 1 no-reason)) (unittest/result-summary r)))
  ((test-suite tc _ r)
    (letsuite ((suite '(test-running test-failed)))
      (unittest/testsuite-run suite r bootstrap-sut)
      (assert-equal '((ran 2) (failed 0)) (unittest/result-summary r)))))

(let* ((r (unittest/test-sut bootstrap-sut)) ; do run the tests.
       (s (unittest/result-summary r)))
    #;(assert-equal '((ran 5) (failed 0)) s)
    (pretty-print s))
