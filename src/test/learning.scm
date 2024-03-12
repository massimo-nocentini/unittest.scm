

(import unittest (chicken base) (chicken pretty-print) (chicken condition) (chicken foreign) (chicken gc))


(define-suite learning-suite

    ((test/len _) (let ((my-strlen (foreign-lambda* int ((scheme-object cons)) "C_return(C_header_size(cons));")))
                    (⊦= 2 (my-strlen (cons 1 '())))
                    (⊦= 2 (my-strlen (cons 1 (cons 2 (cons 3 '())))))
                    (⊦= 11 (my-strlen "hello world"))))

)

(unittest/✓ learning-suite)