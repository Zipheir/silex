(define-library (silex output)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme write))

  (cond-expand
    ((library (srfi 162))
     (import (srfi 162)))
    ((library (srfi 130))
     (import (srfi 130)))
    ((library (srfi 13))
     (import (srfi 13)))
    (else
     (begin (error "No string library found."))))

  (export prep-set-rules-yytext? output)

  (include "util.scm")
  (include "prep.scm")
  (include "output2.scm")
  (include "output.scm"))
