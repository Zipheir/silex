(define-library (silex output)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme write))

  (export prep-set-rules-yytext? output)

  (include "util.scm")
  (include "prep.scm")
  (include "output2.scm")
  (include "output.scm"))
