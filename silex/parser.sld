(define-library (silex parser)
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

  (export parser)

  (include "util.scm")
  (include "tables/action.scm")
  (include "tables/class.scm")
  (include "tables/macro.scm")
  (include "tables/regexp.scm")
  (include "tables/string.scm")
  (include "multilex.scm")
  (include "lexparser.scm"))
