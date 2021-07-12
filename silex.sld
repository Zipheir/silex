(define-library (silex)
  (import (scheme base)
          (scheme char)
          (scheme file)
          (scheme write)
          (srfi 13))

  ;; exports TODO

  (include "tables/action.scm")
  (include "tables/class.scm")
  (include "tables/macro.scm")
  (include "tables/regexp.scm")
  (include "tables/string.scm")
  (include "silex.scm")
  (include "output.scm")
  (include "output2.scm")
  )
