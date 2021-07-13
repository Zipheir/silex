(define-library (silex dfa)
  (import (scheme base))

  (export sweep noeps)

  (include "util.scm")
  (include "noeps.scm")
  (include "sweep.scm"))
