;; Copyright (C) 1997 Danny Dubé, Université de Montréal.
;; All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;;   Redistributions in binary form must reproduce the above
;;;   copyright notice, this list of conditions and the following
;;;   disclaimer in the documentation and/or other materials provided
;;;   with the distribution.
;;;
;;;   Neither the name of the author nor the names of its contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;; PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
;;; HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;; TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(define (string-append-list lst)
  (let loop1 ((n 0) (x lst) (y '()))
    (if (pair? x)
      (let ((s (car x)))
        (loop1 (+ n (string-length s)) (cdr x) (cons s y)))
      (let ((result (make-string n #\space)))
        (let loop2 ((k (- n 1)) (y y))
          (if (pair? y)
            (let ((s (car y)))
              (let loop3 ((i k) (j (- (string-length s) 1)))
                (if (not (< j 0))
                  (begin
                    (string-set! result i (string-ref s j))
                    (loop3 (- i 1) (- j 1)))
                  (loop2 i (cdr y)))))
            result))))))

;
; Quelques definitions de constantes
;

(define eof-tok              0)
(define hblank-tok           1)
(define vblank-tok           2)
(define pipe-tok             3)
(define question-tok         4)
(define plus-tok             5)
(define star-tok             6)
(define lpar-tok             7)
(define rpar-tok             8)
(define dot-tok              9)
(define lbrack-tok          10)
(define lbrack-rbrack-tok   11)
(define lbrack-caret-tok    12)
(define lbrack-minus-tok    13)
(define subst-tok           14)
(define power-tok           15)
(define doublequote-tok     16)
(define char-tok            17)
(define caret-tok           18)
(define dollar-tok          19)
(define <<EOF>>-tok         20)
(define <<ERROR>>-tok       21)
(define percent-percent-tok 22)
(define id-tok              23)
(define rbrack-tok          24)
(define minus-tok           25)
(define illegal-tok         26)
; Tokens agreges
(define class-tok           27)
(define string-tok          28)

(define number-of-tokens 29)

(define newline-ch   (char->integer #\newline))
(define tab-ch       (char->integer #\  ))
(define dollar-ch    (char->integer #\$))
(define minus-ch     (char->integer #\-))
(define rbrack-ch    (char->integer #\]))
(define caret-ch     (char->integer #\^))

(define dot-class (list (cons 'inf- (- newline-ch 1))
                        (cons (+ newline-ch 1) 'inf+)))

(define default-action
  (string-append "        (yycontinue)" (string #\newline)))
(define default-<<EOF>>-action
  (string-append "       '(0)" (string #\newline)))
(define default-<<ERROR>>-action
  (string-append "       (begin"
                 (string #\newline)
                 "         (display \"Error: Invalid token.\")"
                 (string #\newline)
                 "         (newline)"
                 (string #\newline)
                 "         'error)"
                 (string #\newline)))

;
; Fabrication de tables de dispatch
;

(define make-dispatch-table
  (lambda (size alist default)
    (let ((v (make-vector size default)))
      (let loop ((alist alist))
        (if (null? alist)
            v
            (begin
              (vector-set! v (caar alist) (cdar alist))
              (loop (cdr alist))))))))

;
; Fonctions de manipulation des tokens
;

(define make-tok
  (lambda (tok-type lexeme line column . attr)
    (cond ((null? attr)
           (vector tok-type line column lexeme))
          ((null? (cdr attr))
           (vector tok-type line column lexeme (car attr)))
          (else
           (vector tok-type line column lexeme (car attr) (cadr attr))))))

(define get-tok-type     (lambda (tok) (vector-ref tok 0)))
(define get-tok-line     (lambda (tok) (vector-ref tok 1)))
(define get-tok-column   (lambda (tok) (vector-ref tok 2)))
(define get-tok-lexeme   (lambda (tok) (vector-ref tok 3)))
(define get-tok-attr     (lambda (tok) (vector-ref tok 4)))
(define get-tok-2nd-attr (lambda (tok) (vector-ref tok 5)))

;
; Fonctions de manipulations des regles
;

(define make-rule
  (lambda (line eof? error? bol? eol? regexp action)
    (vector line eof? error? bol? eol? regexp action #f)))

(define get-rule-line    (lambda (rule) (vector-ref rule 0)))
(define get-rule-eof?    (lambda (rule) (vector-ref rule 1)))
(define get-rule-error?  (lambda (rule) (vector-ref rule 2)))
(define get-rule-bol?    (lambda (rule) (vector-ref rule 3)))
(define get-rule-eol?    (lambda (rule) (vector-ref rule 4)))
(define get-rule-regexp  (lambda (rule) (vector-ref rule 5)))
(define get-rule-action  (lambda (rule) (vector-ref rule 6)))
(define get-rule-yytext? (lambda (rule) (vector-ref rule 7)))

(define set-rule-regexp  (lambda (rule regexp)  (vector-set! rule 5 regexp)))
(define set-rule-action  (lambda (rule action)  (vector-set! rule 6 action)))
(define set-rule-yytext? (lambda (rule yytext?) (vector-set! rule 7 yytext?)))

;
; Noeuds des regexp
;

(define epsilon-re  0)
(define or-re       1)
(define conc-re     2)
(define star-re     3)
(define plus-re     4)
(define question-re 5)
(define class-re    6)
(define char-re     7)

(define make-re
  (lambda (re-type . lattr)
    (cond ((null? lattr)
           (vector re-type))
          ((null? (cdr lattr))
           (vector re-type (car lattr)))
          ((null? (cddr lattr))
           (vector re-type (car lattr) (cadr lattr))))))

(define get-re-type  (lambda (re) (vector-ref re 0)))
(define get-re-attr1 (lambda (re) (vector-ref re 1)))
(define get-re-attr2 (lambda (re) (vector-ref re 2)))

;
; Fonctions de manipulation des ensembles d'etats
;

; Intersection de deux ensembles d'etats
(define ss-inter
  (lambda (ss1 ss2)
    (cond ((null? ss1)
           '())
          ((null? ss2)
           '())
          (else
           (let ((t1 (car ss1))
                 (t2 (car ss2)))
             (cond ((< t1 t2)
                    (ss-inter (cdr ss1) ss2))
                   ((= t1 t2)
                    (cons t1 (ss-inter (cdr ss1) (cdr ss2))))
                   (else
                    (ss-inter ss1 (cdr ss2)))))))))

; Difference entre deux ensembles d'etats
(define ss-diff
  (lambda (ss1 ss2)
    (cond ((null? ss1)
           '())
          ((null? ss2)
           ss1)
          (else
           (let ((t1 (car ss1))
                 (t2 (car ss2)))
             (cond ((< t1 t2)
                    (cons t1 (ss-diff (cdr ss1) ss2)))
                   ((= t1 t2)
                    (ss-diff (cdr ss1) (cdr ss2)))
                   (else
                    (ss-diff ss1 (cdr ss2)))))))))

; Union de deux ensembles d'etats
(define ss-union
  (lambda (ss1 ss2)
    (cond ((null? ss1)
           ss2)
          ((null? ss2)
           ss1)
          (else
           (let ((t1 (car ss1))
                 (t2 (car ss2)))
             (cond ((< t1 t2)
                    (cons t1 (ss-union (cdr ss1) ss2)))
                   ((= t1 t2)
                    (cons t1 (ss-union (cdr ss1) (cdr ss2))))
                   (else
                    (cons t2 (ss-union ss1 (cdr ss2))))))))))

; Decoupage de deux ensembles d'etats
(define ss-sep
  (lambda (ss1 ss2)
    (let loop ((ss1 ss1) (ss2 ss2) (l '()) (c '()) (r '()))
      (if (null? ss1)
          (if (null? ss2)
              (vector (reverse l) (reverse c) (reverse r))
              (loop ss1 (cdr ss2) l c (cons (car ss2) r)))
          (if (null? ss2)
              (loop (cdr ss1) ss2 (cons (car ss1) l) c r)
              (let ((t1 (car ss1))
                    (t2 (car ss2)))
                (cond ((< t1 t2)
                       (loop (cdr ss1) ss2 (cons t1 l) c r))
                      ((= t1 t2)
                       (loop (cdr ss1) (cdr ss2) l (cons t1 c) r))
                      (else
                       (loop ss1 (cdr ss2) l c (cons t2 r))))))))))

;
; Fonctions de manipulation des classes de caracteres
;

; Comparaisons de bornes d'intervalles
(define class-= eqv?)

(define class-<=
  (lambda (b1 b2)
    (cond ((eq? b1 'inf-) #t)
          ((eq? b2 'inf+) #t)
          ((eq? b1 'inf+) #f)
          ((eq? b2 'inf-) #f)
          (else (<= b1 b2)))))

(define class->=
  (lambda (b1 b2)
    (cond ((eq? b1 'inf+) #t)
          ((eq? b2 'inf-) #t)
          ((eq? b1 'inf-) #f)
          ((eq? b2 'inf+) #f)
          (else (>= b1 b2)))))

(define class-<
  (lambda (b1 b2)
    (cond ((eq? b1 'inf+) #f)
          ((eq? b2 'inf-) #f)
          ((eq? b1 'inf-) #t)
          ((eq? b2 'inf+) #t)
          (else (< b1 b2)))))

(define class->
  (lambda (b1 b2)
    (cond ((eq? b1 'inf-) #f)
          ((eq? b2 'inf+) #f)
          ((eq? b1 'inf+) #t)
          ((eq? b2 'inf-) #t)
          (else (> b1 b2)))))

; Complementation d'une classe
(define class-compl
  (lambda (c)
    (let loop ((c c) (start 'inf-))
      (if (null? c)
          (list (cons start 'inf+))
          (let* ((r (car c))
                 (rstart (car r))
                 (rend (cdr r)))
            (if (class-< start rstart)
                (cons (cons start (- rstart 1))
                      (loop c rstart))
                (if (class-< rend 'inf+)
                    (loop (cdr c) (+ rend 1))
                    '())))))))

; Union de deux classes de caracteres
(define class-union
  (lambda (c1 c2)
    (let loop ((c1 c1) (c2 c2) (u '()))
      (if (null? c1)
          (if (null? c2)
              (reverse u)
              (loop c1 (cdr c2) (cons (car c2) u)))
          (if (null? c2)
              (loop (cdr c1) c2 (cons (car c1) u))
              (let* ((r1 (car c1))
                     (r2 (car c2))
                     (r1start (car r1))
                     (r1end (cdr r1))
                     (r2start (car r2))
                     (r2end (cdr r2)))
                (if (class-<= r1start r2start)
                    (cond ((class-= r1end 'inf+)
                           (loop c1 (cdr c2) u))
                          ((class-< (+ r1end 1) r2start)
                           (loop (cdr c1) c2 (cons r1 u)))
                          ((class-<= r1end r2end)
                           (loop (cdr c1)
                                 (cons (cons r1start r2end) (cdr c2))
                                 u))
                          (else
                           (loop c1 (cdr c2) u)))
                    (cond ((class-= r2end 'inf+)
                           (loop (cdr c1) c2 u))
                          ((class-> r1start (+ r2end 1))
                           (loop c1 (cdr c2) (cons r2 u)))
                          ((class->= r1end r2end)
                           (loop (cons (cons r2start r1end) (cdr c1))
                                 (cdr c2)
                                 u))
                          (else
                           (loop (cdr c1) c2 u))))))))))

; Decoupage de deux classes de caracteres
(define class-sep
  (lambda (c1 c2)
    (let loop ((c1 c1) (c2 c2) (l '()) (c '()) (r '()))
      (if (null? c1)
          (if (null? c2)
              (vector (reverse l) (reverse c) (reverse r))
              (loop c1 (cdr c2) l c (cons (car c2) r)))
          (if (null? c2)
              (loop (cdr c1) c2 (cons (car c1) l) c r)
              (let* ((r1 (car c1))
                     (r2 (car c2))
                     (r1start (car r1))
                     (r1end (cdr r1))
                     (r2start (car r2))
                     (r2end (cdr r2)))
                (cond ((class-< r1start r2start)
                       (if (class-< r1end r2start)
                           (loop (cdr c1) c2 (cons r1 l) c r)
                           (loop (cons (cons r2start r1end) (cdr c1)) c2
                                 (cons (cons r1start (- r2start 1)) l) c r)))
                      ((class-> r1start r2start)
                       (if (class-> r1start r2end)
                           (loop c1 (cdr c2) l c (cons r2 r))
                           (loop c1 (cons (cons r1start r2end) (cdr c2))
                                 l c (cons (cons r2start (- r1start 1)) r))))
                      (else
                       (cond ((class-< r1end r2end)
                              (loop (cdr c1)
                                    (cons (cons (+ r1end 1) r2end) (cdr c2))
                                    l (cons r1 c) r))
                             ((class-= r1end r2end)
                              (loop (cdr c1) (cdr c2) l (cons r1 c) r))
                             (else
                              (loop (cons (cons (+ r2end 1) r1end) (cdr c1))
                                    (cdr c2)
                                    l (cons r2 c) r)))))))))))

; Transformer une classe (finie) de caracteres en une liste de ...
(define class->char-list
  (lambda (c)
    (let loop1 ((c c))
      (if (null? c)
          '()
          (let* ((r (car c))
                 (rend (cdr r))
                 (tail (loop1 (cdr c))))
            (let loop2 ((rstart (car r)))
              (if (<= rstart rend)
                  (cons (integer->char rstart) (loop2 (+ rstart 1)))
                  tail)))))))

; Transformer une classe de caracteres en une liste poss. compl.
; 1er element = #t -> classe complementee
(define class->tagged-char-list
  (lambda (c)
    (let* ((finite? (or (null? c) (number? (caar c))))
           (c2 (if finite? c (class-compl c)))
           (c-l (class->char-list c2)))
      (cons (not finite?) c-l))))

;
; Fonction digraph
;

; Fonction "digraph".
; Etant donne un graphe dirige dont les noeuds comportent une valeur,
; calcule pour chaque noeud la "somme" des valeurs contenues dans le
; noeud lui-meme et ceux atteignables a partir de celui-ci.  La "somme"
; consiste a appliquer un operateur commutatif et associatif aux valeurs
; lorsqu'elles sont additionnees.
; L'entree consiste en un vecteur de voisinages externes, un autre de
; valeurs initiales et d'un operateur.
; La sortie est un vecteur de valeurs finales.
(define digraph
  (lambda (arcs init op)
    (let* ((nbnodes (vector-length arcs))
           (infinity nbnodes)
           (prio (make-vector nbnodes -1))
           (stack (make-vector nbnodes #f))
           (sp 0)
           (final (make-vector nbnodes #f)))
      (letrec ((store-final
                (lambda (self-sp value)
                  (let loop ()
                    (if (> sp self-sp)
                        (let ((voisin (vector-ref stack (- sp 1))))
                          (vector-set! prio voisin infinity)
                          (set! sp (- sp 1))
                          (vector-set! final voisin value)
                          (loop))))))
               (visit-node
                (lambda (n)
                  (let ((self-sp sp))
                    (vector-set! prio n self-sp)
                    (vector-set! stack sp n)
                    (set! sp (+ sp 1))
                    (vector-set! final n (vector-ref init n))
                    (let loop ((vois (vector-ref arcs n)))
                      (if (pair? vois)
                          (let* ((v (car vois))
                                 (vprio (vector-ref prio v)))
                            (if (= vprio -1)
                                (visit-node v))
                            (vector-set! prio n (min (vector-ref prio n)
                                                     (vector-ref prio v)))
                            (vector-set! final n (op (vector-ref final n)
                                                     (vector-ref final v)))
                            (loop (cdr vois)))))
                    (if (= (vector-ref prio n) self-sp)
                        (store-final self-sp (vector-ref final n)))))))
        (let loop ((n 0))
          (if (< n nbnodes)
              (begin
                (if (= (vector-ref prio n) -1)
                    (visit-node n))
                (loop (+ n 1)))))
        final))))

;
; Fonction de tri
;

(define merge-sort-merge
  (lambda (l1 l2 cmp-<=)
    (cond ((null? l1)
           l2)
          ((null? l2)
           l1)
          (else
           (let ((h1 (car l1))
                 (h2 (car l2)))
             (if (cmp-<= h1 h2)
                 (cons h1 (merge-sort-merge (cdr l1) l2 cmp-<=))
                 (cons h2 (merge-sort-merge l1 (cdr l2) cmp-<=))))))))

(define merge-sort
  (lambda (l cmp-<=)
    (if (null? l)
        l
        (let loop1 ((ll (map list l)))
          (if (null? (cdr ll))
              (car ll)
              (loop1
               (let loop2 ((ll ll))
                 (cond ((null? ll)
                        ll)
                       ((null? (cdr ll))
                        ll)
                       (else
                        (cons (merge-sort-merge (car ll) (cadr ll) cmp-<=)
                              (loop2 (cddr ll))))))))))))

; Module re2nfa.scm.
; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

; Le vecteur d'etats contient la table de transition du nfa.
; Chaque entree contient les arcs partant de l'etat correspondant.
; Les arcs sont stockes dans une liste.
; Chaque arc est une paire (class . destination).
; Les caracteres d'une classe sont enumeres par ranges.
; Les ranges sont donnes dans une liste,
;   chaque element etant une paire (debut . fin).
; Le symbole eps peut remplacer une classe.
; L'acceptation est decrite par une paire (acc-if-eol . acc-if-no-eol).

; Quelques variables globales
(define r2n-counter 0)
(define r2n-v-arcs '#(#f))
(define r2n-v-acc '#(#f))
(define r2n-v-len 1)

; Initialisation des variables globales
(define r2n-init
  (lambda ()
    (set! r2n-counter 0)
    (set! r2n-v-arcs (vector '()))
    (set! r2n-v-acc (vector #f))
    (set! r2n-v-len 1)))

; Agrandissement des vecteurs
(define r2n-extend-v
  (lambda ()
    (let* ((new-len (* 2 r2n-v-len))
           (new-v-arcs (make-vector new-len '()))
           (new-v-acc (make-vector new-len #f)))
      (let loop ((i 0))
        (if (< i r2n-v-len)
            (begin
              (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
              (vector-set! new-v-acc i (vector-ref r2n-v-acc i))
              (loop (+ i 1)))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len new-len))))

; Finalisation des vecteurs
(define r2n-finalize-v
  (lambda ()
    (let* ((new-v-arcs (make-vector r2n-counter))
           (new-v-acc (make-vector r2n-counter)))
      (let loop ((i 0))
        (if (< i r2n-counter)
            (begin
              (vector-set! new-v-arcs i (vector-ref r2n-v-arcs i))
              (vector-set! new-v-acc i (vector-ref r2n-v-acc i))
              (loop (+ i 1)))))
      (set! r2n-v-arcs new-v-arcs)
      (set! r2n-v-acc new-v-acc)
      (set! r2n-v-len r2n-counter))))

; Creation d'etat
(define r2n-get-state
  (lambda (acc)
    (if (= r2n-counter r2n-v-len)
        (r2n-extend-v))
    (let ((state r2n-counter))
      (set! r2n-counter (+ r2n-counter 1))
      (vector-set! r2n-v-acc state (or acc (cons #f #f)))
      state)))

; Ajout d'un arc
(define r2n-add-arc
  (lambda (start chars end)
    (vector-set! r2n-v-arcs
                 start
                 (cons (cons chars end) (vector-ref r2n-v-arcs start)))))

; Construction de l'automate a partir des regexp
(define r2n-build-epsilon
  (lambda (re start end)
    (r2n-add-arc start 'eps end)))

(define r2n-build-or
  (lambda (re start end)
    (let ((re1 (get-re-attr1 re))
          (re2 (get-re-attr2 re)))
      (r2n-build-re re1 start end)
      (r2n-build-re re2 start end))))

(define r2n-build-conc
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
           (re2 (get-re-attr2 re))
           (inter (r2n-get-state #f)))
      (r2n-build-re re1 start inter)
      (r2n-build-re re2 inter end))))

(define r2n-build-star
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
           (inter1 (r2n-get-state #f))
           (inter2 (r2n-get-state #f)))
      (r2n-add-arc start 'eps inter1)
      (r2n-add-arc inter1 'eps inter2)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter2 inter1))))

(define r2n-build-plus
  (lambda (re start end)
    (let* ((re1 (get-re-attr1 re))
           (inter1 (r2n-get-state #f))
           (inter2 (r2n-get-state #f)))
      (r2n-add-arc start 'eps inter1)
      (r2n-add-arc inter2 'eps inter1)
      (r2n-add-arc inter2 'eps end)
      (r2n-build-re re1 inter1 inter2))))

(define r2n-build-question
  (lambda (re start end)
    (let ((re1 (get-re-attr1 re)))
      (r2n-add-arc start 'eps end)
      (r2n-build-re re1 start end))))

(define r2n-build-class
  (lambda (re start end)
    (let ((class (get-re-attr1 re)))
      (r2n-add-arc start class end))))

(define r2n-build-char
  (lambda (re start end)
    (let* ((c (get-re-attr1 re))
           (class (list (cons c c))))
      (r2n-add-arc start class end))))

(define r2n-build-re
  (let ((sub-function-v (vector r2n-build-epsilon
                                r2n-build-or
                                r2n-build-conc
                                r2n-build-star
                                r2n-build-plus
                                r2n-build-question
                                r2n-build-class
                                r2n-build-char)))
    (lambda (re start end)
      (let* ((re-type (get-re-type re))
             (sub-f (vector-ref sub-function-v re-type)))
        (sub-f re start end)))))

; Construction de l'automate relatif a une regle
(define r2n-build-rule
  (lambda (rule ruleno nl-start no-nl-start)
    (let* ((re (get-rule-regexp rule))
           (bol? (get-rule-bol? rule))
           (eol? (get-rule-eol? rule))
           (rule-start (r2n-get-state #f))
           (rule-end (r2n-get-state (if eol?
                                        (cons ruleno #f)
                                        (cons ruleno ruleno)))))
      (r2n-build-re re rule-start rule-end)
      (r2n-add-arc nl-start 'eps rule-start)
      (if (not bol?)
          (r2n-add-arc no-nl-start 'eps rule-start)))))

; Construction de l'automate complet
(define re2nfa
  (lambda (rules)
    (let ((nb-of-rules (vector-length rules)))
      (r2n-init)
      (let* ((nl-start (r2n-get-state #f))
             (no-nl-start (r2n-get-state #f)))
        (let loop ((i 0))
          (if (< i nb-of-rules)
              (begin
                (r2n-build-rule (vector-ref rules i)
                                i
                                nl-start
                                no-nl-start)
                (loop (+ i 1)))))
        (r2n-finalize-v)
        (let ((v-arcs r2n-v-arcs)
              (v-acc r2n-v-acc))
          (r2n-init)
          (list nl-start no-nl-start v-arcs v-acc))))))

; Module noeps.scm.
; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

; Fonction "merge" qui elimine les repetitions
(define noeps-merge-1
  (lambda (l1 l2)
    (cond ((null? l1)
           l2)
          ((null? l2)
           l1)
          (else
           (let ((t1 (car l1))
                 (t2 (car l2)))
             (cond ((< t1 t2)
                    (cons t1 (noeps-merge-1 (cdr l1) l2)))
                   ((= t1 t2)
                    (cons t1 (noeps-merge-1 (cdr l1) (cdr l2))))
                   (else
                    (cons t2 (noeps-merge-1 l1 (cdr l2))))))))))

; Fabrication des voisinages externes
(define noeps-mkvois
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
           (arcs (make-vector nbnodes '())))
      (let loop1 ((n 0))
        (if (< n nbnodes)
            (begin
              (let loop2 ((trans (vector-ref trans-v n)) (ends '()))
                (if (null? trans)
                    (vector-set! arcs n ends)
                    (let* ((tran (car trans))
                           (class (car tran))
                           (end (cdr tran)))
                      (loop2 (cdr trans) (if (eq? class 'eps)
                                             (noeps-merge-1 ends (list end))
                                             ends)))))
              (loop1 (+ n 1)))))
      arcs)))

; Fabrication des valeurs initiales
(define noeps-mkinit
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
           (init (make-vector nbnodes)))
      (let loop ((n 0))
        (if (< n nbnodes)
            (begin
              (vector-set! init n (list n))
              (loop (+ n 1)))))
      init)))

; Traduction d'une liste d'arcs
(define noeps-trad-arcs
  (lambda (trans dict)
    (let loop ((trans trans))
      (if (null? trans)
          '()
          (let* ((tran (car trans))
                 (class (car tran))
                 (end (cdr tran)))
            (if (eq? class 'eps)
                (loop (cdr trans))
                (let* ((new-end (vector-ref dict end))
                       (new-tran (cons class new-end)))
                  (cons new-tran (loop (cdr trans))))))))))

; Elimination des transitions eps
(define noeps
  (lambda (nl-start no-nl-start arcs acc)
    (let* ((digraph-arcs (noeps-mkvois arcs))
           (digraph-init (noeps-mkinit arcs))
           (dict (digraph digraph-arcs digraph-init noeps-merge-1))
           (new-nl-start (vector-ref dict nl-start))
           (new-no-nl-start (vector-ref dict no-nl-start)))
      (let loop ((i (- (vector-length arcs) 1)))
        (if (>= i 0)
            (begin
              (vector-set! arcs i (noeps-trad-arcs (vector-ref arcs i) dict))
              (loop (- i 1)))))
      (list new-nl-start new-no-nl-start arcs acc))))

; Module sweep.scm.
; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

; Preparer les arcs pour digraph
(define sweep-mkarcs
  (lambda (trans-v)
    (let* ((nbnodes (vector-length trans-v))
           (arcs-v (make-vector nbnodes '())))
      (let loop1 ((n 0))
        (if (< n nbnodes)
            (let loop2 ((trans (vector-ref trans-v n)) (arcs '()))
              (if (null? trans)
                  (begin
                    (vector-set! arcs-v n arcs)
                    (loop1 (+ n 1)))
                  (loop2 (cdr trans) (noeps-merge-1 (cdar trans) arcs))))
            arcs-v)))))

; Preparer l'operateur pour digraph
(define sweep-op
  (let ((acc-min (lambda (rule1 rule2)
                   (cond ((not rule1)
                          rule2)
                         ((not rule2)
                          rule1)
                         (else
                          (min rule1 rule2))))))
    (lambda (acc1 acc2)
      (cons (acc-min (car acc1) (car acc2))
            (acc-min (cdr acc1) (cdr acc2))))))

; Renumerotation des etats (#f pour etat a eliminer)
; Retourne (new-nbnodes . dict)
(define sweep-renum
  (lambda (dist-acc-v)
    (let* ((nbnodes (vector-length dist-acc-v))
           (dict (make-vector nbnodes)))
      (let loop ((n 0) (new-n 0))
        (if (< n nbnodes)
            (let* ((acc (vector-ref dist-acc-v n))
                   (dead? (equal? acc '(#f . #f))))
              (if dead?
                  (begin
                    (vector-set! dict n #f)
                    (loop (+ n 1) new-n))
                  (begin
                    (vector-set! dict n new-n)
                    (loop (+ n 1) (+ new-n 1)))))
            (cons new-n dict))))))

; Elimination des etats inutiles d'une liste d'etats
(define sweep-list
  (lambda (ss dict)
    (if (null? ss)
        '()
        (let* ((olds (car ss))
               (news (vector-ref dict olds)))
          (if news
              (cons news (sweep-list (cdr ss) dict))
              (sweep-list (cdr ss) dict))))))

; Elimination des etats inutiles d'une liste d'arcs
(define sweep-arcs
  (lambda (arcs dict)
    (if (null? arcs)
        '()
        (let* ((arc (car arcs))
               (class (car arc))
               (ss (cdr arc))
               (new-ss (sweep-list ss dict)))
          (if (null? new-ss)
              (sweep-arcs (cdr arcs) dict)
              (cons (cons class new-ss) (sweep-arcs (cdr arcs) dict)))))))

; Elimination des etats inutiles dans toutes les transitions
(define sweep-all-arcs
  (lambda (arcs-v dict)
    (let loop ((n (- (vector-length arcs-v) 1)))
      (if (>= n 0)
          (begin
            (vector-set! arcs-v n (sweep-arcs (vector-ref arcs-v n) dict))
            (loop (- n 1)))
          arcs-v))))

; Elimination des etats inutiles dans un vecteur
(define sweep-states
  (lambda (v new-nbnodes dict)
    (let ((nbnodes (vector-length v))
          (new-v (make-vector new-nbnodes)))
      (let loop ((n 0))
        (if (< n nbnodes)
            (let ((new-n (vector-ref dict n)))
              (if new-n
                  (vector-set! new-v new-n (vector-ref v n)))
              (loop (+ n 1)))
            new-v)))))

; Elimination des etats inutiles
(define sweep
  (lambda (nl-start no-nl-start arcs-v acc-v)
    (let* ((digraph-arcs (sweep-mkarcs arcs-v))
           (digraph-init acc-v)
           (digraph-op sweep-op)
           (dist-acc-v (digraph digraph-arcs digraph-init digraph-op))
           (result (sweep-renum dist-acc-v))
           (new-nbnodes (car result))
           (dict (cdr result))
           (new-nl-start (sweep-list nl-start dict))
           (new-no-nl-start (sweep-list no-nl-start dict))
           (new-arcs-v (sweep-states (sweep-all-arcs arcs-v dict)
                                     new-nbnodes
                                     dict))
           (new-acc-v (sweep-states acc-v new-nbnodes dict)))
      (list new-nl-start new-no-nl-start new-arcs-v new-acc-v))))

; Module nfa2dfa.scm.
; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

; Recoupement de deux arcs
(define n2d-2arcs
  (lambda (arc1 arc2)
    (let* ((class1 (car arc1))
           (ss1 (cdr arc1))
           (class2 (car arc2))
           (ss2 (cdr arc2))
           (result (class-sep class1 class2))
           (classl (vector-ref result 0))
           (classc (vector-ref result 1))
           (classr (vector-ref result 2))
           (ssl ss1)
           (ssc (ss-union ss1 ss2))
           (ssr ss2))
      (vector (if (or (null? classl) (null? ssl)) #f (cons classl ssl))
              (if (or (null? classc) (null? ssc)) #f (cons classc ssc))
              (if (or (null? classr) (null? ssr)) #f (cons classr ssr))))))

; Insertion d'un arc dans une liste d'arcs a classes distinctes
(define n2d-insert-arc
  (lambda (new-arc arcs)
    (if (null? arcs)
        (list new-arc)
        (let* ((arc (car arcs))
               (others (cdr arcs))
               (result (n2d-2arcs new-arc arc))
               (arcl (vector-ref result 0))
               (arcc (vector-ref result 1))
               (arcr (vector-ref result 2))
               (list-arcc (if arcc (list arcc) '()))
               (list-arcr (if arcr (list arcr) '())))
          (if arcl
              (append list-arcc list-arcr (n2d-insert-arc arcl others))
              (append list-arcc list-arcr others))))))

; Regroupement des arcs qui aboutissent au meme sous-ensemble d'etats
(define n2d-factorize-arcs
  (lambda (arcs)
    (if (null? arcs)
        '()
        (let* ((arc (car arcs))
               (arc-ss (cdr arc))
               (others-no-fact (cdr arcs))
               (others (n2d-factorize-arcs others-no-fact)))
          (let loop ((o others))
            (if (null? o)
                (list arc)
                (let* ((o1 (car o))
                       (o1-ss (cdr o1)))
                  (if (equal? o1-ss arc-ss)
                      (let* ((arc-class (car arc))
                             (o1-class (car o1))
                             (new-class (class-union arc-class o1-class))
                             (new-arc (cons new-class arc-ss)))
                        (cons new-arc (cdr o)))
                      (cons o1 (loop (cdr o)))))))))))

; Transformer une liste d'arcs quelconques en des arcs a classes distinctes
(define n2d-distinguish-arcs
  (lambda (arcs)
    (let loop ((arcs arcs) (n-arcs '()))
      (if (null? arcs)
          n-arcs
          (loop (cdr arcs) (n2d-insert-arc (car arcs) n-arcs))))))

; Transformer une liste d'arcs quelconques en des arcs a classes et a
; destinations distinctes
(define n2d-normalize-arcs
  (lambda (arcs)
    (n2d-factorize-arcs (n2d-distinguish-arcs arcs))))

; Factoriser des arcs a destination unique (~deterministes)
(define n2d-factorize-darcs
  (lambda (arcs)
    (if (null? arcs)
        '()
        (let* ((arc (car arcs))
               (arc-end (cdr arc))
               (other-arcs (cdr arcs))
               (farcs (n2d-factorize-darcs other-arcs)))
          (let loop ((farcs farcs))
            (if (null? farcs)
                (list arc)
                (let* ((farc (car farcs))
                       (farc-end (cdr farc)))
                  (if (= farc-end arc-end)
                      (let* ((arc-class (car arc))
                             (farc-class (car farc))
                             (new-class (class-union farc-class arc-class))
                             (new-arc (cons new-class arc-end)))
                        (cons new-arc (cdr farcs)))
                      (cons farc (loop (cdr farcs)))))))))))

; Normaliser un vecteur de listes d'arcs
(define n2d-normalize-arcs-v
  (lambda (arcs-v)
    (let* ((nbnodes (vector-length arcs-v))
           (new-v (make-vector nbnodes)))
      (let loop ((n 0))
        (if (= n nbnodes)
            new-v
            (begin
              (vector-set! new-v n (n2d-normalize-arcs (vector-ref arcs-v n)))
              (loop (+ n 1))))))))

; Inserer un arc dans une liste d'arcs a classes distinctes en separant
; les arcs contenant une partie de la classe du nouvel arc des autres arcs
; Retourne: (oui . non)
(define n2d-ins-sep-arc
  (lambda (new-arc arcs)
    (if (null? arcs)
        (cons (list new-arc) '())
        (let* ((arc (car arcs))
               (others (cdr arcs))
               (result (n2d-2arcs new-arc arc))
               (arcl (vector-ref result 0))
               (arcc (vector-ref result 1))
               (arcr (vector-ref result 2))
               (l-arcc (if arcc (list arcc) '()))
               (l-arcr (if arcr (list arcr) '()))
               (result (if arcl
                           (n2d-ins-sep-arc arcl others)
                           (cons '() others)))
               (oui-arcs (car result))
               (non-arcs (cdr result)))
          (cons (append l-arcc oui-arcs) (append l-arcr non-arcs))))))

; Combiner deux listes d'arcs a classes distinctes
; Ne tente pas de combiner les arcs qui ont nec. des classes disjointes
; Conjecture: les arcs crees ont leurs classes disjointes
; Note: envisager de rajouter un "n2d-factorize-arcs" !!!!!!!!!!!!
(define n2d-combine-arcs
  (lambda (arcs1 arcs2)
    (let loop ((arcs1 arcs1) (arcs2 arcs2) (dist-arcs2 '()))
      (if (null? arcs1)
          (append arcs2 dist-arcs2)
          (let* ((arc (car arcs1))
                 (result (n2d-ins-sep-arc arc arcs2))
                 (oui-arcs (car result))
                 (non-arcs (cdr result)))
            (loop (cdr arcs1) non-arcs (append oui-arcs dist-arcs2)))))))

; ; 
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Recherche lineaire.  Creation naive
; ; des arcs d'un ensemble d'etats.
; ; 
; 
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
; 
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
; 
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
; 
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
; 
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)))
; 
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
;          (v (make-vector new-len #f)))
;       (let loop ((n 0))
;       (if (= n n2d-state-count)
;           (begin
;             (set! n2d-state-dict v)
;             (set! n2d-state-len new-len))
;           (begin
;             (vector-set! v n (vector-ref n2d-state-dict n))
;             (loop (+ n 1))))))))
; 
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
;          (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
;         (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
; 
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((n 0))
;       (if (= n n2d-state-count)
;         (n2d-add-state ss)
;         (let* ((dentry (vector-ref n2d-state-dict n))
;                (dentry-ss (get-dentry-ss dentry)))
;           (if (equal? dentry-ss ss)
;               n
;               (loop (+ n 1))))))))
; 
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
;          (ss (cdr arc))
;          (s (n2d-search-state ss)))
;       (cons class s))))
; 
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
; 
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
;                  (cond ((not rule1)
;                         rule2)
;                        ((not rule2)
;                         rule1)
;                        (else
;                         (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
;           (acc-min (cdr acc1) (cdr acc2))))))
; 
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
;       (cons #f #f)
;       (n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
; 
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
;          (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
;       (if (= n n2d-state-count)
;           (cons arcs-v acc-v)
;           (begin
;             (vector-set! arcs-v n (get-dentry-darcs
;                                    (vector-ref n2d-state-dict n)))
;             (vector-set! acc-v n (get-dentry-acc
;                                   (vector-ref n2d-state-dict n)))
;             (loop (+ n 1))))))))
; 
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
;          (no-nl-d (n2d-search-state no-nl-start)))
;       (let loop ((n 0))
;       (if (< n n2d-state-count)
;           (let* ((dentry (vector-ref n2d-state-dict n))
;                  (ss (get-dentry-ss dentry))
;                  (arcss (map (lambda (s) (vector-ref arcs-v s)) ss))
;                  (arcs (apply append arcss))
;                  (dist-arcs (n2d-distinguish-arcs arcs))
;                  (darcs (n2d-translate-arcs dist-arcs))
;                  (fact-darcs (n2d-factorize-darcs darcs))
;                  (accs (map (lambda (s) (vector-ref acc-v s)) ss))
;                  (acc (n2d-acc-mins accs)))
;             (set-dentry-darcs dentry fact-darcs)
;             (set-dentry-acc   dentry acc)
;             (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
;            (new-arcs-v (car result))
;            (new-acc-v (cdr result)))
;       (n2d-init-glob-vars)
;       (list nl-d no-nl-d new-arcs-v new-acc-v)))))

; ; 
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Recherche lineaire.  Creation des
; ; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; ; classes distinctes.
; ; 
; 
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
; 
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
; 
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
; 
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
; 
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)))
; 
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
;          (v (make-vector new-len #f)))
;       (let loop ((n 0))
;       (if (= n n2d-state-count)
;           (begin
;             (set! n2d-state-dict v)
;             (set! n2d-state-len new-len))
;           (begin
;             (vector-set! v n (vector-ref n2d-state-dict n))
;             (loop (+ n 1))))))))
; 
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
;          (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
;         (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
; 
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((n 0))
;       (if (= n n2d-state-count)
;         (n2d-add-state ss)
;         (let* ((dentry (vector-ref n2d-state-dict n))
;                (dentry-ss (get-dentry-ss dentry)))
;           (if (equal? dentry-ss ss)
;               n
;               (loop (+ n 1))))))))
; 
; ; Combiner des listes d'arcs a classes dictinctes
; (define n2d-combine-arcs-l
;   (lambda (arcs-l)
;     (if (null? arcs-l)
;       '()
;       (let* ((arcs (car arcs-l))
;              (other-arcs-l (cdr arcs-l))
;              (other-arcs (n2d-combine-arcs-l other-arcs-l)))
;         (n2d-combine-arcs arcs other-arcs)))))
; 
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
;          (ss (cdr arc))
;          (s (n2d-search-state ss)))
;       (cons class s))))
; 
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
; 
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
;                  (cond ((not rule1)
;                         rule2)
;                        ((not rule2)
;                         rule1)
;                        (else
;                         (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
;           (acc-min (cdr acc1) (cdr acc2))))))
; 
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
;       (cons #f #f)
;       (n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
; 
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
;          (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
;       (if (= n n2d-state-count)
;           (cons arcs-v acc-v)
;           (begin
;             (vector-set! arcs-v n (get-dentry-darcs
;                                    (vector-ref n2d-state-dict n)))
;             (vector-set! acc-v n (get-dentry-acc
;                                   (vector-ref n2d-state-dict n)))
;             (loop (+ n 1))))))))
; 
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
;          (no-nl-d (n2d-search-state no-nl-start))
;          (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
;       (let loop ((n 0))
;       (if (< n n2d-state-count)
;           (let* ((dentry (vector-ref n2d-state-dict n))
;                  (ss (get-dentry-ss dentry))
;                  (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
;                  (arcs (n2d-combine-arcs-l arcs-l))
;                  (darcs (n2d-translate-arcs arcs))
;                  (fact-darcs (n2d-factorize-darcs darcs))
;                  (accs (map (lambda (s) (vector-ref acc-v s)) ss))
;                  (acc (n2d-acc-mins accs)))
;             (set-dentry-darcs dentry fact-darcs)
;             (set-dentry-acc   dentry acc)
;             (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
;            (new-arcs-v (car result))
;            (new-acc-v (cdr result)))
;       (n2d-init-glob-vars)
;       (list nl-d no-nl-d new-arcs-v new-acc-v)))))

; ; 
; ; Section temporaire: vieille facon de generer le dfa
; ; Dictionnaire d'etat det.  Arbre de recherche.  Creation des
; ; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; ; classes distinctes.
; ; 
; 
; ; Quelques variables globales
; (define n2d-state-dict '#(#f))
; (define n2d-state-len 1)
; (define n2d-state-count 0)
; (define n2d-state-tree '#(#f ()))
; 
; ; Fonctions de gestion des entrees du dictionnaire
; (define make-dentry (lambda (ss) (vector ss #f #f)))
; 
; (define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
; (define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
; (define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))
; 
; (define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
; (define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))
; 
; ; Fonctions de gestion de l'arbre de recherche
; (define make-snode (lambda () (vector #f '())))
; 
; (define get-snode-dstate   (lambda (snode) (vector-ref snode 0)))
; (define get-snode-children (lambda (snode) (vector-ref snode 1)))
; 
; (define set-snode-dstate
;   (lambda (snode dstate)   (vector-set! snode 0 dstate)))
; (define set-snode-children
;   (lambda (snode children) (vector-set! snode 1 children)))
; 
; ; Initialisation des variables globales
; (define n2d-init-glob-vars
;   (lambda ()
;     (set! n2d-state-dict (vector #f))
;     (set! n2d-state-len 1)
;     (set! n2d-state-count 0)
;     (set! n2d-state-tree (make-snode))))
; 
; ; Extension du dictionnaire
; (define n2d-extend-dict
;   (lambda ()
;     (let* ((new-len (* 2 n2d-state-len))
;          (v (make-vector new-len #f)))
;       (let loop ((n 0))
;       (if (= n n2d-state-count)
;           (begin
;             (set! n2d-state-dict v)
;             (set! n2d-state-len new-len))
;           (begin
;             (vector-set! v n (vector-ref n2d-state-dict n))
;             (loop (+ n 1))))))))
; 
; ; Ajout d'un etat
; (define n2d-add-state
;   (lambda (ss)
;     (let* ((s n2d-state-count)
;          (dentry (make-dentry ss)))
;       (if (= n2d-state-count n2d-state-len)
;         (n2d-extend-dict))
;       (vector-set! n2d-state-dict s dentry)
;       (set! n2d-state-count (+ n2d-state-count 1))
;       s)))
; 
; ; Recherche d'un etat
; (define n2d-search-state
;   (lambda (ss)
;     (let loop ((s-l ss) (snode n2d-state-tree))
;       (if (null? s-l)
;         (or (get-snode-dstate snode)
;             (let ((s (n2d-add-state ss)))
;               (set-snode-dstate snode s)
;               s))
;         (let* ((next-s (car s-l))
;                (alist (get-snode-children snode))
;                (ass (or (assv next-s alist)
;                         (let ((ass (cons next-s (make-snode))))
;                           (set-snode-children snode (cons ass alist))
;                           ass))))
;           (loop (cdr s-l) (cdr ass)))))))
; 
; ; Combiner des listes d'arcs a classes dictinctes
; (define n2d-combine-arcs-l
;   (lambda (arcs-l)
;     (if (null? arcs-l)
;       '()
;       (let* ((arcs (car arcs-l))
;              (other-arcs-l (cdr arcs-l))
;              (other-arcs (n2d-combine-arcs-l other-arcs-l)))
;         (n2d-combine-arcs arcs other-arcs)))))
; 
; ; Transformer un arc non-det. en un arc det.
; (define n2d-translate-arc
;   (lambda (arc)
;     (let* ((class (car arc))
;          (ss (cdr arc))
;          (s (n2d-search-state ss)))
;       (cons class s))))
; 
; ; Transformer une liste d'arcs non-det. en ...
; (define n2d-translate-arcs
;   (lambda (arcs)
;     (map n2d-translate-arc arcs)))
; 
; ; Trouver le minimum de deux acceptants
; (define n2d-acc-min2
;   (let ((acc-min (lambda (rule1 rule2)
;                  (cond ((not rule1)
;                         rule2)
;                        ((not rule2)
;                         rule1)
;                        (else
;                         (min rule1 rule2))))))
;     (lambda (acc1 acc2)
;       (cons (acc-min (car acc1) (car acc2))
;           (acc-min (cdr acc1) (cdr acc2))))))
; 
; ; Trouver le minimum de plusieurs acceptants
; (define n2d-acc-mins
;   (lambda (accs)
;     (if (null? accs)
;       (cons #f #f)
;       (n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))
; 
; ; Fabriquer les vecteurs d'arcs et d'acceptance
; (define n2d-extract-vs
;   (lambda ()
;     (let* ((arcs-v (make-vector n2d-state-count))
;          (acc-v (make-vector n2d-state-count)))
;       (let loop ((n 0))
;       (if (= n n2d-state-count)
;           (cons arcs-v acc-v)
;           (begin
;             (vector-set! arcs-v n (get-dentry-darcs
;                                    (vector-ref n2d-state-dict n)))
;             (vector-set! acc-v n (get-dentry-acc
;                                   (vector-ref n2d-state-dict n)))
;             (loop (+ n 1))))))))
; 
; ; Effectuer la transformation de l'automate de non-det. a det.
; (define nfa2dfa
;   (lambda (nl-start no-nl-start arcs-v acc-v)
;     (n2d-init-glob-vars)
;     (let* ((nl-d (n2d-search-state nl-start))
;          (no-nl-d (n2d-search-state no-nl-start))
;          (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
;       (let loop ((n 0))
;       (if (< n n2d-state-count)
;           (let* ((dentry (vector-ref n2d-state-dict n))
;                  (ss (get-dentry-ss dentry))
;                  (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
;                  (arcs (n2d-combine-arcs-l arcs-l))
;                  (darcs (n2d-translate-arcs arcs))
;                  (fact-darcs (n2d-factorize-darcs darcs))
;                  (accs (map (lambda (s) (vector-ref acc-v s)) ss))
;                  (acc (n2d-acc-mins accs)))
;             (set-dentry-darcs dentry fact-darcs)
;             (set-dentry-acc   dentry acc)
;             (loop (+ n 1)))))
;       (let* ((result (n2d-extract-vs))
;            (new-arcs-v (car result))
;            (new-acc-v (cdr result)))
;       (n2d-init-glob-vars)
;       (list nl-d no-nl-d new-arcs-v new-acc-v)))))

; 
; Section temporaire: vieille facon de generer le dfa
; Dictionnaire d'etat det.  Table de hashage.  Creation des
; arcs d'un ensemble d'etats en combinant des ensembles d'arcs a
; classes distinctes.
; 

; Quelques variables globales
(define n2d-state-dict '#(#f))
(define n2d-state-len 1)
(define n2d-state-count 0)
(define n2d-state-hash '#())

; Fonctions de gestion des entrees du dictionnaire
(define make-dentry (lambda (ss) (vector ss #f #f)))

(define get-dentry-ss    (lambda (dentry) (vector-ref dentry 0)))
(define get-dentry-darcs (lambda (dentry) (vector-ref dentry 1)))
(define get-dentry-acc   (lambda (dentry) (vector-ref dentry 2)))

(define set-dentry-darcs (lambda (dentry arcs) (vector-set! dentry 1 arcs)))
(define set-dentry-acc   (lambda (dentry acc)  (vector-set! dentry 2 acc)))

; Initialisation des variables globales
(define n2d-init-glob-vars
  (lambda (hash-len)
    (set! n2d-state-dict (vector #f))
    (set! n2d-state-len 1)
    (set! n2d-state-count 0)
    (set! n2d-state-hash (make-vector hash-len '()))))

; Extension du dictionnaire
(define n2d-extend-dict
  (lambda ()
    (let* ((new-len (* 2 n2d-state-len))
           (v (make-vector new-len #f)))
      (let loop ((n 0))
        (if (= n n2d-state-count)
            (begin
              (set! n2d-state-dict v)
              (set! n2d-state-len new-len))
            (begin
              (vector-set! v n (vector-ref n2d-state-dict n))
              (loop (+ n 1))))))))

; Ajout d'un etat
(define n2d-add-state
  (lambda (ss)
    (let* ((s n2d-state-count)
           (dentry (make-dentry ss)))
      (if (= n2d-state-count n2d-state-len)
          (n2d-extend-dict))
      (vector-set! n2d-state-dict s dentry)
      (set! n2d-state-count (+ n2d-state-count 1))
      s)))

; Recherche d'un etat
(define n2d-search-state
  (lambda (ss)
    (let* ((hash-no (if (null? ss) 0 (car ss)))
           (alist (vector-ref n2d-state-hash hash-no))
           (ass (assoc ss alist)))
      (if ass
          (cdr ass)
          (let* ((s (n2d-add-state ss))
                 (new-ass (cons ss s)))
            (vector-set! n2d-state-hash hash-no (cons new-ass alist))
            s)))))

; Combiner des listes d'arcs a classes dictinctes
(define n2d-combine-arcs-l
  (lambda (arcs-l)
    (if (null? arcs-l)
        '()
        (let* ((arcs (car arcs-l))
               (other-arcs-l (cdr arcs-l))
               (other-arcs (n2d-combine-arcs-l other-arcs-l)))
          (n2d-combine-arcs arcs other-arcs)))))

; Transformer un arc non-det. en un arc det.
(define n2d-translate-arc
  (lambda (arc)
    (let* ((class (car arc))
           (ss (cdr arc))
           (s (n2d-search-state ss)))
      (cons class s))))

; Transformer une liste d'arcs non-det. en ...
(define n2d-translate-arcs
  (lambda (arcs)
    (map n2d-translate-arc arcs)))

; Trouver le minimum de deux acceptants
(define n2d-acc-min2
  (let ((acc-min (lambda (rule1 rule2)
                   (cond ((not rule1)
                          rule2)
                         ((not rule2)
                          rule1)
                         (else
                          (min rule1 rule2))))))
    (lambda (acc1 acc2)
      (cons (acc-min (car acc1) (car acc2))
            (acc-min (cdr acc1) (cdr acc2))))))

; Trouver le minimum de plusieurs acceptants
(define n2d-acc-mins
  (lambda (accs)
    (if (null? accs)
        (cons #f #f)
        (n2d-acc-min2 (car accs) (n2d-acc-mins (cdr accs))))))

; Fabriquer les vecteurs d'arcs et d'acceptance
(define n2d-extract-vs
  (lambda ()
    (let* ((arcs-v (make-vector n2d-state-count))
           (acc-v (make-vector n2d-state-count)))
      (let loop ((n 0))
        (if (= n n2d-state-count)
            (cons arcs-v acc-v)
            (begin
              (vector-set! arcs-v n (get-dentry-darcs
                                     (vector-ref n2d-state-dict n)))
              (vector-set! acc-v n (get-dentry-acc
                                    (vector-ref n2d-state-dict n)))
              (loop (+ n 1))))))))

; Effectuer la transformation de l'automate de non-det. a det.
(define nfa2dfa
  (lambda (nl-start no-nl-start arcs-v acc-v)
    (n2d-init-glob-vars (vector-length arcs-v))
    (let* ((nl-d (n2d-search-state nl-start))
           (no-nl-d (n2d-search-state no-nl-start))
           (norm-arcs-v (n2d-normalize-arcs-v arcs-v)))
      (let loop ((n 0))
        (if (< n n2d-state-count)
            (let* ((dentry (vector-ref n2d-state-dict n))
                   (ss (get-dentry-ss dentry))
                   (arcs-l (map (lambda (s) (vector-ref norm-arcs-v s)) ss))
                   (arcs (n2d-combine-arcs-l arcs-l))
                   (darcs (n2d-translate-arcs arcs))
                   (fact-darcs (n2d-factorize-darcs darcs))
                   (accs (map (lambda (s) (vector-ref acc-v s)) ss))
                   (acc (n2d-acc-mins accs)))
              (set-dentry-darcs dentry fact-darcs)
              (set-dentry-acc   dentry acc)
              (loop (+ n 1)))))
      (let* ((result (n2d-extract-vs))
             (new-arcs-v (car result))
             (new-acc-v (cdr result)))
        (n2d-init-glob-vars 0)
        (list nl-d no-nl-d new-arcs-v new-acc-v)))))

; Module prep.scm.
; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

;
; Divers pre-traitements avant l'ecriture des tables
;

; Passe d'un arc multi-range a une liste d'arcs mono-range
(define prep-arc->sharcs
  (lambda (arc)
    (let* ((range-l (car arc))
           (dest (cdr arc))
           (op (lambda (range) (cons range dest))))
      (map op range-l))))

; Compare des arcs courts selon leur premier caractere
(define prep-sharc-<=
  (lambda (sharc1 sharc2)
    (class-<= (caar sharc1) (caar sharc2))))

; Remplit les trous parmi les sharcs avec des arcs "erreur"
(define prep-fill-error
  (lambda (sharcs)
    (let loop ((sharcs sharcs) (start 'inf-))
      (cond ((class-= start 'inf+)
             '())
            ((null? sharcs)
             (cons (cons (cons start 'inf+) 'err) (loop sharcs 'inf+)))
            (else
             (let* ((sharc (car sharcs))
                    (h (caar sharc))
                    (t (cdar sharc)))
               (if (class-< start h)
                   (cons (cons (cons start (- h 1)) 'err) (loop sharcs h))
                   (cons sharc (loop (cdr sharcs)
                                     (if (class-= t 'inf+)
                                         'inf+
                                         (+ t 1)))))))))))

; ; Passe d'une liste d'arcs a un arbre de decision
; ; 1ere methode: seulement des comparaisons <
; (define prep-arcs->tree
;   (lambda (arcs)
;     (let* ((sharcs-l (map prep-arc->sharcs arcs))
;          (sharcs (apply append sharcs-l))
;          (sorted-with-holes (merge-sort sharcs prep-sharc-<=))
;          (sorted (prep-fill-error sorted-with-holes))
;          (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
;          (table (list->vector (map op sorted))))
;       (let loop ((left 0) (right (- (vector-length table) 1)))
;       (if (= left right)
;           (cdr (vector-ref table left))
;           (let ((mid (quotient (+ left right 1) 2)))
;             (list (car (vector-ref table mid))
;                   (loop left (- mid 1))
;                   (loop mid right))))))))

; Passe d'une liste d'arcs a un arbre de decision
; 2eme methode: permettre des comparaisons = quand ca adonne
(define prep-arcs->tree
  (lambda (arcs)
    (let* ((sharcs-l (map prep-arc->sharcs arcs))
           (sharcs (apply append sharcs-l))
           (sorted-with-holes (merge-sort sharcs prep-sharc-<=))
           (sorted (prep-fill-error sorted-with-holes))
           (op (lambda (sharc) (cons (caar sharc) (cdr sharc))))
           (table (list->vector (map op sorted))))
      (let loop ((left 0) (right (- (vector-length table) 1)))
        (if (= left right)
            (cdr (vector-ref table left))
            (let ((mid (quotient (+ left right 1) 2)))
              (if (and (= (+ left 2) right)
                       (= (+ (car (vector-ref table mid)) 1)
                          (car (vector-ref table right)))
                       (eqv? (cdr (vector-ref table left))
                             (cdr (vector-ref table right))))
                  (list '=
                        (car (vector-ref table mid))
                        (cdr (vector-ref table mid))
                        (cdr (vector-ref table left)))
                  (list (car (vector-ref table mid))
                        (loop left (- mid 1))
                        (loop mid right)))))))))

; Determine si une action a besoin de calculer yytext
(define prep-detect-yytext
  (lambda (s)
    (let loop1 ((i (- (string-length s) 6)))
      (cond ((< i 0)
             #f)
            ((char-ci=? (string-ref s i) #\y)
             (let loop2 ((j 5))
               (cond ((= j 0)
                      #t)
                     ((char-ci=? (string-ref s (+ i j))
                                 (string-ref "yytext" j))
                      (loop2 (- j 1)))
                     (else
                      (loop1 (- i 1))))))
            (else
             (loop1 (- i 1)))))))

; Note dans une regle si son action a besoin de yytext
(define prep-set-rule-yytext?
  (lambda (rule)
    (let ((action (get-rule-action rule)))
      (set-rule-yytext? rule (prep-detect-yytext action)))))

; Note dans toutes les regles si leurs actions ont besoin de yytext
(define prep-set-rules-yytext?
  (lambda (rules)
    (let loop ((n (- (vector-length rules) 1)))
      (if (>= n 0)
          (begin
            (prep-set-rule-yytext? (vector-ref rules n))
            (loop (- n 1)))))))
