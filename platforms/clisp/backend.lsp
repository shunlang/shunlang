"Copyright (c) 2010-2015, Mark Tarver

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of Mark Tarver may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Mark Tarver ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Mark Tarver BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."

(DEFUN shun.mk-kl (V646) (LET ((Shun (read-file V646))) (LET ((KL (MAPCAR (QUOTE shun.produce-kl) Shun))) (LET ((KLString (shun.code-string KL))) (LET ((WriteKL (write-to-file (cn V646 ".kl") KLString))) (LET ((CL (MAPCAR (FUNCTION (LAMBDA (X) (shun.kl-to-lisp () X))) KL))) (LET ((CLString (shun.code-string CL))) (LET ((WriteCL (write-to-file (cn V646 ".lsp") CLString))) (QUOTE shun.ok))))))))) 

(DEFUN shun.produce-kl (V647) (COND ((AND (CONSP V647) (AND (EQ (QUOTE define) (CAR V647)) (CONSP (CDR V647)))) (shun.shun->kl (CAR (CDR V647)) (CDR (CDR V647)))) (T V647))) 

(DEFUN shun.code-string (V648) (COND ((NULL V648) "") ((CONSP V648) (cn (shun.app (CAR V648) " 

" (QUOTE shun.r)) (shun.code-string (CDR V648)))) (T (shun.f_error (QUOTE shun.code-string))))) 

(DEFUN shun.kl-to-lisp (V660 V661) (COND ((CONSP (MEMBER V661 V660)) V661) ((AND (CONSP V661) (AND (EQ (QUOTE type) (CAR V661)) (AND (CONSP (CDR V661)) (AND (CONSP (CDR (CDR V661))) (NULL (CDR (CDR (CDR V661)))))))) (shun.kl-to-lisp V660 (CAR (CDR V661)))) ((AND (CONSP V661) (AND (EQ (QUOTE lambda) (CAR V661)) (AND (CONSP (CDR V661)) (AND (CONSP (CDR (CDR V661))) (NULL (CDR (CDR (CDR V661)))))))) (LET ((ChX (shun.ch-T (CAR (CDR V661))))) (CONS (QUOTE FUNCTION) (CONS (CONS (QUOTE LAMBDA) (CONS (CONS ChX ()) (CONS (shun.kl-to-lisp (CONS ChX V660) (SUBST ChX (CAR (CDR V661)) (CAR (CDR (CDR V661))))) ()))) ())))) ((AND (CONSP V661) (AND (EQ (QUOTE let) (CAR V661)) (AND (CONSP (CDR V661)) (AND (CONSP (CDR (CDR V661))) (AND (CONSP (CDR (CDR (CDR V661)))) (NULL (CDR (CDR (CDR (CDR V661)))))))))) (LET ((ChX (shun.ch-T (CAR (CDR V661))))) (CONS (QUOTE LET) (CONS (CONS (CONS ChX (CONS (shun.kl-to-lisp V660 (CAR (CDR (CDR V661)))) ())) ()) (CONS (shun.kl-to-lisp (CONS ChX V660) (SUBST ChX (CAR (CDR V661)) (CAR (CDR (CDR (CDR V661)))))) ()))))) ((AND (CONSP V661) (AND (EQ (QUOTE defun) (CAR V661)) (AND (CONSP (CDR V661)) (AND (CONSP (CDR (CDR V661))) (AND (CONSP (CDR (CDR (CDR V661)))) (NULL (CDR (CDR (CDR (CDR V661)))))))))) (CONS (QUOTE DEFUN) (CONS (CAR (CDR V661)) (CONS (CAR (CDR (CDR V661))) (CONS (shun.kl-to-lisp (CAR (CDR (CDR V661))) (CAR (CDR (CDR (CDR V661))))) ()))))) ((AND (CONSP V661) (EQ (QUOTE cond) (CAR V661))) (CONS (QUOTE COND) (MAPCAR (FUNCTION (LAMBDA (C) (shun.cond_code V660 C))) (CDR V661)))) ((CONSP V661) (LET ((Arguments (MAPCAR (FUNCTION (LAMBDA (Y) (shun.kl-to-lisp V660 Y))) (CDR V661)))) (shun.optimise-application (IF (CONSP (MEMBER (CAR V661) V660)) (CONS (QUOTE shun.apply) (CONS (CAR V661) (CONS (CONS (QUOTE LIST) Arguments) ()))) (IF (CONSP (CAR V661)) (CONS (QUOTE shun.apply) (CONS (shun.kl-to-lisp V660 (CAR V661)) (CONS (CONS (QUOTE LIST) Arguments) ()))) (IF (shun.wrapper (shun.partial-application? (CAR V661) Arguments)) (shun.partially-apply (CAR V661) Arguments) (CONS (shun.maplispsym (CAR V661)) Arguments))))))) ((NULL V661) ()) ((EQ (SYMBOLP V661) (QUOTE T)) (CONS (QUOTE QUOTE) (CONS V661 ()))) (T V661))) 

(DEFUN shun.ch-T (V662) (COND ((EQ T V662) (QUOTE T1957)) (T V662))) 

(DEFUN shun.apply (V175303 V175304)
  (LET ((FSym (shun.maplispsym V175303)))
    (trap-error (shun.apply-help FSym V175304)
                #'(LAMBDA (E)
                    (shun.analyse-application V175303 
                                FSym V175304
                               (error-to-string E))))))

(DEFUN shun.apply-help (V175305 V175306)
  (COND ((NULL V175306) (FUNCALL V175305))
        ((AND (CONSP V175306) (NULL (CDR V175306)))
         (FUNCALL V175305 (CAR V175306)))
        ((CONSP V175306)
         (shun.apply-help (FUNCALL V175305 (CAR V175306)) 
                          (CDR V175306)))
        (T (shun.f_error 'shun.apply-help))))

(DEFUN shun.analyse-application 
     (V175307 V175308 V175309 V175310)
  (LET ((Lambda
         (IF (shun.wrapper 
               (shun.partial-application? V175307 V175309))
             (shun.build-up-lambda-expression V175308 V175307)
             (IF (shun.wrapper (shun.lazyboolop? V175307))
                 (shun.build-up-lambda-expression 
                                         V175308 V175307)
                 (simple-error V175310)))))
    (shun.curried-apply Lambda V175309)))

(DEFUN shun.build-up-lambda-expression (V175311 V175312)
  (EVAL (shun.mk-lambda V175311 (arity V175312))))

(DEFUN shun.lazyboolop? (F)
  (COND ((EQ F 'and) 'true)
        ((EQ F 'or) 'false)
        (T 'false))) 

(DEFUN shun.curried-apply (V529 V530)
 (COND ((AND (CONSP V530) (NULL (CDR V530))) (FUNCALL V529 (CAR V530)))
  ((CONSP V530) (shun.curried-apply (FUNCALL V529 (CAR V530)) (CDR V530)))
  (T  (simple-error
  (cn "cannot apply "
      (shun.app V529 "
"
                'shun.a))))))
  
(DEFUN shun.partial-application? (V669 V670) (LET ((Arity (trap-error (arity V669) (FUNCTION (LAMBDA (E) -1))))) (IF (shun.ABSEQUAL Arity -1) (QUOTE false) (IF (shun.ABSEQUAL Arity (length V670)) (QUOTE false) (IF (shun.wrapper (shun.greater? (length V670) Arity)) (QUOTE false) (QUOTE true)))))) 

(DEFUN shun.partially-apply (V671 V672) (LET ((Arity (arity V671))) (LET ((Lambda (shun.mk-lambda (CONS (shun.maplispsym V671) ()) Arity))) (shun.build-partial-application Lambda V672)))) 

(DEFUN shun.optimise-application (V673) (COND ((AND (CONSP V673) (AND (EQ (QUOTE hd) (CAR V673)) (AND (CONSP (CDR V673)) (NULL (CDR (CDR V673)))))) (CONS (QUOTE CAR) (CONS (shun.optimise-application (CAR (CDR V673))) ()))) ((AND (CONSP V673) (AND (EQ (QUOTE tl) (CAR V673)) (AND (CONSP (CDR V673)) (NULL (CDR (CDR V673)))))) (CONS (QUOTE CDR) (CONS (shun.optimise-application (CAR (CDR V673))) ()))) ((AND (CONSP V673) (AND (EQ (QUOTE cons) (CAR V673)) (AND (CONSP (CDR V673)) (AND (CONSP (CDR (CDR V673))) (NULL (CDR (CDR (CDR V673)))))))) (CONS (QUOTE CONS) (CONS (shun.optimise-application (CAR (CDR V673))) (CONS (shun.optimise-application (CAR (CDR (CDR V673)))) ())))) ((AND (CONSP V673) (AND (EQ (QUOTE append) (CAR V673)) (AND (CONSP (CDR V673)) (AND (CONSP (CDR (CDR V673))) (NULL (CDR (CDR (CDR V673)))))))) (CONS (QUOTE APPEND) (CONS (shun.optimise-application (CAR (CDR V673))) (CONS (shun.optimise-application (CAR (CDR (CDR V673)))) ())))) ((AND (CONSP V673) (AND (EQ (QUOTE reverse) (CAR V673)) (AND (CONSP (CDR V673)) (NULL (CDR (CDR V673)))))) (CONS (QUOTE REVERSE) (CONS (shun.optimise-application (CAR (CDR V673))) ()))) ((AND (CONSP V673) (AND (EQ (QUOTE if) (CAR V673)) (AND (CONSP (CDR V673)) (AND (CONSP (CDR (CDR V673))) (AND (CONSP (CDR (CDR (CDR V673)))) (NULL (CDR (CDR (CDR (CDR V673)))))))))) (CONS (QUOTE IF) (CONS (shun.wrap (CAR (CDR V673))) (CONS (shun.optimise-application (CAR (CDR (CDR V673)))) (CONS (shun.optimise-application (CAR (CDR (CDR (CDR V673))))) ()))))) ((AND (CONSP V673) (AND (EQ (QUOTE value) (CAR V673)) (AND (CONSP (CDR V673)) (AND (CONSP (CAR (CDR V673))) (AND (CONSP (CDR (CAR (CDR V673)))) (AND (NULL (CDR (CDR (CAR (CDR V673))))) (AND (NULL (CDR (CDR V673))) (EQ (CAR (CAR (CDR V673))) (QUOTE QUOTE))))))))) (CAR (CDR (CAR (CDR V673))))) ((AND (CONSP V673) (AND (EQ (QUOTE +) (CAR V673)) (AND (CONSP (CDR V673)) (AND (shun.ABSEQUAL 1 (CAR (CDR V673))) (AND (CONSP (CDR (CDR V673))) (NULL (CDR (CDR (CDR V673))))))))) (CONS (intern "1+") (CONS (shun.optimise-application (CAR (CDR (CDR V673)))) ()))) ((AND (CONSP V673) (AND (EQ (QUOTE +) (CAR V673)) (AND (CONSP (CDR V673)) (AND (CONSP (CDR (CDR V673))) (AND (shun.ABSEQUAL 1 (CAR (CDR (CDR V673)))) (NULL (CDR (CDR (CDR V673))))))))) (CONS (intern "1+") (CONS (shun.optimise-application (CAR (CDR V673))) ()))) ((AND (CONSP V673) (AND (EQ (QUOTE -) (CAR V673)) (AND (CONSP (CDR V673)) (AND (CONSP (CDR (CDR V673))) (AND (shun.ABSEQUAL 1 (CAR (CDR (CDR V673)))) (NULL (CDR (CDR (CDR V673))))))))) (CONS (intern "1-") (CONS (shun.optimise-application (CAR (CDR V673))) ()))) ((CONSP V673) (MAPCAR (QUOTE shun.optimise-application) V673)) (T V673))) 

(DEFUN shun.mk-lambda (V674 V675) (COND ((shun.ABSEQUAL 0 V675) V674) (T (LET ((X (gensym (QUOTE V)))) (CONS (QUOTE lambda) (CONS X (CONS (shun.mk-lambda (shun.endcons V674 X) (1- V675)) ()))))))) 

(DEFUN shun.endcons (V676 V677) (COND ((CONSP V676) (APPEND V676 (CONS V677 ()))) (T (CONS V676 (CONS V677 ()))))) 

(DEFUN shun.build-partial-application (V678 V679) (COND ((NULL V679) V678) ((CONSP V679) (shun.build-partial-application (CONS (QUOTE FUNCALL) (CONS V678 (CONS (CAR V679) ()))) (CDR V679))) (T (shun.f_error (QUOTE shun.build-partial-application))))) 

(DEFUN shun.cond_code (V680 V681) (COND ((AND (CONSP V681) (AND (CONSP (CDR V681)) (NULL (CDR (CDR V681))))) (CONS (shun.lisp_test V680 (CAR V681)) (CONS (shun.kl-to-lisp V680 (CAR (CDR V681))) ()))) (T (shun.f_error (QUOTE shun.cond_code))))) 

(DEFUN shun.lisp_test (V684 V685) (COND ((EQ (QUOTE true) V685) (QUOTE T)) ((AND (CONSP V685) (EQ (QUOTE and) (CAR V685))) (CONS (QUOTE AND) (MAPCAR (FUNCTION (LAMBDA (X) (shun.wrap (shun.kl-to-lisp V684 X)))) (CDR V685)))) (T (shun.wrap (shun.kl-to-lisp V684 V685))))) 

(DEFUN shun.wrap (V1173)
  (COND
   ((AND (CONSP V1173)
         (AND (EQ 'cons? (CAR V1173))
              (AND (CONSP (CDR V1173)) (NULL (CDR (CDR V1173))))))
    (CONS 'CONSP (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'string? (CAR V1173))
              (AND (CONSP (CDR V1173)) (NULL (CDR (CDR V1173))))))
    (CONS 'STRINGP (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'number? (CAR V1173))
              (AND (CONSP (CDR V1173)) (NULL (CDR (CDR V1173))))))
    (CONS 'NUMBERP (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'empty? (CAR V1173))
              (AND (CONSP (CDR V1173)) (NULL (CDR (CDR V1173))))))
    (CONS 'NULL (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'and (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (NULL (CDR (CDR (CDR V1173))))))))
    (CONS 'AND
          (CONS (shun.wrap (CAR (CDR V1173)))
                (CONS (shun.wrap (CAR (CDR (CDR V1173)))) NIL))))
   ((AND (CONSP V1173)
         (AND (EQ 'or (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (NULL (CDR (CDR (CDR V1173))))))))
    (CONS 'OR
          (CONS (shun.wrap (CAR (CDR V1173)))
                (CONS (shun.wrap (CAR (CDR (CDR V1173)))) NIL))))
   ((AND (CONSP V1173)
         (AND (EQ 'not (CAR V1173))
              (AND (CONSP (CDR V1173)) (NULL (CDR (CDR V1173))))))
    (CONS 'NOT (CONS (shun.wrap (CAR (CDR V1173))) NIL)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (AND (NULL (CAR (CDR (CDR V1173))))
                             (NULL (CDR (CDR (CDR V1173)))))))))
    (CONS 'NULL (CONS (CAR (CDR V1173)) NIL)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (NULL (CAR (CDR V1173)))
                        (AND (CONSP (CDR (CDR V1173)))
                             (NULL (CDR (CDR (CDR V1173)))))))))
    (CONS 'NULL (CDR (CDR V1173))))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (AND (CONSP (CAR (CDR (CDR V1173))))
                             (AND (CONSP (CDR (CAR (CDR (CDR V1173)))))
                                  (AND
                                   (NULL (CDR (CDR (CAR (CDR (CDR V1173))))))
                                   (AND (NULL (CDR (CDR (CDR V1173))))
                                        (AND
                                         (EQ
                                          (SYMBOLP
                                           (CAR (CDR (CAR (CDR (CDR V1173))))))
                                          T)
                                         (EQ (CAR (CAR (CDR (CDR V1173))))
                                             'QUOTE))))))))))
    (CONS 'EQ (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CAR (CDR V1173)))
                        (AND (CONSP (CDR (CAR (CDR V1173))))
                             (AND (NULL (CDR (CDR (CAR (CDR V1173)))))
                                  (AND (CONSP (CDR (CDR V1173)))
                                       (AND (NULL (CDR (CDR (CDR V1173))))
                                            (AND
                                             (EQ
                                              (SYMBOLP
                                               (CAR (CDR (CAR (CDR V1173)))))
                                              T)
                                             (EQ (CAR (CAR (CDR V1173)))
                                                 'QUOTE))))))))))
    (CONS 'EQ (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CAR (CDR V1173)))
                        (AND (EQ 'fail (CAR (CAR (CDR V1173))))
                             (AND (NULL (CDR (CAR (CDR V1173))))
                                  (AND (CONSP (CDR (CDR V1173)))
                                       (NULL (CDR (CDR (CDR V1173)))))))))))
    (CONS 'EQ (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (AND (CONSP (CAR (CDR (CDR V1173))))
                             (AND (EQ 'fail (CAR (CAR (CDR (CDR V1173)))))
                                  (AND (NULL (CDR (CAR (CDR (CDR V1173)))))
                                       (NULL (CDR (CDR (CDR V1173)))))))))))
    (CONS 'EQ (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (AND (NULL (CDR (CDR (CDR V1173))))
                             (STRINGP (CAR (CDR V1173))))))))
    (CONS 'EQUAL (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (AND (NULL (CDR (CDR (CDR V1173))))
                             (STRINGP (CAR (CDR (CDR V1173)))))))))
    (CONS 'EQUAL (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (AND (NULL (CDR (CDR (CDR V1173))))
                             (NUMBERP (CAR (CDR (CDR V1173)))))))))
    (CONS 'EQL (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (AND (NULL (CDR (CDR (CDR V1173))))
                             (NUMBERP (CAR (CDR V1173))))))))
    (CONS 'EQL (CONS (CAR (CDR (CDR V1173))) (CONS (CAR (CDR V1173)) NIL))))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.equal? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (NULL (CDR (CDR (CDR V1173))))))))
    (CONS 'shun.ABSEQUAL (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.greater? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (NULL (CDR (CDR (CDR V1173))))))))
    (CONS '> (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.greater-than-or-equal-to? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (NULL (CDR (CDR (CDR V1173))))))))
    (CONS '>= (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.less? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (NULL (CDR (CDR (CDR V1173))))))))
    (CONS '< (CDR V1173)))
   ((AND (CONSP V1173)
         (AND (EQ 'shun.less-than-or-equal-to? (CAR V1173))
              (AND (CONSP (CDR V1173))
                   (AND (CONSP (CDR (CDR V1173)))
                        (NULL (CDR (CDR (CDR V1173))))))))
    (CONS '<= (CDR V1173)))
   (T (CONS 'shun.wrapper (CONS V1173 NIL)))))

(DEFUN shun.wrapper (V687) (COND ((EQ (QUOTE true) V687) (QUOTE T)) ((EQ (QUOTE false) V687) ()) (T (simple-error (cn "boolean expected: not " (shun.app V687 "
" (QUOTE shun.s))))))) 

(DEFUN shun.maplispsym (V688) (COND ((EQ (QUOTE =) V688) (QUOTE shun.equal?)) ((EQ (QUOTE >) V688) (QUOTE shun.greater?)) ((EQ (QUOTE <) V688) (QUOTE shun.less?)) ((EQ (QUOTE >=) V688) (QUOTE shun.greater-than-or-equal-to?)) ((EQ (QUOTE <=) V688) (QUOTE shun.less-than-or-equal-to?)) ((EQ (QUOTE +) V688) (QUOTE shun.add)) ((EQ (QUOTE -) V688) (QUOTE shun.subtract)) ((EQ (QUOTE /) V688) (QUOTE shun.divide)) ((EQ (QUOTE *) V688) (QUOTE shun.multiply)) (T V688))) 



