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

(DEFUN shun.pvar? (X) (IF (AND (ARRAYP X) (NOT (STRINGP X)) (EQ (SVREF X 0) 'shun.pvar))
                          'true
                          'false))

(DEFUN shun.lazyderef (X ProcessN)
   (IF (AND (ARRAYP X) (NOT (STRINGP X)) (EQ (SVREF X 0) 'shun.pvar))
       (LET ((Value (shun.valvector X ProcessN)))
            (IF (EQ Value 'shun.-null-)
                X
                (shun.lazyderef Value ProcessN)))
       X))
                                    
(DEFUN shun.valvector (Var ProcessN)
  (SVREF (SVREF shun.*prologvectors* ProcessN) (SVREF Var 1)))               

(DEFUN shun.unbindv (Var N)
  (LET ((Vector (SVREF shun.*prologvectors* N)))
       (SETF (SVREF Vector (SVREF Var 1)) 'shun.-null-)))

(DEFUN shun.bindv (Var Val N)
   (LET ((Vector (SVREF shun.*prologvectors* N)))
        (SETF (SVREF Vector (SVREF Var 1)) Val)))

(DEFUN shun.copy-vector-stage-1 (V2828 V2829 V2830 V2831)
 (COND ((= V2831 V2828) V2830)
  (T
   (shun.copy-vector-stage-1 (1+ V2828) V2829
    (address-> V2830 V2828 (<-address V2829 V2828)) V2831))))

(DEFUN shun.copy-vector-stage-2 (V2835 V2836 V2837 V2838)
 (COND ((= V2836 V2835) V2838)
  (T
   (shun.copy-vector-stage-2 (1+ V2835) V2836 V2837
    (address-> V2838 V2835 V2837)))))

(DEFUN shun.newpv (N)
  (LET ((Count+1 (1+ (THE INTEGER (SVREF shun.*varcounter* N))))
        (Vector (SVREF shun.*prologvectors* N)))
       (SETF (SVREF shun.*varcounter* N) Count+1)
       (IF (= (THE INTEGER Count+1) (THE INTEGER (limit Vector))) 
           (shun.resizeprocessvector N Count+1)
           'skip)
       (shun.mk-pvar Count+1)))

(DEFUN vector-> (Vector N X)
  (IF (ZEROP N) 
      (ERROR "cannot access 0th element of a vector~%")
      (address-> Vector N X)))

(DEFUN <-vector (Vector N)
  (IF (ZEROP N) 
      (ERROR "cannot access 0th element of a vector~%")
       (let VectorElement (SVREF Vector N)
          (IF (EQ VectorElement (fail))
              (ERROR "vector element not found~%")
              VectorElement))))

(DEFUN variable? (X)
 (IF (AND (SYMBOLP X) (NOT (NULL X)) (UPPER-CASE-P (CHAR (SYMBOL-NAME X) 0)))
     'true 
     'false))

(DEFUN shun.+string? (X) (IF (AND (STRINGP X) (NOT (STRING-EQUAL X "")))
                            'true
                            'false))

(DEFUN thaw (F) (FUNCALL F))

(DEFUN pr (X S) 
      (WRITE-STRING X S) 
      (WHEN (OR (EQ S *stoutput*) (EQ S *stinput*)) 
        (FORCE-OUTPUT S)) 
      X) 

(DEFUN shun.lookup-func (F SymbolTable)
   (LET ((Entry (ASSOC F SymbolTable :TEST 'EQ)))
      (IF (NULL Entry)
          (ERROR "~A has no lambda expansion~%" F)
          (CDR Entry))))
