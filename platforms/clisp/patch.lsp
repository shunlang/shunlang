(DEFUN simple-error (String) (ERROR "~A" String))
(COMPILE 'simple-error)
(SETQ shun.*history* NIL)
(EXT:SAVEINITMEM "NewShun.mem" :INIT-FUNCTION 'shun.byteloop)