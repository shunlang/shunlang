all:
	@echo "run make sbcl or make clisp"	

sbcl:
	cp klambda/* platforms/sbcl && cd platforms/sbcl && sbcl --load install.lsp
	mv platforms/sbcl/shun .

clisp:
	cp klambda/* platforms/clisp && cd platforms/clisp && clisp install.lsp
	mv platforms/clisp/shun.mem .
	echo "clisp -M shun.mem -q -m 10MB" > shun && chmod 755 shun 
