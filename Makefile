include Makefile.base

.PHONY: exe
exe: build
	stack exec -- int-like-exe
