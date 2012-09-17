GHC=ghc

SOURCE = $(shell find src -type f)

#include Makefile.language-c-quote
include Makefile.cu

GHC_FLAGS += -W -Werror \
	-isrc -idist/build \
	-Iinclude -Iinternal -Idist/build \
	-DHMATRIX \
	-optP-include -optPdist/build/autogen/cabal_macros.h

GHC_FLAGS+=-O

#GHC_FLAGS+=-O2
#GHC_FLAGS+=-Odph -rtsopts -threaded
#GHC_FLAGS+=-fno-liberate-case \
#	   -funfolding-use-threshold1000 \
#	   -funfolding-keeness-factor1000
#GHC_FLAGS+=-fllvm -optlo-O3

#GHC_FLAGS+=-ddump-to-file
#GHC_FLAGS+=-ddump-simpl
#GHC_FLAGS+=-dsuppress-all -dppr-case-as-let -dppr-cols200

GHC_FLAGS += \
	-hide-all-packages \
	-package HUnit \
	-package base \
	-package bytestring \
	-package containers \
	-package criterion \
	-package cuda-0.4.1.2 \
	-package exception-transformers \
	-package hmatrix \
	-package language-c-quote-0.4.1 \
	-package logict \
	-package mainland-pretty \
	-package mersenne-random-pure64 \
	-package mtl \
	-package process \
	-package primitive \
	-package repa \
	-package srcloc \
	-package statistics \
	-package syb \
	-package symbol \
	-package template-haskell \
	-package text \
	-package transformers \
	-package uniplate \
	-package vector \
	-package vector-algorithms

TARGETS	= \
	mandel unit \
	append-delayed append-push \
	blackscholes blackscholes-compile \
	blackscholes-cuda blackscholes-openmp

#TARGETS = test bs demo search nikola blackscholes blackscholes-opt scan scan-opt radix radix-opt unit

.PHONY : all
all : $(TARGETS)

.PHONY : clean
clean :
	$(_QUIET)rm -rf *obj $(TARGETS)

GHC_OPT_FLAGS = -O2 -optc-O3

#
# Targets
#

search : $(SOURCE) examples/search/*.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/search/Main.hs \
		-odir obj -hidir obj \
		-iexamples/search $(GHC_FLAGS) -o $@

search-prof : search
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/search/Main.hs \
		-odir obj -hidir obj \
		-iexamples/search $(GHC_FLAGS)  -prof -auto-all -osuf po -o $@

nikola : $(SOURCE) examples/nikola.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/nikola.hs \
		-odir obj -hidir obj \
		-iexamples/nikola $(GHC_FLAGS) -o $@

demo : $(SOURCE) tests/Main.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make tests/Main.hs \
		-itests -odir obj -hidir obj \
		$(GHC_FLAGS) -o $@

blackscholes : $(SOURCE) examples/blackscholes/*.hs examples/blackscholes/BlackScholes/*.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/blackscholes/Main.hs \
		-odir obj -hidir obj \
		-iexamples/blackscholes $(GHC_FLAGS) -o $@

blackscholes-opt : $(SOURCE) examples/blackscholes/*.hs examples/blackscholes/BlackScholes/*.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/blackscholes/Main.hs \
		-odir blackscholes-obj -hidir blackscholes-obj \
		-iexamples/blackscholes $(GHC_FLAGS) $(GHC_OPT_FLAGS) -o $@

blackscholes-compile : $(SOURCE) examples/blackscholes-compile/*.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/blackscholes-compile/Main.hs \
		-odir obj/blackscholes-compile -hidir obj/blackscholes-compile \
		-iexamples/blackscholes-compile $(GHC_FLAGS) -o $@

scan : $(SOURCE) examples/scan.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/scan.hs \
		-odir obj -hidir obj \
		-iexamples/scan $(GHC_FLAGS) -o $@

scan-opt : $(SOURCE) examples/scan.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/scan.hs \
		-odir scan-obj -hidir scan-obj \
		-iexamples/scan $(GHC_FLAGS) $(GHC_OPT_FLAGS) -o $@

radix : $(SOURCE) examples/radix/Main.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/radix/Main.hs \
		-odir obj -hidir obj \
		-iexamples/radix $(GHC_FLAGS) -o $@

radix-opt : $(SOURCE) examples/radix/Main.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/radix/Main.hs \
		-odir radix-obj -hidir radix-obj \
		-iexamples/radix $(GHC_FLAGS) $(GHC_OPT_FLAGS) -o $@

test : $(SOURCE) test.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make test.hs \
		-odir obj -hidir obj \
		$(GHC_FLAGS) -o $@

mandel : $(SOURCE) examples/mandel/Mandel.hs examples/mandel/Main.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/mandel/Main.hs \
		-iexamples/mandel \
		-odir obj/mandel -hidir obj/mandel \
		$(GHC_FLAGS) -o $@

unit : $(SOURCE) tests/unit/Main.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make -itests/unit tests/unit/Main.hs \
		-odir obj -hidir obj \
		$(GHC_FLAGS) -o $@

append-push : $(SOURCE) examples/append-push/*.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/append-push/Main.hs \
		-odir obj/append-push -hidir obj/append-push \
		-iexamples/append-push $(GHC_FLAGS) -o $@

append-delayed : $(SOURCE) examples/append-delayed/*.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/append-delayed/Main.hs \
		-odir obj/append-delayed -hidir obj/append-delayed \
		-iexamples/append-delayed $(GHC_FLAGS) -o $@

matmul : $(SOURCE) examples/matmul.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/matmul.hs \
		-odir obj -hidir obj \
		-isrc \
	        -hide-package monads-fd \
	        -hide-package monads-tf \
		$(GHC_FLAGS) -o $@

blackschole.csv : ./dist/build/blackscholes/blackscholes
	$< -q -s 10 >$@

blackscholes_openmp.c : blackscholes-compile
	./blackscholes-compile -O 1 --openmp --func blackscholes_openmp -o $@

blackscholes_cuda.cu : blackscholes-compile
	./blackscholes-compile -O 1 --cuda --func blackscholes_cuda -o $@

blackscholes-openmp : examples/blackscholes-compile/blackscholes.cc blackscholes_openmp.c
	gcc -std=c99 -O6 -fno-trapping-math -ftree-vectorize -msse3 -fopenmp -DOPENMP $^ -lm -o $@

blackscholes-cuda : examples/blackscholes-compile/blackscholes.cc blackscholes_cuda.cu
	nvcc -DCUDA --gpu-architecture=compute_20 $^ -lm -o $@
