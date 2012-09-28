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
#GHC_FLAGS+=-ddump-splices

GHC_FLAGS += \
	-hide-all-packages \
	-package HUnit \
	-package QuickCheck \
	-package base \
	-package bytestring \
	-package containers \
	-package criterion \
	-package cuda \
	-package deepseq \
	-package exception-transformers \
	-package gloss \
	-package hmatrix \
	-package language-c-quote \
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
	-package test-framework \
	-package test-framework-hunit \
	-package test-framework-quickcheck2 \
	-package text \
	-package transformers \
	-package uniplate \
	-package vector \
	-package vector-algorithms

TARGETS	= \
	mandelbrot unit \
	american \
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

american : $(SOURCE) examples/american/*.hs examples/american/American/*.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/american/Main.hs \
		-odir obj -hidir obj \
		-iexamples/american $(GHC_FLAGS) -o $@

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

test : $(SOURCE) test.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make test.hs \
		-odir obj -hidir obj \
		$(GHC_FLAGS) -o $@

mandelbrot : $(SOURCE) $(shell find examples/mandelbrot -type f)
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make examples/mandelbrot/Main.hs \
		-iexamples/mandelbrot \
		-odir obj/mandelbrot -hidir obj/mandelbrot \
		$(GHC_FLAGS) -o $@

unit : $(SOURCE) tests/unit/Main.hs
	@echo "Compiling and linking" $@
	$(_QUIET)$(GHC) --make -itests/unit tests/unit/Main.hs \
		-odir obj -hidir obj \
		$(GHC_FLAGS) -o $@

CRITERION_TEMPLATE=${HOME}/.cabal/ghc-7.4.2-linux-x86_64/share/criterion-0.6.0.1/templates/report.tpl

blackscholes.csv : ./dist/build/blackscholes/blackscholes
	$< -q -s 10 -u $@

blackscholes.html : ./dist/build/blackscholes/blackscholes
	$< -q -s 10 -t $(CRITERION_TEMPLATE) -o $@

blackscholes_openmp.c : blackscholes-compile
	./blackscholes-compile -O 1 --openmp --func blackscholes_openmp -o $@

blackscholes_cuda.cu : blackscholes-compile
	./blackscholes-compile -O 1 --cuda --func blackscholes_cuda -o $@

blackscholes-openmp : examples/blackscholes-compile/blackscholes.cc blackscholes_openmp.c
	gcc -std=c99 -O6 -fno-trapping-math -ftree-vectorize -msse3 -fopenmp -DOPENMP $^ -lm -o $@

blackscholes-cuda : examples/blackscholes-compile/blackscholes.cc blackscholes_cuda.cu
	nvcc -DCUDA --gpu-architecture=compute_20 $^ -lm -o $@

american.csv : ./dist/build/american/american
	$< -q -s 10 -u $@

american.html : ./dist/build/american/american
	$< -q -s 10 -t $(CRITERION_TEMPLATE) -o $@
