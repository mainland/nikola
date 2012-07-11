GHC=ghc

HAPPY=happy 
HAPPY_ARGS=-agci

ALEX=alex 
ALEX_ARGS=-gi

SOURCE = $(shell find src -type f)

GHC_FLAGS += -W -Werror \
	-isrc -idist/build \
	-Iinclude -Idist/build \
	-DHMATRIX \
	-lcuda \
	-optP-include -optPdist/build/autogen/cabal_macros.h

GHC_FLAGS += \
	-hide-all-packages \
	-package base \
	-package bytestring \
	-package containers \
	-package criterion \
	-package cuda \
	-package exception-transformers \
	-package hmatrix \
	-package language-c-quote \
	-package logict \
	-package mainland-pretty \
	-package mersenne-random-pure64 \
	-package mtl \
	-package process \
	-package srcloc \
	-package statistics \
	-package syb \
	-package symbol \
	-package template-haskell \
	-package uniplate \
	-package vector \
	-package vector-algorithms

TARGETS		= demo search nikola blackscholes blackscholes-opt scan scan-opt radix radix-opt

.PHONY : all
all : $(TARGETS)

.PHONY : clean
clean :
	$(_QUIET)rm -rf obj blackscholes-obj scan-obj radix-obj $(TARGETS)

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
