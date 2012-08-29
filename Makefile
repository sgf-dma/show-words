
prog		:= show_words

# Commands and flags.
RM		:= rm -rvf
GHC		:= ghc
ghc_flags	= -Wall -outputdir $(build_dir)
include_flags	= -i$(sources_dir)

# Sources structure.
.SUFFIXES:
sources_dir	:= src
build_dir	:= bin
vpath %.hs $(sources_dir)

# Source filenames.
prog	:= $(addprefix $(build_dir)/, $(prog))
tmp_sources := $(addprefix $(sources_dir)/,)
sources := $(filter-out $(tmp-sources), $(wildcard $(sources_dir)/*.hs))
objects := $(addprefix $(build_dir)/, $(notdir $(sources:.hs=.o))  Main.o)
ifaces  := $(addprefix $(build_dir)/, $(notdir $(sources:.hs=.hi)) Main.hi)
docs	:= README

# (see static pattern rules for details)
$(prog) : $(build_dir)/% : %.hs
	$(GHC) $(ghc_flags) $(include_flags) --make $< -o $@

.PHONY: clean cleanobj cleanifaces
clean : cleanobj cleanifaces
	$(RM) $(prog)
cleanobj :
	$(RM) $(objects)
cleanifaces :
	$(RM) $(ifaces)

