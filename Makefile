
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
tmp_sources := $(addprefix $(sources_dir)/, utf8.hs opts.hs)
sources := $(filter-out $(tmp_sources), $(wildcard $(sources_dir)/*.hs))
objects := $(addprefix $(build_dir)/, $(notdir $(sources:.hs=.o))  Main.o)
ifaces  := $(addprefix $(build_dir)/, $(notdir $(sources:.hs=.hi)) Main.hi)
docs	:= README

# I need to list all sources as prerequisitives in order to rebuild project,
# if any changes. I need '%.hs' to make '$<' match to correct source file.
# (see static pattern rules for details)
$(prog) : $(build_dir)/% : %.hs $(sources)
	$(GHC) $(ghc_flags) $(include_flags) --make $< -o $@

.PHONY: clean cleanobj cleanifaces
clean : cleanobj cleanifaces
	$(RM) $(prog)
cleanobj :
	$(RM) $(objects)
cleanifaces :
	$(RM) $(ifaces)

