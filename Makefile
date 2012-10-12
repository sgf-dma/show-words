
prog_name	:= show_words

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

# All filenames, including test ones. Hence, when using these variables
# (objects, ifaces) in 'cleanX' target, test programs will also be deleted.
tmp_sources  := $(addprefix $(sources_dir)/, utf8.hs opts.hs)
sources := $(filter-out $(tmp_sources), $(wildcard $(sources_dir)/*.hs))
ifaces  := $(addprefix $(build_dir)/, $(notdir $(sources:.hs=.hi)) Main.hi)
objects := $(addprefix $(build_dir)/, $(notdir $(sources:.hs=.o))  Main.o)
# Choose test filenames from "all". Such filter ensures, that test filenames
# are always subset of "all".
test_sources	:= $(filter $(sources_dir)/test%.hs, $(sources))
test_names	:= $(notdir $(test_sources:.hs=))
test_bins	:= $(addprefix $(build_dir)/, $(test_names))
prog_bin    := $(addprefix $(build_dir)/, $(prog_name))
binaries    := $(prog_bin) $(test_bins)
docs	:= README

# All targets must be PHONY, because make does not know all dependencies and,
# hence, can't track changes in them (to decide whether to update target). GHC
# will do this on its own.
# Here i make corresponding "binary name" from program "name" ..
# (see static pattern rules for details)
.PHONY: $(prog_name) $(test_names)
$(prog_name) $(test_names) : % : $(build_dir)/%
.PHONY: all
all : $(prog_bin) $(test_bins)

# .. and here i compile, but only known "binary names".
# (see static pattern rules for details)
.PHONY: $(prog_bin) $(test_bins)
$(prog_bin) $(test_bins) : $(build_dir)/% : %.hs
	mkdir -p $(build_dir)
	$(GHC) $(ghc_flags) $(include_flags) --make $< -o $@

.PHONY: echo
echo $(test) :
	echo $(test_sources2)

.PHONY: clean cleanobj cleanifaces
clean : cleanobj cleanifaces
	$(RM) $(binaries)
cleanobj :
	$(RM) $(objects)
cleanifaces :
	$(RM) $(ifaces)

