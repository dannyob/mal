SOURCES_BASE = 
SOURCES_LISP = 
SOURCES = $(SOURCES_BASE) $(SOURCES_LISP)

#####################

SRCS = step0_repl.hs  step1_read_print.hs  step2_eval.hs  step3_env.hs
OTHER_SRCS =  Reader.hs Printer.hs Eval.hs MalType.hs Env.hs
BINS = $(SRCS:%.hs=%)

#####################

all: $(BINS)

dist: mal

mal: $(word $(words $(BINS)),$(BINS))
	cp $< $@

$(BINS): %: %.hs $(OTHER_SRCS)
	ghc --make $< -o $@

clean:
	rm -f $(BINS) mal *.hi *.o

.PHONY: stats stats-lisp tests $(TESTS)

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*--|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
