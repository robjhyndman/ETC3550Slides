SOURCES := $(wildcard *.qmd)
TARGETS=$(SOURCES:%.qmd=%.pdf)

%.pdf: %.qmd
	@echo "$< -> $@"
	quarto render '$<'

default: $(TARGETS)

clean:
	rm -rf $(TARGETS)
	rm -rf *_cache
	rm -rf *_files
