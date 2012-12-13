OPAM    = ../_obuild/opam/opam.byte
TOPICS  = $(shell $(OPAM) help topics)
HELPFMT = --help=groff

.PHONY: man pdf
all: man pdf

man:
	$(OPAM) $(HELP) > man/opam.1
	for i in $(TOPICS); do\
	  $(OPAM) help $$i $(HELPFMT) > man/opam-$$i.1; \
	done

pdf:
	$(MAKE) -f tutorials