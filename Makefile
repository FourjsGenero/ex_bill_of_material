FORMS=$(patsubst %.per,%.42f,$(wildcard *.per))

PROGMOD=billofmat.42m

all: $(PROGMOD) $(FORMS)

run: all
	fglrun billofmat

%.42f: %.per
	fglform -M $<

%.42m: %.4gl
	fglcomp -M $<

clean::
	rm -f *.42?
