
NULL =

GEN = ../EasyJoin

JAVA_TESTS = \
  ArrayIterator \
  Lock \
  File \
  Future \
  ReadWriteLock \
  Semaphore \
  $(NULL)

TARGETS = \
  $(JAVA_TESTS:%=%.java) \
  $(JAVA_TESTS:%=%.class) \
  $(JAVA_TESTS:%=%.pdf) \
  $(NULL)

all: $(TARGETS)

# RULES FOR JAVA TESTS

%.java: %.join.java $(GEN)
	$(GEN) --lr -j $<

%.dot: %.join.java $(GEN)
	$(GEN) --lr -d $<

%.class: %.java
	javac $<

%.dat: %.class
	java $(<:%.class=%) 4 100000 >$@

%.png: %.gp %.dat
	gnuplot $<

%.pdf: %.dot
	dot -Tpdf $< >$@

.PHONY: clean
clean:
	rm -f $(TARGETS) *.dot *.pdf *.class *.dat *.png
