RE2_BINDIR=re2/bin
PYTHON_BINDIR=python/src
PERL_BINDIR=perl/src
TCL_BINDIR=tcl/src
RE2J_BINDIR=re2j/build/jar
SED_BINDIR=sed/src
AWK_BINDIR=gawk/src
GREP_BINDIR=grep/src
CPP11_BINDIR=cpp11/bin
ONIGURUMA_BINDIR=oniguruma/bin

KLEENEXSOURCES = $(notdir $(wildcard $(KLEENEXDIR)/*.kex))
KLEENEXSOURCES_OBJECTS = $(KLEENEXSOURCES:.kex=.ko)
KLEENEXDIR = kleenex/src

REPG = ../dist/build/repg/repg
DATA_DIR = ../test/data


# Through unimaginable hackery, we can get a hashmap-like thing in make.
# Attach test input file to the programs, to check that the RE2 C++
# programs called <NAME> are equivalent to the Kleenex programs <NAME>.kex
testinput.patho2         := $(DATA_DIR)/strings/ab_lines_small.txt
testinput.csv_project3   := $(DATA_DIR)/csv/csv_format1.sample.csv
testinput.as             := $(DATA_DIR)/strings/as_small.txt
testinput.simple_id      := $(DATA_DIR)/strings/ab_lines_small.txt
testinput.issuu_json2sql := $(DATA_DIR)/issuu/sample.json
testinput.email          := $(DATA_DIR)/email/emails_from_apache.txt

all:
	@echo "Nothing to do.  Run make equality_check to check all programs."

benchmark:
	@echo "TODO..."

# Check that the test data is there.
data_check:
	@[ -f ${testinput.patho2} ] || (echo "Missing test data file: ${testinput.patho2}" && false)
	@[ -f ${testinput.csv_project3} ] || (echo "Missing test data file: ${testinput.csv_project3}"  && false)
	@[ -f ${testinput.as} ] || (echo "Missing test data file: ${testinput.as}" && false)
	@[ -f ${testinput.simple_id} ] || (echo "Missing test data file: ${testinput.simple_id}" && false)
	@[ -f ${testinput.issuu_json2sql} ] || (echo "Missing test data file: ${testinput.json2sql}" && false)
	@[ -f ${testinput.email} ] || (echo "Missing test data file: ${testinput.email}" && false)
	@echo "Test data OK."

# Check that RE2, Python, and Kleenex compute the same output
equality_check: $(KLEENEXSOURCES_OBJECTS)

utils/extracttime:
	@$(REPG) compile --out utils/extracttime --quiet utils/extracttime.kex

%.ko: $(KLEENEXDIR)/%.kex utils/extracttime
	@if [[ "${testinput.$(*F)}" == "" ]] ; then \
		echo No test data specified for $(*F) ; \
		false ; \
	fi
	@echo "------------------------------------------"
	@echo "Checking $(*F)..."
	$(REPG) compile $< --out kex_$(*F) --quiet=true
	./kex_$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	shasum $(*F).out > $(*F).sha

	@bash utils/runequalstest.sh "RE2-f == Kleenex     " \
		$(*F) $(RE2_BINDIR)/$(*F)-f ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "RE2-glns == Kleenex  " \
		$(*F) $(RE2_BINDIR)/$(*F)-glns ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "RE2-gls == Kleenex   " \
		$(*F) $(RE2_BINDIR)/$(*F)-gls ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Python == Kleenex    " \
		$(*F) $(PYTHON_BINDIR)/$(*F).py ${testinput.$(*F)} $(*F).out python
	@bash utils/runequalstest.sh "Perl == Kleenex      " \
		$(*F) $(PERL_BINDIR)/$(*F).pl ${testinput.$(*F)} $(*F).out perl
	@bash utils/runequalstest.sh "Tcl == Kleenex       " \
		$(*F) $(TCL_BINDIR)/$(*F).tcl ${testinput.$(*F)} $(*F).out tclsh
	@bash utils/runequalstest.sh "RE2/J == Kleenex     " \
		$(*F) $(RE2J_BINDIR)/$(*F).jar ${testinput.$(*F)} $(*F).out "java -jar"
	@bash utils/runequalstest.sh "Sed == Kleenex       " \
		$(*F) $(SED_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Awk == Kleenex       " \
		$(*F) $(AWK_BINDIR)/$(*F).awk ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Grep == Kleenex      " \
		$(*F) $(GREP_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "C++11 == Kleenex     " \
		$(*F) $(CPP11_BINDIR)/$(*F) ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Oniguruma == Kleenex " \
		$(*F) $(ONIGURUMA_BINDIR)/$(*F) ${testinput.$(*F)} $(*F).out

# Do some cleanup!
	@rm $(*F).out $(*F).sha kex_$(*F) tmp_timing



# Check each program individually
patho2: patho2.ko
csv_project3: csv_project3.ko
as: as.ko
simple_id: simple_id.ko
issuu_json2sql: issuu_json2sql.ko
email: email.ko

.PHONY: clean patho2 csv_project3 as simple_id issuu_json2sql email
