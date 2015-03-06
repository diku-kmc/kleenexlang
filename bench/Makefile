RE2_BINDIR=re2/bin
PYTHON_BINDIR=python/src
PERL_BINDIR=perl/src
TCL_BINDIR=tcl/src
RE2J_BINDIR=re2j/build/jar
SED_BINDIR=sed/src
AWK_BINDIR=gawk/src
GREP_BINDIR=grep/src
CPP11_BINDIR=cpp11/bin

HASSOURCES = $(notdir $(wildcard $(HASEDDIR)/*.has))
HASSOURCES_OBJECTS = $(HASSOURCES:.has=.ho)
HASEDDIR = hased_src

REPG = ../dist/build/repg/repg
DATA_DIR = ../test/data


# Through unimaginable hackery, we can get a hashmap-like thing in make.
# Attach test input file to the programs, to check that the RE2 C++
# programs called <NAME> are equivalent to the Hased programs <NAME>.has
testinput.patho2         := $(DATA_DIR)/strings/ab_lines_small.txt
testinput.csv_project3   := $(DATA_DIR)/csv/csv_format1.sample.csv
testinput.as             := $(DATA_DIR)/strings/as_small.txt
testinput.simple_id      := $(DATA_DIR)/strings/ab_lines_small.txt
testinput.issuu_json2sql := $(DATA_DIR)/issuu/sample.json
testinput.email          := $(DATA_DIR)/email/emails_from_apache.txt

all:
	@echo "Nothing to do.  Run make equality_check to check all programs."

# Check that the test data is there.
data_check:
	@[ -f ${testinput.patho2} ] || (echo "Missing test data file: ${testinput.patho2}" && false)
	@[ -f ${testinput.csv_project3} ] || (echo "Missing test data file: ${testinput.csv_project3}"  && false)
	@[ -f ${testinput.as} ] || (echo "Missing test data file: ${testinput.as}" && false)
	@[ -f ${testinput.simple_id} ] || (echo "Missing test data file: ${testinput.simple_id}" && false)
	@[ -f ${testinput.issuu_json2sql} ] || (echo "Missing test data file: ${testinput.json2sql}" && false)
	@[ -f ${testinput.email} ] || (echo "Missing test data file: ${testinput.email}" && false)
	@echo "Test data OK."

# Check that RE2, Python, and Hased compute the same output
equality_check: data_check $(HASSOURCES_OBJECTS)

%.ho: $(HASEDDIR)/%.has
	@if [[ "${testinput.$(*F)}" == "" ]] ; then \
		echo ${red}No test data specified for $(*F)${NC} ; \
		false ; \
	fi
	@echo "------------------------------------------"
	@echo "Checking $(*F)..."
	$(REPG) compile $< --out has_$(*F) --quiet=true
	./has_$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	shasum $(*F).out > $(*F).sha

	@bash utils/runequalstest.sh "RE2 == Hased?    " \
		$(*F) $(RE2_BINDIR)/$(*F) ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Python == Hased? " \
		$(*F) $(PYTHON_BINDIR)/$(*F).py ${testinput.$(*F)} $(*F).out python
	@bash utils/runequalstest.sh "Perl == Hased?   " \
		$(*F) $(PERL_BINDIR)/$(*F).pl ${testinput.$(*F)} $(*F).out perl
	@bash utils/runequalstest.sh "Tcl == Hased?    " \
		$(*F) $(TCL_BINDIR)/$(*F).tcl ${testinput.$(*F)} $(*F).out tclsh
	@bash utils/runequalstest.sh "RE2/J == Hased?  " \
		$(*F) $(RE2J_BINDIR)/$(*F).jar ${testinput.$(*F)} $(*F).out "java -jar"
	@bash utils/runequalstest.sh "Sed == Hased?    " \
		$(*F) $(SED_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Awk == Hased?    " \
		$(*F) $(AWK_BINDIR)/$(*F).awk ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Grep == Hased?   " \
		$(*F) $(GREP_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "C++11 == Hased?  " \
		$(*F) $(CPP11_BINDIR)/$(*F) ${testinput.$(*F)} $(*F).out

# Do some cleanup!
	@rm $(*F).out $(*F).sha has_$(*F) tmp_timing



# Check each program individually
patho2: patho2.ho
csv_project3: csv_project3.ho
as: as.ho
simple_id: simple_id.ho
issuu_json2sql: issuu_json2sql.ho
email: email.ho

.PHONY: clean patho2 csv_project3 as simple_id issuu_json2sql email
