RE2_BINDIR=re2/bin
PYTHON_BINDIR=python/src
PERL_BINDIR=perl/src
TCL_BINDIR=tcl/src
RE2J_BINDIR=re2j/build/jar
SED_BINDIR=sed/src
AWK_BINDIR=gawk/src

HASSOURCES = $(notdir $(wildcard $(HASEDDIR)/*.has))
HASSOURCES_OBJECTS = $(HASSOURCES:.has=.ho)
HASEDDIR = hased_src

REPG = ../dist/build/repg/repg
DATA_DIR = ../test/data

red='\033[0;31m'
green='\033[0;32m'
NC='\033[0m' # No Color

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
		echo ${red}No test data for $(*F)${NC} ; \
		false ; \
	fi
	@echo "------------------------------------------"
	@echo "Checking $(*F)..."
	$(REPG) compile $< --out has_$(*F) --quiet=true
	./has_$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	shasum $(*F).out > $(*F).sha

	@if [ -f $(RE2_BINDIR)/$(*F) ];  then \
		printf "RE2 == Hased?     " ; \
		$(RE2_BINDIR)/$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null  ; \
		shasum -c $(*F).sha &> /dev/null && echo ${green}OK${NC} || echo ${red}FAIL${NC} ; \
	else\
		echo "Can't find $(RE2_BINDIR)/$(*F).  Skipping.";\
	fi

	@if [ -f $(PYTHON_BINDIR)/$(*F).py ]; then \
		printf "Python == Hased?  " ; \
		python $(PYTHON_BINDIR)/$(*F).py < ${testinput.$(*F)} > $(*F).out 2> /dev/null ; \
		shasum -c $(*F).sha &> /dev/null && echo ${green}OK${NC} || echo ${red}FAIL${NC} ; \
	else \
		echo "Can't find $(PYTHON_BINDIR)/$(*F).py.  Skipping."; \
	fi

	@if [ -f $(PERL_BINDIR)/$(*F).pl ]; then \
		printf "Perl == Hased?    " ; \
		perl $(PERL_BINDIR)/$(*F).pl < ${testinput.$(*F)} > $(*F).out 2> /dev/null ; \
		shasum -c $(*F).sha &> /dev/null && echo ${green}OK${NC} || echo ${red}FAIL${NC} ; \
	else \
		echo "Can't find $(PERL_BINDIR)/$(*F).pl  Skipping."; \
	fi


	@if [ -f $(TCL_BINDIR)/$(*F).tcl ]; then \
		printf "Tcl == Hased?     " ; \
		tclsh $(TCL_BINDIR)/$(*F).tcl < ${testinput.$(*F)} > $(*F).out 2> /dev/null ; \
		shasum -c $(*F).sha &> /dev/null && echo ${green}OK${NC} || echo ${red}FAIL${NC} ; \
	else \
		echo "Can't find $(TCL_BINDIR)/$(*F).tcl  Skipping."; \
	fi


	@if [ -f $(RE2J_BINDIR)/$(*F).jar ]; then \
		printf "RE2/J == Hased?   " ; \
		java -jar $(RE2J_BINDIR)/$(*F).jar < ${testinput.$(*F)} > $(*F).out 2> /dev/null ; \
		shasum -c $(*F).sha &> /dev/null && echo ${green}OK${NC} || echo ${red}FAIL${NC} ; \
	else \
		echo "Can't find $(RE2J_BINDIR)/$(*F).jar  Skipping."; \
	fi

	@if [ -f $(SED_BINDIR)/$(*F).sh ]; then \
		printf "Sed == Hased?     " ; \
		bash $(SED_BINDIR)/$(*F).sh < ${testinput.$(*F)} > $(*F).out 2> /dev/null ; \
		shasum -c $(*F).sha &> /dev/null && echo ${green}OK${NC} || echo ${red}FAIL${NC} ; \
	else \
		echo "Can't find $(SED_BINDIR)/$(*F).sh  Skipping."; \
	fi

	@if [ -f $(AWK_BINDIR)/$(*F).awk ]; then \
		printf "Awk == Hased?     " ; \
		$(AWK_BINDIR)/$(*F).awk < ${testinput.$(*F)} > $(*F).out 2> /dev/null ; \
		shasum -c $(*F).sha &> /dev/null && echo ${green}OK${NC} || echo ${red}FAIL${NC} ; \
	else \
		echo "Can't find $(AWK_BINDIR)/$(*F).awk  Skipping."; \
	fi

# Do some cleanup!
	@rm $(*F).out $(*F).sha has_$(*F)



# Check each program individually
patho2: patho2.ho
csv_project3: csv_project3.ho
as: as.ho
simple_id: simple_id.ho
issuu_json2sql: issuu_json2sql.ho
email: email.ho
aaa: aaa.ho

.PHONY: clean
