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
CAT_BINDIR=cat/src
RAGEL_BINDIR=ragel/bin
CUT_BINDIR=cut/src
TR_BINDIR=tr/src

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
testinput.ini2json       := $(DATA_DIR)/ini/php.ini
testinput.iso_datetime_to_json		 := $(DATA_DIR)/datetime/datetime_sample.txt
testinput.url            := $(DATA_DIR)/url/test_urls.txt
testinput.rot13			 := $(DATA_DIR)/strings/random_small.txt
testinput.csv2json       := $(DATA_DIR)/csv/csv_format1.sample.csv
testinput.apache_log     := $(DATA_DIR)/apache_log/example.log
testinput.flip_ab        := $(DATA_DIR)/strings/ab_lines_small.txt

all:
	@echo "Nothing to do."
	@echo "make -k equality_check: run a sanity check on the benchmark programs."
	@echo "make install-benchmark-dependencies: install needed stuff to run benchmark."
	@echo "make build-benchmark-programs: build the benchmark programs (not Kleenex)."
	@echo "make clean-benchmark-programs: delete the built benchmark programs."
	@echo "make generate-test-data: generate needed test data."

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
	@[ -f ${testinput.ini2json} ] || (echo "Missing test data file: ${testinput.ini2json}" && false)
	@[ -f ${testinput.iso_datetime_to_json} ] || (echo "Missing test data file: ${testinput.iso_datetime_to_json}" && false)
	@[ -f ${testinput.rot13} ] || (echo "Missing test data file: ${testinput.rot13}" && false)
	@[ -f ${testinput.csv2json} ] || (echo "Missing test data file: ${testinput.csv2json}" && false)
	@echo "Test data OK."

# Check that RE2, Python, and Kleenex compute the same output
equality_check: $(KLEENEXSOURCES_OBJECTS)

utils/extracttime:
	@$(REPG) compile --out utils/extracttime --quiet utils/extracttime.kex

%.ko: $(KLEENEXDIR)/%.kex utils/extracttime
	@bash -c "if [[ \"${testinput.$(*F)}\" == \"\" ]] ; then \
		echo No test data specified for $(*F) ; \
		false ; \
	fi"
	@echo "------------------------------------------"
	@echo "Checking $(*F)..."
	$(REPG) compile $< --out kex_$(*F) --quiet=true
	./kex_$(*F) < ${testinput.$(*F)} > $(*F).out 2> /dev/null
	shasum $(*F).out > $(*F).sha

	@bash utils/runequalstest.sh "RE2 == Kleenex       " \
		$(*F) $(RE2_BINDIR)/$(*F) ${testinput.$(*F)} $(*F).out
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
	@bash utils/runequalstest.sh "Awk-noregex == Kleenex       " \
		$(*F) $(AWK_BINDIR)/$(*F)-native.awk ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Grep == Kleenex      " \
		$(*F) $(GREP_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "C++11 == Kleenex     " \
		$(*F) $(CPP11_BINDIR)/$(*F) ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Oniguruma == Kleenex " \
		$(*F) $(ONIGURUMA_BINDIR)/$(*F) ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Cat == Kleenex       " \
		$(*F) $(CAT_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Ragel-T1 == Kleenex     " \
		$(*F) $(RAGEL_BINDIR)/$(*F)-T1 ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Ragel-F1 == Kleenex     " \
		$(*F) $(RAGEL_BINDIR)/$(*F)-F1 ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Ragel-G2 == Kleenex     " \
		$(*F) $(RAGEL_BINDIR)/$(*F)-G2 ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Cut == Kleenex       " \
		$(*F) $(CUT_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out
	@bash utils/runequalstest.sh "Tr == Kleenex       " \
		$(*F) $(TR_BINDIR)/$(*F).sh ${testinput.$(*F)} $(*F).out


# Do some cleanup!
	@rm $(*F).out $(*F).sha kex_$(*F) tmp_timing

# So hacky. Special cased because it's two-phase.
ini2json.ko: $(KLEENEXDIR)/ini2json.kex  $(KLEENEXDIR)/ini_stripcomments.kex utils/extracttime
	@bash -c "if [[ \"${testinput.ini2json}\" == \"\" ]] ; then \
		echo No test data specified for ini2json ; \
		false ; \
	fi"
	@echo "------------------------------------------"
	@echo "Checking ini2json..."
	$(REPG) compile $(KLEENEXDIR)/ini_stripcomments.kex --out kex_ini_stripcomments --quiet=true
	$(REPG) compile $< --out kex_ini2json --quiet=true
	./kex_ini_stripcomments < ${testinput.ini2json} | ./kex_ini2json > ini2json.out 2> /dev/null
	shasum ini2json.out > ini2json.sha

	@bash utils/runequalstest.sh "RE2 == Kleenex     " \
		ini2json $(RE2_BINDIR)/ini2json ${testinput.ini2json} ini2json.out
	@bash utils/runequalstest.sh "Python == Kleenex    " \
		ini2json $(PYTHON_BINDIR)/ini2json.py ${testinput.ini2json} ini2json.out python
	@bash utils/runequalstest.sh "Perl == Kleenex      " \
		ini2json $(PERL_BINDIR)/ini2json.pl ${testinput.ini2json} ini2json.out perl
	@bash utils/runequalstest.sh "Tcl == Kleenex       " \
		ini2json $(TCL_BINDIR)/ini2json.tcl ${testinput.ini2json} ini2json.out tclsh
	@bash utils/runequalstest.sh "RE2/J == Kleenex     " \
		ini2json $(RE2J_BINDIR)/ini2json.jar ${testinput.ini2json} ini2json.out "java -jar"
	@bash utils/runequalstest.sh "Sed == Kleenex       " \
		ini2json $(SED_BINDIR)/ini2json.sh ${testinput.ini2json} ini2json.out
	@bash utils/runequalstest.sh "Awk == Kleenex       " \
		ini2json $(AWK_BINDIR)/ini2json.awk ${testinput.ini2json} ini2json.out
	@bash utils/runequalstest.sh "Grep == Kleenex      " \
		ini2json $(GREP_BINDIR)/ini2json.sh ${testinput.ini2json} ini2json.out
	@bash utils/runequalstest.sh "C++11 == Kleenex     " \
		ini2json $(CPP11_BINDIR)/ini2json ${testinput.ini2json} ini2json.out
	@bash utils/runequalstest.sh "Oniguruma == Kleenex " \
		ini2json $(ONIGURUMA_BINDIR)/ini2json ${testinput.ini2json} ini2json.out
	@bash utils/runequalstest.sh "Cat == Kleenex       " \
		ini2json $(CAT_BINDIR)/ini2json.sh ${testinput.ini2json} ini2json.out


# Do some cleanup!
	@rm ini2json.out ini2json.sha kex_ini2json kex_ini_stripcomments tmp_timing

# Check each program individually
patho2: patho2.ko
csv_project3: csv_project3.ko
as: as.ko
simple_id: simple_id.ko
issuu_json2sql: issuu_json2sql.ko
email: email.ko
ini2json: ini2json.ko
iso_datetime_to_json: iso_datetime_to_json.ko
rot13: rot13.ko
csv2json: csv2json.ko
apache_log: apache_log.ko
flip_ab: flip_ab.ko

.PHONY: clean patho2 csv_project3 as simple_id issuu_json2sql email ini2json iso_datetime_to_json


generate-test-data: generate-strings download-issuu generate-emails generate-csv generate-datetime generate-inifile generate-apachelog

generate-strings: generate-ab-strings generate-a-strings generate-random-string

generate-ab-strings:
	@echo "Generating 250 MB a/b lines with average line length of 1000..."
	(cd ../test/data/strings && ./gen_ab_lines.pl -l 1000 -s 250000000 > ab_lines_len1000_250mb.txt)
	@echo "Generating 250 MB a/b lines with average line length of 80..."
	(cd ../test/data/strings && ./gen_ab_lines.pl -l 80 -s 250000000 > ab_lines_len80_250mb.txt)

generate-a-strings:
	@echo "Generating 100 MB \"a\"s on one line..."
	(cd ../test/data/strings && ./gen_ab_lines.pl -a -s 100000000 > as_100mb.txt)
generate-random-string:
	@echo "Generating 250 MB random characters..."
	(cd ../test/data/strings && ./gen_random_chars.sh 250000000 > random_250mb.txt)

generate-emails:
	@echo "Generating 100 MB email addresses..."
	(cd ../test/data/email && ./gen_emails.pl -s 100000000 > emails_100mb.txt)

generate-csv:
	@echo "Generating 250 MB \"format 1\" csv data..."
	(cd ../test/data/csv && ./gen_csv.pl -f 1 -s 250000000 > csv_format1_250mb.csv)

generate-datetime:
	@echo "Generating 250 MB ISO datetime stamps..."
	(cd ../test/data/datetime && ./gen_datetime.pl -s 250000000 > datetimes_250mb.txt)

generate-inifile:
	@echo "Generating a 25 MB .ini file..."
	(cd ../test/data/ini && ./gen_ini.pl -s 25000000 > inifile_25mb.ini)

generate-urls:
	@echo "Generating 100 MB URLs..."
	(cd ../test/data/url && ./gen_url.pl -s 100000000 > urls_100mb.txt)

generate-apachelog:
	@echo "Generating an 250 MB apache log..."
	(cd ../test/data/apache_log && ./generate_big_log.sh)

download-issuu:
	@echo "Downloading 1,000,000 json objects from the /first/ part of the Issuu dataset..."
	(cd ../test/data/issuu && ./download.sh -f -c 0 -n 1000000 > issuu_1000000objs.json)


install-benchmark-dependencies:
	@echo "Installing benchmark deps..."
	@echo "Nothing to do for cat"
	@echo "Nothing to do for cpp11"
	@echo "Nothing to do for gawk"
	@echo "Nothing to do for grep"
	@echo "Installing oniguruma library..."
	(cd oniguruma && make install-oniguruma)
	@echo "Nothing to do for Perl"
	@echo "Nothing to do for Python"
	@echo "Installing Ragel..."
	(cd ragel && make install-ragel)
	@echo "Installing RE2..."
	(cd re2 && make install-libre2)
	@echo "Checking 'ant' for RE2J..."
	(cd re2j && ./getant.sh)
	@echo "Nothing to do for sed"
	@echo "Installing tcl..."
	(cd tcl && make install-tcl)
	@echo "DONE"

build-benchmark-programs:
	@echo "Building benchmark programs (not Kleenex programs)..."
	@echo "Nothing to do for cat"
	@echo "Building cpp11 programs..."
	(cd cpp11 && make)
	@echo "Nothing to do for gawk"
	@echo "Nothing to do for grep"
	@echo "Building oniguruma programs..."
	(cd oniguruma && make)
	@echo "Nothing to do for Perl"
	@echo "Nothing to do for Python"
	@echo "Building Ragel programs..."
	(cd ragel && make)
	@echo "Building RE2 programs..."
	(cd re2 && make)
	@echo "Building RE2J programs..."
	(cd re2j && ./ant jar)
	@echo "Nothing to do for sed"
	@echo "Nothing to do for tcl..."
	@echo "DONE"

clean-benchmark-programs:
	@echo "Cleaning benchmark programs (not Kleenex programs)..."
	@echo "Nothing to do for cat"
	@echo "Cleaning cpp11 programs..."
	(cd cpp11 && make clean)
	@echo "Nothing to do for gawk"
	@echo "Nothing to do for grep"
	@echo "Cleaning oniguruma programs..."
	(cd oniguruma && make clean)
	@echo "Nothing to do for Perl"
	@echo "Nothing to do for Python"
	@echo "Cleaning Ragel programs..."
	(cd ragel && make clean)
	@echo "Cleaning RE2 programs..."
	(cd re2 && make clean)
	@echo "Cleaning RE2J programs..."
	(cd re2j && ./ant clean)
	@echo "Nothing to do for sed"
	@echo "Nothing to do for tcl..."
	@echo "DONE"
