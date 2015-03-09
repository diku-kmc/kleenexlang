# Makefile for setting up Oniguruma test

BINDIR=bin
SOURCES = $(wildcard src/*.c)
OBJECTS = $(SOURCES:.cpp=.o)
ONIG_DESTDIR=$(HOME)
TARGZ_FILE := "onig-5.9.6.tar.gz"

CC=gcc
CCFLAGS=-O3 -I$(ONIG_DESTDIR)/usr/local/include -L$(ONIG_DESTDIR)/usr/local/lib -lonig -Wno-pointer-sign

install-oniguruma: build

onig-5.9.6.tar.gz:
	wget --output-document=$(TARGZ_FILE) http://www.geocities.jp/kosako3/oniguruma/archive/onig-5.9.6.tar.gz
	tar xvf $(TARGZ_FILE)

build: onig-5.9.6.tar.gz
	(cd onig-5.9.6 && ./configure)
	(cd onig-5.9.6 && make)
	(cd onig-5.9.6 && make install DESTDIR=$(ONIG_DESTDIR))


%.o: %.c
	rm -f $(BINDIR)/BIN-$(*F)
	mkdir -p $(BINDIR)
	$(CC) $(CCFLAGS) -o $(BINDIR)/BIN-$(*F) $<
	echo "#!/bin/sh" > $(BINDIR)/$(*F)
	echo 'DIR=$$( cd "$$( dirname "$${BASH_SOURCE[0]}" )" && pwd )' >> $(BINDIR)/$(*F)
	echo "DYLD_LIBRARY_PATH=$(ONIG_DESTDIR)/usr/local/lib/ LD_LIBRARY_PATH=\$$DYLD_LIBRARY_PATH \$$DIR/BIN-$(*F)" >> $(BINDIR)/$(*F)
	chmod +x $(BINDIR)/$(*F)


as: src/as.o
email: src/email.o
patho2: src/patho2.o
simple_id: src/simple_id.o
csv_project3: src/csv_project3.o

clean:
	rm -rf bin
