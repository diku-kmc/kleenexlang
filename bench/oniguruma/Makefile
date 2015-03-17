# Makefile for setting up Oniguruma test

BINDIR=bin
SOURCES = $(wildcard src/*.c)
OBJECTS = $(SOURCES:.c=.o)
ONIG_SRCDIR=onig-5.9.6
ONIG_DESTDIR=$(HOME)
TARGZ_FILE := "onig-5.9.6.tar.gz"

CC=gcc
CCFLAGS=-O3 -I$(ONIG_DESTDIR)/usr/local/include -L$(ONIG_DESTDIR)/usr/local/lib -lonig -Wno-pointer-sign

all: $(OBJECTS)

%.o: %.c
	rm -f $(BINDIR)/BIN-$(*F)
	mkdir -p $(BINDIR)
	$(CC) $(CCFLAGS) -o $(BINDIR)/BIN-$(*F) $<
	@echo "#!/bin/sh" > $(BINDIR)/$(*F)
	@echo 'DIR=$$( cd "$$( dirname "$${BASH_SOURCE[0]}" )" && pwd )' >> $(BINDIR)/$(*F)
	@echo "DYLD_LIBRARY_PATH=$(ONIG_DESTDIR)/usr/local/lib/ LD_LIBRARY_PATH=\$$DYLD_LIBRARY_PATH \$$DIR/BIN-$(*F)" >> $(BINDIR)/$(*F)
	@chmod +x $(BINDIR)/$(*F)

as: src/as.o
email: src/email.o
patho2: src/patho2.o
simple_id: src/simple_id.o
csv_project3: src/csv_project3.o
iso_datetime_to_json: src/iso_datetime_to_json.o


.phony: clean

clean:
	rm -rf bin


install-oniguruma:
	./getlibonig.sh $(ONIG_SRCDIR) $(ONIG_DESTDIR)

