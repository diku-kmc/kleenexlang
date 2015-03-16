FOMA=foma-0.9.17

foma: $(FOMA)
	cd $(FOMA) && make && cd .. & ln -s $(FOMA)/foma foma && ln -s $(FOMA)/flookup flookup && ln -s $(FOMA)/cgflookup cgflookup

.phony: clean
clean:
	rm -rf $(FOMA)
	rm -f foma flookup cgflookup

$(FOMA):
	wget https://foma.googlecode.com/files/$(FOMA).tar.gz -O - | tar zx
