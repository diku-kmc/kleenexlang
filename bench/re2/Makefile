all: matchall linebased matchall-nc linebased-nc

clean:
	rm -f matchall line_based

matchall:
	c++ -O3 -I/usr/local/include -L/usr/local/lib match.cpp -lre2 -pthread -o matchall

linebased:
	c++ -O3 -I/usr/local/include -L/usr/local/lib line_based.cpp -lre2 -pthread -o line_based


matchall-nc:
	c++ -O3 -I/usr/local/include -D NO_CAPTURE -L/usr/local/lib match.cpp -lre2 -pthread -o matchall-nc

linebased-nc:
	c++ -O3 -I/usr/local/include -D NO_CAPTURE -L/usr/local/lib line_based.cpp -lre2 -pthread -o line_based-nc
