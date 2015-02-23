all: matchall linebased

clean:
	rm -f matchall line_based

matchall:
	c++ -O3 -I/usr/local/include -L/usr/local/lib match.cpp -lre2 -pthread -o matchall

linebased:
	c++ -O3 -I/usr/local/include -L/usr/local/lib line_based.cpp -lre2 -pthread -o line_based

