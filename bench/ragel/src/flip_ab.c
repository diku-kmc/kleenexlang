
#line 1 "src/flip_ab.rl"
#include <string.h>
#include <stdio.h>

#define BUFFER_SIZE (200*1024*1024)
#define LINE_LEN 100000000


#line 13 "src/flip_ab.rl"



#line 15 "src/flip_ab.c"
static const char _flip_ab_actions[] = {
	0, 2, 0, 2, 2, 1, 2
};

static const char _flip_ab_key_offsets[] = {
	0, 0
};

static const char _flip_ab_trans_keys[] = {
	97, 98, 0
};

static const char _flip_ab_single_lengths[] = {
	0, 2
};

static const char _flip_ab_range_lengths[] = {
	0, 0
};

static const char _flip_ab_index_offsets[] = {
	0, 0
};

static const char _flip_ab_trans_targs[] = {
	1, 1, 0, 0
};

static const char _flip_ab_trans_actions[] = {
	4, 1, 0, 0
};

static const int flip_ab_start = 1;
static const int flip_ab_first_final = 1;
static const int flip_ab_error = 0;

static const int flip_ab_en_main = 1;


#line 16 "src/flip_ab.rl"

char buffer[BUFFER_SIZE] = {0};
int main(int argc, char **argv) {
  int cs; // Current state
  
  while(fgets(buffer, LINE_LEN, stdin)) {
    char *p = &buffer[0];
    char *pe = p + strlen(buffer) + 1;
    
#line 65 "src/flip_ab.c"
	{
	cs = flip_ab_start;
	}

#line 25 "src/flip_ab.rl"
    
#line 72 "src/flip_ab.c"
	{
	int _klen;
	unsigned int _trans;
	const char *_acts;
	unsigned int _nacts;
	const char *_keys;

	if ( p == pe )
		goto _test_eof;
	if ( cs == 0 )
		goto _out;
_resume:
	_keys = _flip_ab_trans_keys + _flip_ab_key_offsets[cs];
	_trans = _flip_ab_index_offsets[cs];

	_klen = _flip_ab_single_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + _klen - 1;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + ((_upper-_lower) >> 1);
			if ( (*p) < *_mid )
				_upper = _mid - 1;
			else if ( (*p) > *_mid )
				_lower = _mid + 1;
			else {
				_trans += (unsigned int)(_mid - _keys);
				goto _match;
			}
		}
		_keys += _klen;
		_trans += _klen;
	}

	_klen = _flip_ab_range_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + (_klen<<1) - 2;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + (((_upper-_lower) >> 1) & ~1);
			if ( (*p) < _mid[0] )
				_upper = _mid - 2;
			else if ( (*p) > _mid[1] )
				_lower = _mid + 2;
			else {
				_trans += (unsigned int)((_mid - _keys)>>1);
				goto _match;
			}
		}
		_trans += _klen;
	}

_match:
	cs = _flip_ab_trans_targs[_trans];

	if ( _flip_ab_trans_actions[_trans] == 0 )
		goto _again;

	_acts = _flip_ab_actions + _flip_ab_trans_actions[_trans];
	_nacts = (unsigned int) *_acts++;
	while ( _nacts-- > 0 )
	{
		switch ( *_acts++ )
		{
	case 0:
#line 9 "src/flip_ab.rl"
	{ fputs("a", stdout); }
	break;
	case 1:
#line 10 "src/flip_ab.rl"
	{ fputs("b", stdout); }
	break;
	case 2:
#line 12 "src/flip_ab.rl"
	{  }
	break;
#line 157 "src/flip_ab.c"
		}
	}

_again:
	if ( cs == 0 )
		goto _out;
	if ( ++p != pe )
		goto _resume;
	_test_eof: {}
	_out: {}
	}

#line 26 "src/flip_ab.rl"
  }
  return 0;
}
