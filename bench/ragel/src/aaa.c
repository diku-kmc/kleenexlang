
#line 1 "src/aaa.rl"
#include "../common.h"

char *mark;


#line 14 "src/aaa.rl"



#line 13 "src/aaa.c"
static const char _aaa_fst_actions[] = {
	0, 1, 0
};

static const char _aaa_fst_key_offsets[] = {
	0, 0, 1, 2, 3, 4, 5, 6, 
	7
};

static const char _aaa_fst_trans_keys[] = {
	97, 97, 97, 97, 97, 97, 97, 97, 
	0
};

static const char _aaa_fst_single_lengths[] = {
	0, 1, 1, 1, 1, 1, 1, 1, 
	1
};

static const char _aaa_fst_range_lengths[] = {
	0, 0, 0, 0, 0, 0, 0, 0, 
	0
};

static const char _aaa_fst_index_offsets[] = {
	0, 0, 2, 4, 6, 8, 10, 12, 
	14
};

static const char _aaa_fst_trans_targs[] = {
	2, 0, 3, 0, 4, 0, 5, 0, 
	6, 0, 8, 0, 1, 0, 1, 0, 
	0
};

static const char _aaa_fst_trans_actions[] = {
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 1, 0, 
	0
};

static const char _aaa_fst_eof_actions[] = {
	0, 0, 0, 0, 0, 0, 0, 0, 
	1
};

static const int aaa_fst_start = 7;
static const int aaa_fst_first_final = 7;
static const int aaa_fst_error = 0;

static const int aaa_fst_en_main = 7;


#line 17 "src/aaa.rl"



int main(int argc, char **argv) {

  PRE

  while(fgets(buffer, sizeof(buffer), stdin)) {
    char *p = &buffer[0];
    char *pe = p + strlen(buffer) + 1;
    
#line 79 "src/aaa.c"
	{
	cs = aaa_fst_start;
	}

#line 28 "src/aaa.rl"
    
#line 86 "src/aaa.c"
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
	_keys = _aaa_fst_trans_keys + _aaa_fst_key_offsets[cs];
	_trans = _aaa_fst_index_offsets[cs];

	_klen = _aaa_fst_single_lengths[cs];
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

	_klen = _aaa_fst_range_lengths[cs];
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
	cs = _aaa_fst_trans_targs[_trans];

	if ( _aaa_fst_trans_actions[_trans] == 0 )
		goto _again;

	_acts = _aaa_fst_actions + _aaa_fst_trans_actions[_trans];
	_nacts = (unsigned int) *_acts++;
	while ( _nacts-- > 0 )
	{
		switch ( *_acts++ )
		{
	case 0:
#line 7 "src/aaa.rl"
	{ fputs("0", stdout); }
	break;
#line 163 "src/aaa.c"
		}
	}

_again:
	if ( cs == 0 )
		goto _out;
	if ( ++p != pe )
		goto _resume;
	_test_eof: {}
	if ( p == eof )
	{
	const char *__acts = _aaa_fst_actions + _aaa_fst_eof_actions[cs];
	unsigned int __nacts = (unsigned int) *__acts++;
	while ( __nacts-- > 0 ) {
		switch ( *__acts++ ) {
	case 0:
#line 7 "src/aaa.rl"
	{ fputs("0", stdout); }
	break;
#line 183 "src/aaa.c"
		}
	}
	}

	_out: {}
	}

#line 29 "src/aaa.rl"
  }

  POST

  return 0;
}
