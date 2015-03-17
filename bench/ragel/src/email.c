
#line 1 "src/email.rl"
#include "../common.h"

/* From the Ragel manual:
        "To guard against this kind of problem one must ensure that the machine
         specification is divided up using boundaries that do not allow 
         ambiguities from one portion of the machine to the next."
*/


char *mark;


#line 33 "src/email.rl"

/*
Call "mark" upon entry to the sub-machine "email".
Call "print" when exiting the sub-maching "email".
*/

#line 23 "src/email.c"
static const char _email_fst_actions[] = {
	0, 1, 0, 1, 1
};

static const char _email_fst_key_offsets[] = {
	0, 0, 1, 14, 26, 30, 36, 41, 
	45, 52, 57
};

static const char _email_fst_trans_keys[] = {
	10, 33, 46, 61, 63, 64, 35, 39, 
	42, 43, 45, 57, 94, 126, 33, 45, 
	61, 63, 35, 39, 42, 43, 47, 57, 
	94, 126, 48, 57, 97, 122, 45, 46, 
	48, 57, 97, 122, 45, 48, 57, 97, 
	122, 48, 57, 97, 122, 10, 45, 46, 
	48, 57, 97, 122, 45, 48, 57, 97, 
	122, 10, 33, 45, 61, 63, 35, 39, 
	42, 43, 47, 57, 94, 126, 0
};

static const char _email_fst_single_lengths[] = {
	0, 1, 5, 4, 0, 2, 1, 0, 
	3, 1, 5
};

static const char _email_fst_range_lengths[] = {
	0, 0, 4, 4, 2, 2, 2, 2, 
	2, 2, 4
};

static const char _email_fst_index_offsets[] = {
	0, 0, 2, 12, 21, 24, 29, 33, 
	36, 42, 46
};

static const char _email_fst_indicies[] = {
	1, 0, 2, 4, 2, 2, 5, 2, 
	2, 2, 2, 3, 2, 2, 2, 2, 
	2, 2, 2, 2, 3, 6, 6, 3, 
	7, 8, 6, 6, 3, 7, 6, 6, 
	3, 9, 9, 3, 10, 11, 8, 9, 
	9, 3, 11, 9, 9, 3, 1, 12, 
	12, 12, 12, 12, 12, 12, 12, 0, 
	0
};

static const char _email_fst_trans_targs[] = {
	1, 10, 2, 0, 3, 4, 5, 6, 
	7, 8, 10, 9, 2
};

static const char _email_fst_trans_actions[] = {
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 3, 0, 1
};

static const int email_fst_start = 10;
static const int email_fst_first_final = 10;
static const int email_fst_error = 0;

static const int email_fst_en_main = 10;


#line 39 "src/email.rl"



int main(int argc, char **argv) {

  PRE

  while(fgets(buffer, sizeof(buffer), stdin)) {
    char *p = &buffer[0];
    char *pe = p + strlen(buffer) + 1;
    
#line 100 "src/email.c"
	{
	cs = email_fst_start;
	}

#line 50 "src/email.rl"
    
#line 107 "src/email.c"
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
	_keys = _email_fst_trans_keys + _email_fst_key_offsets[cs];
	_trans = _email_fst_index_offsets[cs];

	_klen = _email_fst_single_lengths[cs];
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

	_klen = _email_fst_range_lengths[cs];
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
	_trans = _email_fst_indicies[_trans];
	cs = _email_fst_trans_targs[_trans];

	if ( _email_fst_trans_actions[_trans] == 0 )
		goto _again;

	_acts = _email_fst_actions + _email_fst_trans_actions[_trans];
	_nacts = (unsigned int) *_acts++;
	while ( _nacts-- > 0 )
	{
		switch ( *_acts++ )
		{
	case 0:
#line 14 "src/email.rl"
	{
         mark = p;
  }
	break;
	case 1:
#line 17 "src/email.rl"
	{
    fprintf(stdout, "%s", mark);
  }
	break;
#line 193 "src/email.c"
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

#line 51 "src/email.rl"
  }

  POST

  return 0;
}
