" Vim syntax file
" Language:     Kleenex 
" Author:       Sebastian Paaske Tørholm <sebbe@diku.dk>
" Copyright:    Copyright (c) 2015 The KMC Project
" License:      You may redistribute this under the same terms as Vim itself

if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

syn keyword kleenexTodo TODO XXX FIXME contained
syn region kleenexComment matchgroup=kleenexCommentStart start="/\*" end="\*/" contains=kleenexTodo
syn region kleenexCommentSingleline start="//" end="$" contains=kleenexTodo

syn match kleenexOperator /:=\||/

syn region kleenexString matchgroup=kleenexStringStart start=+"+ skip=+\\\\\|\\"+ end=+"+

syn region kleenexRegex matchgroup=kleenexRegexStart start=+<+ skip=+\\\\\|\\>+ end=+>+
syn region kleenexIgnored matchgroup=kleenexIgnoreRegexStart start=+\~<+ skip=+\\\\\|\\>+ end=+>+

syn match kleenexIgnored /\~\w\+/
syn region kleenexIgnored matchgroup=kleenexIgnoreGroupStart start=+\~(+ end=+)+


syn match kleenexIdentifier /\w\+/

hi def link kleenexOperator            Operator 
hi def link kleenexIdentifier          Identifier 
hi def link kleenexString              String
hi def link kleenexStringStart         kleenexString
hi def link kleenexRegex               Special
hi def link kleenexRegexStart          Type
hi def link kleenexIgnoreRegexStart    Ignore 
hi def link kleenexIgnoreGroupStart    Ignore 
hi def link kleenexIgnored             Ignore 
hi def link kleenexComment             Comment
hi def link kleenexCommentSingleLine   kleenexComment 
hi def link kleenexCommentStart        kleenexComment
hi def link kleenexTodo                Todo

let b:current_syntax = "kleenex"
