" Vim filetype detection file
" Language:     Kleenex 
" Author:       Sebastian Paaske Tørholm <sebbe@diku.dk>
" Copyright:    Copyright (c) 2015 The KMC Project
" License:      You may redistribute this under the same terms as Vim itself

if &compatible || v:version < 603
    finish
endif

autocmd BufRead,BufNewFile *.kex setfiletype kleenex

" vim: set et ts=4 :
