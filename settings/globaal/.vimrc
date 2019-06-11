:set background=dark
:syntax enable
:set ignorecase
:set mouse=a
:set number
:set autoindent

" http://tedlogan.com/techblog3.html
:set noexpandtab
:set tabstop=4
:set shiftwidth=4
:set softtabstop=4

" Haskell wants spaces and I want less indentation:
autocmd FileType haskell setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2
autocmd FileType cabal setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2

" visualize tabs and trailing spaces:
:set list
:set listchars=trail:␣,tab:▸\ 

" Don't enable modelines! They should not be executed
" on untrusted files.

au BufReadPost set nobomb

" Is niet globaal, maar dat moeten we maar later uitzoeken:
" :set background=light

:set formatoptions=tcrq
:set textwidth=80

" maak verbinding met X, want we starten op met vim -X
" zie ook .bashrc en http://markmail.org/message/nwkwulaj4wiuuouu
:call serverlist()

au BufRead,BufNewFile *.ag  set filetype=haskell
au BufRead,BufNewFile *.cag set filetype=haskell
au BufRead,BufNewFile *.il  set filetype=java    " is handig ivm commentaar

" laat tab-switchen werken in urxvt
:nmap <ESC>[6^ <C-PageDown>
:nmap <ESC>[5^ <C-PageUp>
" control-pijltjes in alle modi
:map! <ESC>Od  <C-Left>
:map  <ESC>Od  <C-Left>
:map! <ESC>Oc  <C-Right>
:map  <ESC>Oc  <C-Right>
:map! <ESC>Oa  <C-Up>
:map  <ESC>Oa  <C-Up>
:map! <ESC>Ob  <C-Down>
:map  <ESC>Ob  <C-Down>

