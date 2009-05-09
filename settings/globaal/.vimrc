:set background=dark
:syntax enable
:set ignorecase
:set mouse=a
:set number
:set autoindent
:set expandtab
:set tabstop=2
:set shiftwidth=2

" Is niet globaal, maar dat moeten we maar later uitzoeken:
:set background=light

:set hlsearch
:set formatoptions=tcrq
:set textwidth=80

au BufRead,BufNewFile *.ag  set filetype=haskell
au BufRead,BufNewFile *.cag set filetype=haskell
au BufRead,BufNewFile *.il  set filetype=java    " is handig ivm commentaar

