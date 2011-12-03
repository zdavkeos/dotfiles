""" vimrc file
""" --Zach Davis


"" fix for vi/vim
set nocompatible

"" setup indentation rules
"set autoindent
set smartindent

"" tabwidth 4, use 4 spaces
set tabstop=4
"set shiftwidth=4 " auto-indent amount
"set expandtab " change tabs to spaces

"" make backspace and delete work
set bs=2
"set backspace=indent,eol,start

"" show matching parens
set showmatch

"" show ruler at bottom of screen
set ruler

"" auto re-read files if changed externally
set autoread

"" ignore case when searching
set ignorecase

"" turn off backups
set nobackup
set nowb
set noswapfile

"" fancy colors and stuff
if has("gui_running")
	set guioptions-=T
	set t_Co=256
	colorscheme zenburn
	set nonu
else
	set nonu
endif

" setup system specific stuff
if MySys() == "windows"
     set gfn=Consolas:h10
elseif MySys() == "linux"
     set gfn=Inconsolata\ 12
     set shell=/bin/zsh
endif
