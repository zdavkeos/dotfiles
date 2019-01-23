""" vimrc file
""" --Zach Davis


"" fix for vi/vim
set nocompatible

"" setup indentation rules
"set autoindent
set smartindent

"" tabwidth 4 "use 4 spaces
set tabstop=4
"set shiftwidth=4 " auto-indent amount
"set expandtab " change tabs to spaces

" view whitespace in list mode
set listchars=tab:»·,trail:·

"" make backspace and delete work
set bs=2
"set backspace=indent,eol,start

"" show matching parens
set showmatch

"" show ruler at bottom of screen
set ruler

" allow cursor to roam past the end of the line
set virtualedit=onemore

" auto re-read files if changed externally
set autoread

"" ignore case when searching
"set ignorecase
set smartcase

"" turn off backups
set nobackup
set nowb
set noswapfile

" fancy colors and stuff
syntax on
if has("gui_running")
	set guioptions-=T
	set t_Co=256
	set background=dark
	let g:solarized_italic=0

	colorscheme solarized
	" colorscheme zenburn
	set lines=50
	set columns=110
	"set nonu
else
	set nonu
endif

" syntax highlighting for REL, RC and SKM files
augroup filetypedetect
au BufNewFile,BufRead *.geojson setf javascript
au BufNewFile,BufRead *.mpf setf ngc
augroup END

" setup system specific stuff
if MySys() == "windows"
     set gfn=Consolas:h10
elseif MySys() == "linux"
     set gfn=Inconsolata\ 10
     set shell=/bin/zsh
endif
