
" set accordingly
fun! MySys()
  return "linux"
"  return "windows"
endfun

if MySys() == "windows"
	" settings from windows _vimrc
	behave mswin
	set lines=40
	set columns=110

	set runtimepath=$VIMRUNTIME,C:\\Users\\zdavis\\.vim
	source C:\\Users\\zdavis\\.vim\\.vimrc
else
	set runtimepath=~/.vim,$VIMRUNTIME
	source ~/.vim/.vimrc
endif
