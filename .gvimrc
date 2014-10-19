set guifont=Inconsolata\ 13

set mousehide		" Hide the mouse when typing text

set guioptions-=m
set guioptions-=T
set guioptions-=r
set guioptions-=L

map <silent> <F11>
\    :call system("wmctrl -ir " . v:windowid . " -b toggle,fullscreen")<CR>

