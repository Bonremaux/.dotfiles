set nocompatible
syntax on
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" =============================
"             PLUGINS
" =============================

Bundle 'gmarik/vundle'
Bundle 'altercation/vim-colors-solarized'
Bundle 'Valloric/YouCompleteMe'
Bundle 'tomtom/tcomment_vim'
Bundle 'vim-scripts/a.vim'
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/unite.vim'
Bundle 'Shougo/neomru.vim'
Bundle 'Shougo/vimfiler.vim'
Bundle 'bling/vim-airline'
Bundle 'osyo-manga/vim-over'

filetype plugin indent on

" =============================
"             SETTINGS
" =============================

set mouse=a
set mousehide

set background=dark
let g:solarized_termcolors=256
colorscheme solarized

set backspace=indent,eol,start

set shiftwidth=4
set tabstop=4
set smarttab
set expandtab

set autoindent
set smartindent
set cindent

set nowrap
set linebreak
set textwidth=80
set colorcolumn=81

set noshowmatch

set showcmd

set encoding=utf-8
set t_Co=256

set hlsearch
set incsearch
set ignorecase

set nobackup
set nowritebackup
set noswapfile

set list
set listchars=tab:▸\ ,extends:❯,precedes:❮,nbsp:␣
set so=7

set ruler

set colorcolumn=80

set virtualedit=onemore

set cursorline

set fillchars+=vert:│

set wildmenu
set wildmode=list:longest,full

set ls=2

set clipboard=unnamedplus

set autoread

set number

au BufNewFile,BufRead *.cl set filetype=c
au BufNewFile,BufRead *.cu set filetype=cpp
au BufNewFile,BufRead *.cuh set filetype=cpp

" =============================
"             MAPPING
" =============================

let mapleader=','

nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-l> <C-W>l
nmap <C-h> <C-W>h

map <Right> <C-w><
map <Up> <C-W>-
map <Down> <C-W>+
map <Left> <C-w>>

nnoremap ; :

inoremap jk <esc>
inoremap kj <esc>

nnoremap U <C-r>

vnoremap < <gv
vnoremap > >gv

vnoremap <c-y> "+y

nmap <leader>, :b#<CR>

nmap <leader>e :set number!<CR>

nmap <leader>p :set paste!<CR>

nmap <leader>w :set wrap!<CR>

nmap <leader>/ :let @/=""<CR>

inoremap <C-p> <C-r>*

" =============================
"             Unite
" =============================

let g:unite_source_history_yank_enable=1
let g:unite_enable_start_insert=1
let g:unite_split_rule="botright"
let g:unite_winheight=10
let g:unite_candidate_icon="▷"
let g:unite_force_overwrite_statusline=0
let g:unite_prompt = '>>> '
let g:unite_marked_icon = '✓'

if executable('ack')
  let g:unite_source_grep_command = 'ack'
  let g:unite_source_grep_default_opts = '--noheading --nocolor --nocolumn --smart-case -k -H'
  let g:unite_source_grep_recursive_opt = ''
endif

call unite#filters#matcher_default#use(['matcher_fuzzy'])
call unite#filters#sorter_default#use(['sorter_rank'])
call unite#custom#source('file_rec/async,file_rec','ignore_pattern','tags')

autocmd FileType unite call s:unite_my_settings()
function! s:unite_my_settings()
    imap <buffer> <TAB> <Plug>(unite_select_next_line)
endfunction

nnoremap <leader>g :Unite -no-start-insert -no-quit grep:.<cr>
nnoremap <leader>G :UniteWithCursorWord -no-start-insert -no-quit grep:.<cr>
nnoremap <leader>y :Unite -buffer-name=history -no-start-insert history/yank<cr>
nnoremap <leader>f :Unite -buffer-name=file file_rec<cr>
nnoremap <space> :Unite -buffer-name=buffer buffer<cr>
nnoremap <leader>l :Unite -buffer-name=search line<cr>
nnoremap <Leader>m :Unite -buffer-name=recent -no-start-insert file_mru<cr>
nnoremap <Leader>T :Unite -buffer-name=tags tag<cr>

" =============================
"              A
" =============================

nnoremap <leader>h :A<cr>
nnoremap <leader>H :AV<cr>

" =============================
"          IndentLine
" =============================

let g:indentLine_color_tty = 235

" =============================
"            airline
" =============================

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

" =============================
"            vimfiler
" =============================

autocmd FileType vimfiler call s:vimfiler_my_settings()

function! s:vimfiler_my_settings()
    unmap <buffer> <C-l>
    unmap <buffer> <C-j>
endfunction

nnoremap <leader>n :VimFilerExplorer -winwidth=25<CR>

" =============================
"             YCM
" =============================

let g:ycm_add_preview_to_completeopt = 0
set completeopt-=preview
let g:ycm_semantic_triggers = {'haskell' : ['.']}
setlocal omnifunc=necoghc#omnifunc

nnoremap gd :YcmCompleter GoTo<CR>

" =============================
"          OTHER STUFF
" =============================

command! -nargs=* -bang Replace call Replace("<bang>", <f-args>)

function! Replace (bang, pattern, replacement)
    let inplace = a:bang == '!'

    if a:pattern == '' || a:replacement == ''
        return
    endif

    if !inplace
        let sed = 'sed s/\\b'.a:pattern.'\\b/'.a:replacement.'/g'
        let ack = 'ack -k -w --group --nocolor '.a:pattern
        cexpr system(sed, system(ack))
        copen
        execute 'match Search /'.a:replacement.'/'
    else
        exe '!ack -l -k -w --print0 '.a:pattern.
                    \' | xargs -0 -n 1 '.
                    \'sed -i "s/\b'.a:pattern.'\b/'.a:replacement.'/g"'
    endif
endfunction

set tags=.tags,tags;
map <F4> :! ctags -R -f .tags .

map <F9> :wa \| silent make! -j2 \| redraw! \| cw<CR>
map <S-F9> :wa \| execute '!' . g:cmakeprg<CR>
map <F10> :execute 'silent !' . g:runprg \| redraw!<CR>
map <F12> :wa \| execute '!' . g:runtestprg<CR>

autocmd SessionLoadPost * call s:LoadLocalVimrc()

function! s:LoadLocalVimrc ()
    if filereadable('.vimrc.local')
        source .vimrc.local
    endif
endfunction

call s:LoadLocalVimrc()

