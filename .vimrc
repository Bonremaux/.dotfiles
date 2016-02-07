set nocompatible
syntax on
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" PLUGINS
" -----------------------------
Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
Bundle 'tomtom/tcomment_vim'
Bundle 'bling/vim-airline'
Bundle 'Valloric/YouCompleteMe'
Bundle 'vim-scripts/a.vim'
Bundle 'drmikehenry/vim-headerguard'
Bundle 'nathanaelkane/vim-indent-guides'
Bundle 'majutsushi/tagbar'
Bundle 'justinmk/vim-syntax-extra'
Bundle 'zah/nim.vim'
Bundle 'ctrlpvim/ctrlp.vim'
Bundle 'bkad/CamelCaseMotion'
Bundle 'tomtom/checksyntax_vim'
Bundle 'godlygeek/tabular'
Bundle 'rking/ag.vim'
Bundle 'mhinz/vim-signify'
Bundle 'marijnh/tern_for_vim'
Bundle 'Olical/vim-enmasse'

Bundle 'altercation/vim-colors-solarized'
Bundle 'yosiat/oceanic-next-vim'
Bundle 'morhetz/gruvbox'
Bundle 'nanotech/jellybeans.vim'
Bundle 'zeis/vim-kolor'

filetype plugin indent on

" SETTINGS
" -----------------------------
set shell=/usr/bin/bash

set mouse=a
set mousehide

set background=dark
let g:solarized_termcolors=256
colorscheme gruvbox

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
set autoread
set number
set clipboard=unnamedplus

" MAPPING
" -----------------------------
let mapleader=','

nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-l> <C-W>l
nmap <C-h> <C-W>h

" map <Right> <C-w><
" map <Up> <C-W>-
" map <Down> <C-W>+
" map <Left> <C-w>>

nnoremap ; :

inoremap jk <esc>
inoremap kj <esc>

nnoremap U <C-r>

nnoremap <leader>, :b#<CR>
nnoremap <leader>l :ls<CR>
nnoremap <leader>b :bp<CR>
nnoremap <leader>f :bn<CR>

nmap <Space> :let @/=""<CR>

" inoremap <C-p> <C-r>*

" nnoremap Y "aY
" nnoremap P "ap

" insert one char
map <C-i> i_<Esc>r

nmap 0 ^

nmap <F2> :set wrap!<CR>
nmap <F3> :set number!<CR>
nmap <F4> :set paste!<CR>
nmap <F5> :e $MYVIMRC<CR>
nmap <S-F5> :source $MYVIMRC<CR>

" A
" -----------------------------
nnoremap <leader>h :A<cr>
nnoremap <leader>H :AV<cr>

" airline
" -----------------------------
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

" NERDTree
" -----------------------------
nnoremap <leader>n :NERDTreeToggle<CR>

" YCM
" -----------------------------
let g:ycm_add_preview_to_completeopt = 0
set completeopt-=preview
let g:ycm_min_num_of_chars_for_completion = 1
let g:ycm_confirm_extra_conf = 0

" manual YCM activation
" YCM auto loading must be disabled by removing autocmd command from the YCM
" sources
command! YCM call youcompleteme#Enable()
nnoremap gd :YcmCompleter GoTo<CR>

if filereadable('.ycm_extra_conf.py')
    autocmd VimEnter * call youcompleteme#Enable()
endif

" Tagbar
" -----------------------------
nnoremap <leader>T :TagbarToggle<CR>
nnoremap <leader>t :TagbarOpenAutoClose<CR>

" Indent Guides
" -----------------------------
let g:indent_guides_enable_on_vim_startup = 0

" CtrlP
" -----------------------------
let g:ctrlp_user_command = [
            \ '.git/',
            \ 'git --git-dir=%s/.git ls-files -oc --exclude-standard'
            \ ]

nnoremap <leader>/ :CtrlPBufTagAll<CR>
nnoremap <leader>m :CtrlPMRUFiles<CR>

" CheckSyntax
" -----------------------------
map <F8> :up <bar> :CheckSyntax<CR>
imap <F8> <ESC>:up <bar> :CheckSyntax<CR>

" OTHER STUFF
" -----------------------------
au BufNewFile,BufRead *.cl set filetype=c
au BufNewFile,BufRead *.cu set filetype=cpp
au BufNewFile,BufRead *.cuh set filetype=cpp
au FileType c,cpp imap #i #include

" set tags=.tags,tags;
" map <F5> :! ctags -R -f .tags .

autocmd SessionLoadPost * call s:LoadLocalVimrc()

function! s:LoadLocalVimrc ()
    if filereadable('.vimrc.local')
        source .vimrc.local
    endif
endfunction

call s:LoadLocalVimrc()

if exists("*Run")
    map <F9> :up <bar> call Run()<CR><CR>
    imap <F9> <ESC>:up <bar> call Run()<CR><CR>
    map <S-F9> :up <bar> call Run()<CR>
endif

" :W saves using sudo
command! W w !sudo tee % > /dev/null
