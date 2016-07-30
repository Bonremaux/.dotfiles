set nocompatible
syntax on
filetype off
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

" PLUGINS
" -----------------------------
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'tomtom/tcomment_vim'
Plugin 'bling/vim-airline'
Plugin 'Shougo/deoplete.nvim'
Plugin 'Shougo/neoinclude.vim'
Plugin 'Rip-Rip/clang_complete'
Plugin 'vim-scripts/a.vim'
Plugin 'drmikehenry/vim-headerguard'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'majutsushi/tagbar'
Plugin 'justinmk/vim-syntax-extra'
Plugin 'zah/nim.vim'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'bkad/CamelCaseMotion'
Plugin 'tomtom/checksyntax_vim'
Plugin 'godlygeek/tabular'
Plugin 'rking/ag.vim'
Plugin 'mhinz/vim-signify'
Plugin 'marijnh/tern_for_vim'
Plugin 'Olical/vim-enmasse'
Plugin 'rust-lang/rust.vim'
Plugin 'racer-rust/vim-racer'
Plugin 'svermeulen/vim-easyclip'
Plugin 'tpope/vim-repeat'
Plugin 'keith/swift.vim'

Plugin 'altercation/vim-colors-solarized'
Plugin 'yosiat/oceanic-next-vim'
Plugin 'morhetz/gruvbox'
Plugin 'nanotech/jellybeans.vim'
Plugin 'zeis/vim-kolor'

call vundle#end()
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
set colorcolumn=120

set noshowmatch
set showcmd

" set encoding=utf-8
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
set virtualedit=onemore
set cursorline
set fillchars+=vert:│
set wildmenu
set wildmode=list:longest,full
set ls=2
set autoread
set number
set clipboard=unnamedplus
set completeopt-=preview
set hidden
set undofile
set undodir=~/.vim/.undo

" MAPPING
" -----------------------------
let mapleader=','

nmap <C-j> <C-W>j
nmap <C-k> <C-W>k
nmap <C-l> <C-W>l
nmap <C-h> <C-W>h

nnoremap ; :

inoremap jk <esc>
inoremap kj <esc>

nnoremap U <C-r>

nnoremap <leader>, :b#<CR>
nnoremap <leader>b :bp<CR>
nnoremap <leader>f :bn<CR>

nmap <Space> :let @/=""<CR>

" insert one char
map <C-i> i_<Esc>r

nnoremap H ^
nnoremap L $

nmap <F2> :set wrap!<CR>
nmap <F3> :set number!<CR>
nmap <F4> :set paste!<CR>
nmap <F5> :e $MYVIMRC<CR>
nmap <S-F5> :source $MYVIMRC<CR>

nnoremap <leader>Q :q<CR>
nnoremap <leader>q :bd<CR>

" Use Q for formatting the current paragraph (or visual selection)
vnoremap Q gq
nnoremap Q gqap

nnoremap <Tab> %
vnoremap <Tab> %

" Quote words under cursor
nnoremap <leader>" viW<esc>a"<esc>gvo<esc>i"<esc>gvo<esc>3l
nnoremap <leader>' viW<esc>a'<esc>gvo<esc>i'<esc>gvo<esc>3l

" Quote current selection
" TODO: This only works for selections that are created "forwardly"
vnoremap <leader>" <esc>a"<esc>gvo<esc>i"<esc>gvo<esc>ll
vnoremap <leader>' <esc>a'<esc>gvo<esc>i'<esc>gvo<esc>ll

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

" deoplete
" -----------------------------
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smart_case = 1

let g:racer_cmd = "/usr/bin/racer"
let $RUST_SRC_PATH = "/usr/src/rust/src/"

let g:clang_library_path = "/usr/lib/libclang.so"

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
nnoremap <Leader>r :%s/\<<C-r><C-w>\>//gI<Left><Left><Left>
nnoremap <Leader>R :%s/\<<C-r><C-w>\>//gIc<Left><Left><Left><Left>
vnoremap <Leader>r "hy:%s/<C-r>h//gI<left><left><left>
vnoremap <Leader>R "hy:%s/<C-r>h//gIc<left><left><left><left>

au BufNewFile,BufRead *.cl set filetype=c
au BufNewFile,BufRead *.cu set filetype=cpp
au BufNewFile,BufRead *.cuh set filetype=cpp
au FileType c,cpp imap #i #include

autocmd SessionLoadPost * call s:LoadLocalVimrc()

function! s:LoadLocalVimrc ()
    if filereadable('.vimrc.local')
        source .vimrc.local
    endif
endfunction

call s:LoadLocalVimrc()

if exists("*Run")
    map <F9> :up <bar> call Run()<CR>
    imap <F9> <ESC>:up <bar> call Run()<CR>
    map <S-F9> :up <bar> call Run()<CR>
endif

if exists("*RunTests")
    map <F10> :up <bar> call RunTests()<CR>
    imap <F10> <ESC>:up <bar> call RunTests()<CR>
endif
