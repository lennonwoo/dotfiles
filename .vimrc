syntax enable
" set mouse=a
" set background=light
" colorscheme solarized

set number
set relativenumber
set incsearch hlsearch
set ignorecase smartcase
set clipboard=unnamed
set scrolloff=5
set showmode
set showmatch
set clipboard+=unnamedplus

map ! <Home>
map 0 <End>
map <C-m> :nohlsearch<cr>

inoremap <C-v> <Esc>pa
inoremap <C-d> <delete>
inoremap <C-a> <Home>
inoremap <C-e> <End>
inoremap <C-f> <right>
inoremap <C-b> <left>
nnoremap Y y$
nnoremap <Space>dd ggVGxi

" tab
nnoremap <A-1> 1gt
nnoremap <A-2> 2gt
nnoremap <A-3> 3gt
nnoremap <A-4> 4gt
nnoremap <A-5> 5gt
nnoremap <A-6> 6gt
nnoremap <A-7> 7gt
nnoremap <A-8> 8gt
nnoremap <A-9> 9gt
nnoremap <A-e> 1gtgT
map <C-h> gT
map <C-l> gt

inoremap <A-1> <esc>1gtli
inoremap <A-2> <esc>2gtli
inoremap <A-3> <esc>3gtli
inoremap <A-4> <esc>4gtli
inoremap <A-5> <esc>5gtli
inoremap <A-6> <esc>6gtli
inoremap <A-7> <esc>7gtli
inoremap <A-8> <esc>8gtli
inoremap <A-9> <esc>9gtli
inoremap <A-e> <esc>1gtgTli

" For clipboard
" nnoremap yy "+y
" vnoremap y "+y

" tmux config
if exists('$TMUX')
  set term=screen-256color
endif
