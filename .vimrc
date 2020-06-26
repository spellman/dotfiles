" Set the mapleader.
" let mapleader="\"
map <Space> <Leader>
set showcmd

" Set not compatible with vi to enable stuff.
set nocompatible

" No swap files
set noswapfile

" Enable Vundle.
filetype off
" Set the runtime path to include Vundle and initialize.
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Not My Plugins
" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'
Plugin 'vim-scripts/CSApprox'
Plugin 'sheerun/vim-polyglot'
Plugin 'tpope/vim-vinegar'
Plugin 'tpope/vim-fugitive'
Plugin 'easymotion/vim-easymotion'
Plugin 'pangloss/vim-javascript'
Plugin 'mxw/vim-jsx'
Plugin 'othree/yajs.vim'
Plugin 'othree/javascript-libraries-syntax.vim'
"Plugin 'othree/html5.vim'
Plugin 'jason0x43/vim-js-indent'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-endwise'
Plugin 'rking/ag.vim'
Plugin 'jgdavey/tslime.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'guns/vim-clojure-static'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-classpath'
"Plugin 'vim-scripts/paredit.vim'
"Plugin 'guns/vim-sexp'
Plugin 'tpope/vim-repeat'
"Plugin 'dgrnbrg/vim-redl'
Plugin 'kien/rainbow_parentheses.vim'
"Plugin 'luochen1990/rainbow'
Plugin 'tpope/vim-git'
Plugin 'haya14busa/incsearch.vim'
Plugin 'rickhowe/diffchar.vim'

" All of your Plugins must be added before the following line:
call vundle#end()
filetype plugin indent on


" Edit vimrc.
nnoremap <Leader>ev :tabedit $MYVIMRC<cr>

" Source vimrc.
nnoremap <Leader>sv :source $MYVIMRC<cr>

" Always show status line, even when only one window is open.
set laststatus=2

" Show git status in status line, via vim-fugitive.
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P

" Increase command history from 20 (default) to 1000
set history=1000

" Increase undo history from default to 1000
set undolevels=1000

" Let % switch between opening and closing brackets.
" By sourcing matchit.vim, % can also switch between
"   if/elsif/else/end
"   opening and closing XML tags
"   etc.
runtime macros/matchit.vim

" Match bracket pairs with tab.
nnoremap <tab> %
onoremap <tab> %
vnoremap <tab> %

" <TAB> shows list of possible commands
set wildmode=list:longest

" / searches case-sensitive only if searching for capital letter
" * searches consistently case-sensitive.
set ignorecase
set smartcase

" Highlight search terms...
set hlsearch
set incsearch " ...dynamically as they are typed.

" Keep 3 lines below cursor instead of it being at bottom (default)
set scrolloff=3

" Show line numbers
set number

" Show ruler
set ruler

" Intuitive backspacing in insert mode
set backspace=indent,eol,start

" File-type highlighting and configuration.
" Run :filetype (without args) to see what you may have
" to turn on yourself, or just set them all to be sure.
syntax on
filetype on
filetype plugin on
filetype indent on

" Tab is 2 spaces
set tabstop=2
set softtabstop=2

" Tabs to spaces
set expandtab

" Shift indent by 2 spaces
set shiftwidth=2

" Autoindent on
set autoindent

" c: Auto-wrap comments to textwidth
" j: Remove comment leader when joining lines 'where it makes sense'.
set textwidth=79
set formatoptions=cj

" Strip trailing whitespace in the current file.
nnoremap <leader>sw :%s/\s\+$//<cr>:let @/=''<CR>

" incsearch.vim
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" Multiple buffers
set hidden

" Don't redraw while executing macros
set lazyredraw



" Color schemes
function ColorDark()
    syntax on
    " IMPORTANT: Uncomment one of the following lines to force
    " using 256 colors (or 88 colors) if your terminal supports it,
    " but does not automatically use 256 colors by default.
    set t_Co=256
    "set t_Co=88
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
    colorscheme dark
endfunction

function ColorHC()
    syntax off
    set t_Co=0
    set background=dark
endfunction

" Probably not the good way to do this. See the updates at the bottom of
" http://snk.tuxfamily.org/log/vim-256color-bce.html
if &term =~ '256color'
  " Disable Background Color Erase (BCE) so that color schemes
  " work properly when Vim is used inside tmux and GNU screen.
  " See also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

" <F2> to toggle paste mode for insert
set pastetoggle=<F2>

" Easy navigation of wrapped lines
nnoremap j gj
nnoremap k gk

" Easy window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Clear highlighted searches
nnoremap <silent> <Leader>/ :nohlsearch<CR>

"Apply sudo after opening a file that requires it for editing. Use w!!
cnoremap w!! w !sudo tee % >/dev/null

"Move lines and selections up and down.
nnoremap <silent> <C-Up> :move -2<CR>
nnoremap <silent> <C-Down> :move +<CR>
xnoremap <silent> <C-Up> :move '<-2<CR>gv
xnoremap <silent> <C-Down> :move '>+<CR>gv
imap <silent> <C-Up> <C-O><C-Up>
imap <silent> <C-Down> <C-O><C-Down>
smap <silent> <C-Up> <C-G><C-Up><C-G>
smap <silent> <C-Down> <C-G><C-Down><C-G>

" ctrlp.vim
nnoremap <Leader>p :<C-U>CtrlPMixed<CR>



" Minitest.vim mappings
nmap <Leader>t <Plug>vim-minitest#RunCurrentTestFile
nmap <Leader>s <Plug>vim-minitest#RunNearestTest
nmap <Leader>l <Plug>vim-minitest#RunLastTest
nmap <Leader>a <Plug>vim-minitest#RunAllTests



" Rspec.vim mappings
"nnoremap <Leader>t :call RunCurrentSpecFile()<CR>
"nnoremap <Leader>s :call RunNearestSpec()<CR>
"nnoremap <Leader>l :call RunLastSpec()<CR>
"nnoremap <Leader>a :call RunAllSpecs()<CR>



" tslime
vnoremap <Leader>e <Plug>SendSelectionToTmux
nnoremap <Leader>e <Plug>NormalModeSendToTmux
nnoremap <C-c>v <Plug>SetTmuxVars
" Load ruby file ('ruby load')
nnoremap <Leader>rl :call Send_to_Tmux("load '".@%."'\n")<CR>



"Rainbow parentheses for plugin kien/rainbow_parentheses.vim
let g:rbpt_colorpairs = [
\     ['117', '#93E0E3'],
\     ['172', '#D0BF8F'],
\     ['47', '#BFEBBF'],
\     ['253', '#DADADA'],
\     ['24', '#366060'],
\     ['28', '#7F9F7F'],
\     ['130', '#DFAF8F'],
\     ['69', '#6CA0A3'],
\     ['40', '#8FB28F'],
\     ['155', '#E0CF9F'],
\     ['111', '#94BFF3'],
\     ['119', '#9FC59F']
\   ]
" NOTE: For some reason, it starts with the fourth pair down and goes up.
autocmd VimEnter * RainbowParenthesesToggleAll
autocmd BufEnter * RainbowParenthesesLoadRound
autocmd BufEnter * RainbowParenthesesLoadSquare
autocmd BufEnter * RainbowParenthesesLoadBraces



" Rainbow parentheses for plugin luochen1990/rainbow
"let g:rainbow_active = 1
"let g:rainbow_conf = {
"\   'ctermfgs': ['lightblue', 'darkmagenta', 'darkred', 'lightred', 'yellow', 'gray'],
"\   'operators': '_,_',
"\   'parentheses': [['(',')'], ['\[','\]'], ['{','}']],
"\   'separately': {
"\       '*': {},
"\       'ruby': {
"\           'parentheses': [['(',')'], ['\[','\]'], ['{','}'], ['\<do\>', '\<end\>']],
"\       },
"\       'html': {
"\           'parentheses': [['(',')'], ['\[','\]'], ['{','}'], ['<\a[^>]*[^/]>\|<\a>','</[^>]*>'], ['<meta','>'], ['<link','>']],
"\       },
"\       'eruby': {
"\           'parentheses': [['(',')'], ['\[','\]'], ['{','}'], ['\<do\>', '\<end\>'], ['<\a[^>]*[^/]>\|<\a>','</[^>]*>'], ['<%','%>'], ['<%=','%>'], ['<%-','%>']],
"\       },
"\       'tex': {
"\           'operators': '',
"\           'parentheses': [['(',')'], ['\[','\]']],
"\       },
"\   }
"\}

" Paredit for plugin vim-scripts/paredit.vim
"let g:paredit_electric_return = 0


" Send command Android device to bring up the React Native developer menu.
nnoremap <Leader>rr :!adb shell input keyevent 82 <enter>



" Vim-Clojure-Static
" Align multiline strings to character after opening quote.
let g:clojure_align_multiline_strings = 1



" Colorscheme to apply on starting Vim.
call ColorDark()
