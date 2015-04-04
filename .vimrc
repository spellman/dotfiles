" Change the mapleader from \ to ,
let mapleader=","

" Set not compatible with vi to enable stuff.
set nocompatible

" No swap files
set noswapfile

" Enable Pathogen
call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

" Enable Vundle
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Not My Plugins
Plugin 'gmarik/vundle'
"Plugin 'minibufexpl.vim'
"Plugin 'pangloss/vim-javascript'
Plugin 'tpope/vim-rails'
Plugin 'rking/ag.vim'
Plugin 'jgdavey/tslime.vim'
Plugin 'altercation/vim-colors-solarized'
Plugin 'kien/ctrlp.vim'
Plugin 'guns/vim-clojure-static'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-classpath'
"Plugin 'vim-scripts/paredit.vim'
Plugin 'guns/vim-sexp'
Plugin 'tpope/vim-repeat'
Plugin 'dgrnbrg/vim-redl'
Plugin 'kien/rainbow_parentheses.vim'
"Plugin 'luochen1990/rainbow'

" My Plugins
Plugin 'spellman/vim-minitest'



" Edit vimrc.
nnoremap <Leader>ev :tabedit $MYVIMRC<cr>

" Source vimrc.
nnoremap <Leader>sv :source $MYVIMRC<cr>

"Always show status line, even when only one window is open.
set laststatus=2

" Increase command history from 20 (default) to 100
set history=100

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

augroup myfiletypes
  autocmd!

  " Ruby
  autocmd FileType ruby nnoremap <buffer> <Leader>c I# <esc>

  " Javascript
  autocmd FileType javascript nnoremap <buffer> <Leader>c I// <esc>
  autocmd FileType javascript let g:html_indent_inctags = "html,body,head,tbody"
  autocmd FileType javascript let g:html_indent_script1 = "inc"
  autocmd FileType javascript let g:html_indent_style1 = "inc"

  " Clojure
  autocmd FileType clojure nnoremap <buffer> <Leader>c I; <esc>
  autocmd FileType clojure nmap <buffer> <Leader>r <Plug>fireplace-addon#PrintFireplaceResultOperator
  autocmd FileType clojure vmap <buffer> <Leader>r <Plug>fireplace-addon#PrintFireplaceResultOperator

  " CSS
  " Sort CSS properties
  autocmd FileType html,css nnoremap <buffer> <leader>sc ?{<CR>jV/^\s*\}?$<CR>k:sort<CR>:noh<CR>
augroup END



" Multiple buffers
set hidden

" 'Optimal usage' of project plugin
let g:proj_flags="imstvcg"

set suffixesadd=.rb
set includeexpr+=substitute(v:fname,'s$','','g')

" Don't redraw while executing macros
set lazyredraw



" Color schemes
function ColorWombat()
    syntax on
    " IMPORTANT: Uncomment one of the following lines to force
    " using 256 colors (or 88 colors) if your terminal supports it,
    " but does not automatically use 256 colors by default.
    set t_Co=256
    "set t_Co=88
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
    colorscheme wombat
endfunction

function ColorSolarizedDark()
    syntax on
    " IMPORTANT: Uncomment one of the following lines to force
    " using 256 colors (or 88 colors) if your terminal supports it,
    " but does not automatically use 256 colors by default.
    set t_Co=256
    "set t_Co=88
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
    set background=dark
    let g:solarized_termcolors=256
    colorscheme solarized
endfunction

function ColorSolarizedLight()
    syntax on
    " IMPORTANT: Uncomment one of the following lines to force
    " using 256 colors (or 88 colors) if your terminal supports it,
    " but does not automatically use 256 colors by default.
    set t_Co=256
    "set t_Co=88
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
    set background=light
    let g:solarized_termcolors=256
    colorscheme solarized
endfunction

function ColorGreen()
    syntax on
    " IMPORTANT: Uncomment one of the following lines to force
    " using 256 colors (or 88 colors) if your terminal supports it,
    " but does not automatically use 256 colors by default.
    set t_Co=256
    "set t_Co=88
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
    colorscheme green
endfunction

function ColorDark()
    syntax on
    " IMPORTANT: Uncomment one of the following lines to force
    " using 256 colors (or 88 colors) if your terminal supports it,
    " but does not automatically use 256 colors by default.
    "set t_Co=256
    "set t_Co=88
    "let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
    colorscheme dark
endfunction

function ColorBlackOnWhite()
    syntax on
    " IMPORTANT: Uncomment one of the following lines to force
    " using 256 colors (or 88 colors) if your terminal supports it,
    " but does not automatically use 256 colors by default.
    set t_Co=256
    "set t_Co=88
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
    colorscheme basic
endfunction

function ColorHC()
    syntax off
    set t_Co=0
    set background=dark
endfunction


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

" Clear highlighted searches with ,/
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



" Mini Buffer Explorer
"nnoremap <Leader>b :MiniBufExplorer<cr>
"nnoremap <Leader>c :CMiniBufExplorer<cr>
"nnoremap <Leader>u :UMiniBufExplorer<cr>
"nnoremap <Leader>t :TMiniBufExplorer<cr>

" Put new window above current or on the left for vertical split
"let g:miniBufExplSplitBelow=0



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
au VimEnter * RainbowParenthesesToggleAll
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

let g:rbpt_colorpairs = [
\     ['37', '37'],
\     ['148', '148'],
\     ['29', '29'],
\     ['153', '153'],
\     ['93', '93'],
\     ['22', '22'],
\     ['166', '166'],
\     ['63', '63'],
\     ['28', '28'],
\     ['142', '142'],
\     ['19', '19'],
\     ['41', '41']
\   ]



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
let g:paredit_electric_return = 0



" Vim-Clojure-Static
" Align multiline strings to character after opening quote.
let g:clojure_align_multiline_strings = 1



" Colorscheme to apply on starting Vim.
call ColorDark()
