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

" My Bundles
Bundle 'spellman/vim-minitest'

" Other Bundles
Bundle 'gmarik/vundle'
"Bundle 'minibufexpl.vim'
"Bundle 'pangloss/vim-javascript'
Bundle 'rking/ag.vim'
Bundle 'jgdavey/tslime.vim'
Bundle 'altercation/vim-colors-solarized'
Bundle 'kien/ctrlp.vim'
"Bundle 'luochen1990/rainbow'
Bundle 'guns/vim-clojure-static'
Bundle 'guns/vim-clojure-highlight'
Bundle 'tpope/vim-fireplace'
Bundle 'tpope/vim-classpath'
"Bundle 'kien/rainbow_parentheses.vim'
Bundle 'vim-scripts/paredit.vim'



" Edit vimrc.
nnoremap <leader>ev :vsp $MYVIMRC<cr>

" Source vimrc.
nnoremap <leader>sv :source $MYVIMRC<cr>

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

" <TAB> shows list of possible commands
set wildmode=list:longest

" / searches case-sensitive only if searching for capital letter
" * searches consistently case-sensitive. 
set ignorecase 
set smartcase

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

" Highlight search terms...
set hlsearch
set incsearch " ...dynamically as they are typed.

" Tab is 2 spaces
set tabstop=2

" Shift indent by 2 spaces
set shiftwidth=2

" Tabs to spaces
set expandtab

augroup myfiletypes
  autocmd!
  " Autoindent with two spaces, always expand tabs
  autocmd FileType ruby,eruby,yaml,javascript set ai sw=2 sts=2 et
  autocmd FileType html,css,scss setlocal ai sw=2 sts=2 et

  " Ruby
  autocmd FileType ruby nnoremap <buffer> <leader>c I#<esc>

  " Javascript
  autocmd FileType javascript nnoremap <buffer> <leader>c I//<esc>

  " Clojure
  autocmd FileType clojure nnoremap <buffer> <leader>c I;<esc>
augroup END



" Javascript stuff
let g:html_indent_inctags = "html,body,head,tbody"
let g:html_indent_script1 = "inc"
let g:html_indent_style1 = "inc"



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
    set t_Co=256
    "set t_Co=88
    let g:CSApprox_attr_map = { 'bold' : 'bold', 'italic' : '', 'sp' : '' }
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

set pastetoggle=<F2>

" Ease navigation of wrapped lines
nnoremap j gj
nnoremap k gk

" Easy window navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Clear highlighted searches with ,/
nnoremap <silent> ,/ :nohlsearch<CR>

"Apply sudo after opening a file that requires it for editing. Use w!!
cnoremap w!! w !sudo tee % >/dev/null



" Mini Buffer Explorer
"nnoremap <Leader>b :MiniBufExplorer<cr>
"nnoremap <Leader>c :CMiniBufExplorer<cr>
"nnoremap <Leader>u :UMiniBufExplorer<cr>
"nnoremap <Leader>t :TMiniBufExplorer<cr>

" Put new window above current or on the left for vertical split
let g:miniBufExplSplitBelow=0



" Minitest.vim mappings
nnoremap <Leader>t :call RunCurrentTestFile()<CR>
nnoremap <Leader>s :call RunNearestTest()<CR>
nnoremap <Leader>l :call RunLastTest()<CR>
nnoremap <Leader>a :call RunAllTests()<CR>



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



" Fireplace
"
" Documentation Settings
" ----------------------
" <leader>m - view docs for a function
"nnoremap <buffer> <leader>d <Plug>FireplaceK
"
" <leader>M - view source of function
"nnoremap <buffer> <leader>D <Plug>FireplaceSource
"
"
" Eval Settings
" ----------------------
" <leader>ee - evaluate whole file
"nnoremap <buffer> <leader>ee :%Eval<CR>
"
" <leader>el - evaluates outermost form for the current line
"nnoremap <buffer> <leader>el :Eval<CR>
"
" <leader>ei - evaluates local form (nested part of function)
"exe 'nnoremap <buffer> <leader>ei <Plug>FireplaceEditab' . &cedit . 'i<CR>'
"
" <leader>tt - run clojure test
"nnoremap <buffer> <leader>tt :Eval (clojure.test/run-tests)<CR>
"
"
" 'Require' Settings
" ----------------------
" <leader>rr -  require<leader> reload
"nnoremap <buffer> <leader>rr :Require<CR>
"
" <leader>R  -  require, reload-all
"nnoremap <buffer> <leader>R :Require!<CR>
"
"
" Quasi-REPL Settings
" ----------------------
" <leader>qr - one-line repl (to quickly evaluate an expression)
"nmap <buffer> <leader>qr <Plug>FireplacePrompt
"
" <leader>Q ten-line repl (to see previously evaluated expressions)
"exe 'nmap <buffer> <leader>Q <Plug>FireplacePrompt' . &cedit . 'i'



" Rainbow parentheses for plugin kien/rainbow_parentheses.vim
"au VimEnter * RainbowParenthesesToggleAll
"au Syntax * RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
"au Syntax * RainbowParenthesesLoadBraces

"let g:rbpt_colorpairs = [
"\     ['lightred',   'firebrick3'],
"\     ['darkred',    'DarkOrchid3'],
"\     ['darkmagenta',    'SeaGreen3'],
"\     ['lightblue',       'RoyalBlue3'],
"\     ['gray',     'SeaGreen3'],
"\     ['yellow',    'RoyalBlue3'],
"\   ]



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



" Colorscheme to apply on starting Vim.
call ColorDark()
