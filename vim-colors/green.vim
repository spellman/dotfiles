" Vim color file
" Name: green.vim

if !has("gui_running") && &t_Co != 256 && &t_Co != 88
	echomsg ""
	echomsg "err: please use GUI or a 256-color terminal or 88-color terminal"
	echomsg ""
	finish
endif

if &background == "light"
	set background=dark
endif

hi clear

if exists("syntax_on")
  syntax reset
endif

let g:colors_name = 'green'

hi Cursor		       guifg=#00ff00                 gui=bold,reverse  ctermfg=034  ctermbg=000  cterm=bold,reverse
hi Normal		       guifg=#00a900  guibg=#000000  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi Visual                         guibg=#001500  gui=reverse                    ctermbg=000  cterm=reverse

hi Comment	       guifg=#008220  guibg=#000000  gui=none          ctermfg=028  ctermbg=000  cterm=none

hi Constant	       guifg=#1fc700  guibg=#001c00  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi String                                                          ctermfg=034  ctermbg=000  cterm=none
hi Identifier      guifg=#50d930  guibg=#000000  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi Function                                                        ctermfg=034  ctermbg=000  cterm=none
hi Type			       guifg=#1fb631  guibg=#000000  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi Statement	     guifg=#2fc626  guibg=#000000  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi Keyword                                                         ctermfg=034  ctermbg=000  cterm=none
hi PreProc	       guifg=#00ac5c  guibg=#000000  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi Number                                                          ctermfg=034  ctermbg=000  cterm=none
hi Special	       guifg=#00d700  guibg=#001200  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi Title		       guifg=#09ab00  guibg=#000000  gui=none          ctermfg=034  ctermbg=000  cterm=none

hi Todo			       guifg=#000000  guibg=#00ed00  gui=none          ctermfg=000  ctermbg=002  cterm=none

hi FoldColumn	     guifg=#00b900  guibg=#000300  gui=none          ctermfg=046  ctermbg=016  cterm=none
hi Folded		       guifg=#00bf00  guibg=#001200  gui=none          ctermfg=010  ctermbg=022  cterm=none

hi DiffAdd	       guifg=#00bf00  guibg=#002200  gui=none          ctermfg=034  ctermbg=000  cterm=none
hi DiffDelete	     guifg=#000000  guibg=#005500  gui=none          ctermfg=002  ctermbg=000  cterm=none
hi DiffText		     guifg=#00aa00  guibg=#004400  gui=underline     ctermfg=028  ctermbg=000  cterm=none
hi DiffChange	     guifg=#00a900  guibg=#002200  gui=none          ctermfg=040  ctermbg=000  cterm=none

hi CursorLine	     guifg=#000000  guibg=#00cc00  gui=none          ctermfg=000  ctermbg=002  cterm=none
hi CursorColumn    guifg=#000000  guibg=#00cc00  gui=none          ctermfg=000  ctermbg=002  cterm=none
hi CursorIM	       guifg=#00ff00  guibg=#000000  gui=bold          ctermfg=046  ctermbg=000  cterm=bold

hi MatchParen      guifg=#304300  guibg=#00fe00  gui=none          ctermfg=010  ctermbg=022  cterm=bold

hi Pmenu		       guifg=#00bf00  guibg=#000a00  gui=none          ctermfg=002  ctermbg=000  cterm=none
hi PmenuSbar	     guifg=#00dc00  guibg=#001c00  gui=none          ctermfg=034  ctermbg=022  cterm=none
hi PmenuSel	       guifg=#00f300  guibg=#002200  gui=none          ctermfg=010  ctermbg=022  cterm=none

hi TabLine		     guifg=#00f400  guibg=#000a00  gui=none          ctermfg=040  ctermbg=000  cterm=none
hi TabLineFill     guifg=#00ea00  guibg=#000000  gui=none          ctermfg=000  ctermbg=000  cterm=none
hi TabLineSel	     guifg=#00f000  guibg=#002a00  gui=none          ctermfg=046  ctermbg=022  cterm=bold

hi Directory	     guifg=#009330  guibg=#000000  gui=none          ctermfg=046  ctermbg=000  cterm=none

hi Error		       guifg=#000000  guibg=#00d000  gui=none          ctermfg=000  ctermbg=010  cterm=none
hi ErrorMsg		     guifg=#000000  guibg=#00ff00  gui=bold          ctermfg=000  ctermbg=010  cterm=none
hi IncSearch	                                   gui=reverse                                 cterm=reverse
hi LineNr	         guifg=#007900  guibg=#000600  gui=none          ctermfg=022  ctermbg=000  cterm=none
hi ModeMsg	       guifg=#00ea00  guibg=#000900  gui=none          ctermfg=002  ctermbg=000  cterm=none
hi MoreMsg	       guifg=#00e700  guibg=#001000  gui=bold          ctermfg=002  ctermbg=000  cterm=bold
hi NonText	       guifg=#008700  guibg=#001000  gui=none          ctermfg=022  ctermbg=000  cterm=none
hi Question	       guifg=#009f00  guibg=#000000  gui=none          ctermfg=040  ctermbg=000  cterm=none
hi Search		                                     gui=reverse                                 cterm=reverse
hi SpecialKey	     guifg=#008000  guibg=#002300  gui=bold          ctermfg=034  ctermbg=000  cterm=bold
hi StatusLine	     guifg=#00ff00  guibg=#001000  gui=none          ctermfg=040  ctermbg=234  cterm=bold
hi StatusLineNC    guifg=#005500  guibg=#001000  gui=none          ctermfg=002  ctermbg=234  cterm=none
hi Underlined	     guifg=#00b400  guibg=#000000  gui=underline     ctermfg=002  ctermbg=000  cterm=underline
hi VertSplit	     guifg=#005100  guibg=#005100  gui=none          ctermfg=022  ctermbg=000  cterm=none
hi VisualNOS	     guifg=#005100  guibg=#005100  gui=reverse       ctermfg=022  ctermbg=000  cterm=reverse
hi WarningMsg	     guifg=#000000  guibg=#00ff00  gui=none          ctermfg=010  ctermbg=000  cterm=none
hi WildMenu		     guifg=#00cb00  guibg=#000000  gui=reverse       ctermfg=000  ctermbg=010  cterm=reverse
