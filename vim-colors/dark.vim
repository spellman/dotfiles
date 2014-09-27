" Vim color file
" Name: dark.vim

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

let g:colors_name = 'dark'

hi Cursor		       ctermfg=153  ctermbg=235  cterm=bold,reverse
hi Normal		       ctermfg=153  ctermbg=235  cterm=none
hi Visual                       ctermbg=235  cterm=reverse

hi Comment	       ctermfg=103  ctermbg=235  cterm=none

hi Constant	       ctermfg=153  ctermbg=235  cterm=none
hi String          ctermfg=153  ctermbg=235  cterm=none
hi Identifier      ctermfg=153  ctermbg=235  cterm=none
hi Function        ctermfg=153  ctermbg=235  cterm=none
hi Type			       ctermfg=153  ctermbg=235  cterm=none
hi Statement	     ctermfg=153  ctermbg=235  cterm=none
hi Keyword         ctermfg=153  ctermbg=235  cterm=none
hi PreProc	       ctermfg=153  ctermbg=235  cterm=none
hi Number          ctermfg=153  ctermbg=235  cterm=none
hi Special	       ctermfg=153  ctermbg=235  cterm=none
hi Title		       ctermfg=153  ctermbg=235  cterm=none

hi Todo			       ctermfg=153  ctermbg=060  cterm=none

hi FoldColumn	     ctermfg=189  ctermbg=000  cterm=none
hi Folded		       ctermfg=189  ctermbg=060  cterm=none

hi DiffAdd	       ctermfg=034  ctermbg=235  cterm=none
hi DiffDelete	     ctermfg=009  ctermbg=235  cterm=none
hi DiffText		     ctermfg=153  ctermbg=235  cterm=none
hi DiffChange	     ctermfg=172  ctermbg=235  cterm=none

hi CursorLine	     ctermfg=235  ctermbg=060  cterm=none
hi CursorColumn    ctermfg=235  ctermbg=060  cterm=none
hi CursorIM	       ctermfg=195  ctermbg=235  cterm=bold

hi MatchParen      ctermfg=159  ctermbg=060  cterm=bold

hi Pmenu		       ctermfg=153  ctermbg=239  cterm=none
hi PmenuSbar	     ctermfg=153  ctermbg=060  cterm=none
hi PmenuSel	       ctermfg=189  ctermbg=060  cterm=none

hi TabLine		     ctermfg=103  ctermbg=233  cterm=none
hi TabLineFill     ctermfg=235  ctermbg=233  cterm=none
hi TabLineSel	     ctermfg=153  ctermbg=233  cterm=bold

hi Directory	     ctermfg=111  ctermbg=235  cterm=none

hi Error		       ctermfg=235  ctermbg=189  cterm=none
hi ErrorMsg		     ctermfg=235  ctermbg=189  cterm=none
hi IncSearch	                               cterm=reverse
hi LineNr	         ctermfg=060  ctermbg=234  cterm=none
hi ModeMsg	       ctermfg=060  ctermbg=235  cterm=none
hi MoreMsg	       ctermfg=060  ctermbg=235  cterm=bold
hi NonText	       ctermfg=060  ctermbg=235  cterm=none
hi Question	       ctermfg=103  ctermbg=235  cterm=none
hi Search		                                 cterm=reverse
hi SpecialKey	     ctermfg=153  ctermbg=235  cterm=bold
hi StatusLine	     ctermfg=103  ctermbg=233  cterm=bold
hi StatusLineNC    ctermfg=060  ctermbg=233  cterm=none
hi Underlined	     ctermfg=061  ctermbg=235  cterm=underline
hi VertSplit	     ctermfg=060  ctermbg=233  cterm=none
hi VisualNOS	     ctermfg=060  ctermbg=235  cterm=reverse
hi WarningMsg	     ctermfg=189  ctermbg=235  cterm=none
hi WildMenu		     ctermfg=235  ctermbg=189  cterm=reverse
