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

hi Cursor		       guifg=#DADADA  guibg=#303030  gui=bold,reverse
hi Normal		       guifg=#DADADA  guibg=#303030  gui=none
hi Visual          guifg=#DADADA  guibg=#5F5F5F  gui=reverse

hi Comment	       guifg=#8A8A8A  guibg=#303030  gui=none

hi Constant	       guifg=#DADADA  guibg=#303030  gui=none
hi String          guifg=#DADADA  guibg=#303030  gui=none
hi Identifier      guifg=#DADADA  guibg=#303030  gui=none
hi Function        guifg=#DADADA  guibg=#303030  gui=none
hi Type			       guifg=#DADADA  guibg=#303030  gui=none
hi Statement	     guifg=#DADADA  guibg=#303030  gui=none
hi Keyword         guifg=#DADADA  guibg=#303030  gui=none
hi PreProc	       guifg=#DADADA  guibg=#303030  gui=none
hi Number          guifg=#DADADA  guibg=#303030  gui=none
hi Special	       guifg=#DADADA  guibg=#303030  gui=none
hi Title		       guifg=#DADADA  guibg=#303030  gui=none

hi Todo			       guifg=#DADADA  guibg=#767676  gui=none

hi FoldColumn	     guifg=#DADADA  guibg=#262626  gui=none
hi Folded		       guifg=#DADADA  guibg=#767676  gui=none

hi DiffAdd	       guifg=#8FB28F  guibg=#262626  gui=none
hi DiffDelete	     guifg=#DCA3A3  guibg=#262626  gui=none
hi DiffText		     guifg=#DADADA  guibg=#303030  gui=none
hi DiffChange	     guifg=#6CA0A3  guibg=#262626  gui=none

hi CursorLine	     guifg=#303030  guibg=#767676  gui=none
hi CursorColumn    guifg=#303030  guibg=#767676  gui=none
hi CursorIM	       guifg=#DADADA  guibg=#303030  gui=bold

hi MatchParen      guifg=#DADADA  guibg=#767676  gui=bold

hi Pmenu		       guifg=#DADADA  guibg=#262626  gui=none
hi PmenuSbar	     guifg=#DADADA  guibg=#767676  gui=none
hi PmenuSel	       guifg=#DADADA  guibg=#767676  gui=none

hi TabLine		     guifg=#8A8A8A  guibg=#262626  gui=none
hi TabLineFill     guifg=#303030  guibg=#262626  gui=none
hi TabLineSel	     guifg=#DADADA  guibg=#262626  gui=bold

hi Directory	     guifg=#DADADA  guibg=#303030  gui=none

hi Error		       guifg=#303030  guibg=#DADADA  gui=none
hi ErrorMsg		     guifg=#303030  guibg=#DADADA  gui=none
hi IncSearch	                               gui=reverse
hi LineNr	         guifg=#767676  guibg=#262626  gui=none
hi ModeMsg	       guifg=#767676  guibg=#303030  gui=none
hi MoreMsg	       guifg=#767676  guibg=#303030  gui=bold
hi NonText	       guifg=#767676  guibg=#303030  gui=none
hi Question	       guifg=#8A8A8A  guibg=#303030  gui=none
hi Search		                                 gui=reverse
hi SpecialKey	     guifg=#DADADA  guibg=#303030  gui=bold
hi StatusLine	     guifg=#8A8A8A  guibg=#262626  gui=bold
hi StatusLineNC    guifg=#767676  guibg=#262626  gui=none
hi Underlined	     guifg=#767676  guibg=#303030  gui=underline
hi VertSplit	     guifg=#767676  guibg=#262626  gui=none
hi VisualNOS	     guifg=#767676  guibg=#303030  gui=reverse
hi WarningMsg	     guifg=#DADADA  guibg=#303030  gui=none
hi WildMenu		     guifg=#303030  guibg=#DADADA  gui=reverse
