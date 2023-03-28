" Set the mapleader.
" let mapleader="\"
map <Space> <Leader>
map , <Localleader>

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

if exists("g:neovide")
    " Put anything you want to happen only in Neovide here
    let g:neovide_scroll_animation_length = 0
    let g:neovide_cursor_animation_length = 0
    let g:neovide_cursor_trail_size = 0
    let g:neovide_hide_mouse_when_typing = v:true
endif

if exists('g:vscode')
    " VSCode
endif

