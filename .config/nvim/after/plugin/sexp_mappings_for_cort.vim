" after/plugin/sexp.vim - Sexp mappings for regular people
" Copied from https://github.com/tpope/vim-sexp-mappings-for-regular-people/blob/cc5923e357373ea6ef0c13eae82f44e6b9b1d374/plugin/sexp_mappings_for_regular_people.vim
" and then customized with my mappings.

" NOTE: In VimL, we bind `<` with `<lt>`.
" We bind < with meta as `<M-lt>`.
" We bind > with meta as `<M-char-62>`
" Sources
" * https://www.reddit.com/r/vim/comments/8liqaz/comment/dzftwxr/
" * https://www.reddit.com/r/vim/comments/8liqaz/comment/dzgi1ma/

" NOTE: Different terminals interpret keys modified with option/meta/alt
" differently. With Option + Shift + 9, for example,
" * Kitty sees Option + Shift + 9
" * iterm2, Alacritty, and Wezterm see Option + (
" And these interpretations are different :O
" Therefore, we map the former represetation to the cmd and then map the
" latter to the former.

if exists("g:loaded_sexp_mappings_for_regular_people") || &cp
  finish
endif
let g:loaded_sexp_mappings_for_regular_people = 1

function! s:map(mode, lhs, rhs) abort
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') . '|sil! ' . a:mode . 'unmap <buffer> ' . a:lhs
  return a:mode . 'map <buffer> ' . a:lhs . ' ' . a:rhs
endfunction

function! s:map_sexp_wrap(type, target, left, right, pos) abort
  let mode = (a:type ==# 'v' ? 'x' : 'n')
  let b:undo_ftplugin = get(b:, 'undo_ftplugin', 'exe') . '|sil! ' . mode . 'unmap <buffer> ' . a:target
  return mode.'noremap '
        \ . '<buffer><silent> ' . a:target . ' :<C-U>let b:sexp_count = v:count<Bar>exe "normal! m`"<Bar>'
        \ . 'call sexp#wrap("'.a:type.'", "'.a:left.'", "'.a:right.'", '.a:pos.', 0)'
        \ . '<Bar>silent! call repeat#set("'.a:target.'", v:count)<CR>'
endfunction

function! s:sexp_mappings() abort
  if !exists('g:sexp_loaded')
    return
  endif

  " Movement
  exe s:map('n', '<M-u>', '<Plug>(sexp_move_to_prev_bracket)')
  exe s:map('n', '<M-o>', '<Plug>(sexp_move_to_next_bracket)')
  exe s:map('n', '<M-h>', '<Plug>(sexp_move_to_prev_element_head)')
  exe s:map('n', '<M-l>', '<Plug>(sexp_move_to_next_element_head)')
  exe s:map('n', '<M-n>', '<Plug>(sexp_flow_to_prev_leaf_head)')
  exe s:map('n', '<M-.>', '<Plug>(sexp_flow_to_next_leaf_head)')
  

  " Insertion
  exe s:map('n', '<M-i>', '<Plug>(sexp_insert_at_list_head)')
  exe s:map('n', '<M-S-i>', '<Plug>(sexp_insert_at_list_tail)')
  nmap <buffer> <M-I> <M-S-i>
  exe s:map_sexp_wrap('e', '<M-S-9>', '(', ')', 0)
  nmap <buffer> <M-(> <M-S-9>
  exe s:map_sexp_wrap('e', '<M-[>', '[', ']', 0)
  exe s:map_sexp_wrap('e', '<M-S-[>', '{', '}', 0)
  nmap <buffer> <M-{> <M-S-[>
  " TODO: Is wrap-with-angle-brackets available?
  " NOTE: Wrap with double quotes is more general than s-expressions (plus the
  " version that used that functionality from the vim-sexp plugin wasn't
  " working) so that wrapping is defined in nvim/lua/cort/key_mappings.lua in
  " terms of basic Vim commands.

  " S-Expression Manipulation
  exe s:map('n', '<M-q>', '<Plug>(sexp_capture_prev_element)')
  exe s:map('n', '<M-e>', '<Plug>(sexp_capture_next_element)')
  exe s:map('n', '<M-s>', '<Plug>(sexp_splice_list)')
  " TODO: <M-a> splice-killing-backward
  " TODO: <M-d> splice-killing-forward
  exe s:map('n', '<M-z>', '<Plug>(sexp_emit_head_element)')
  exe s:map('n', '<M-c>', '<Plug>(sexp_emit_tail_element)')
  exe s:map('n', '<M-S-j>', '<Plug>(sexp_swap_element_backward)')
  nmap <buffer> <M-J> <M-S-j>
  exe s:map('n', '<M-S-k>', '<Plug>(sexp_swap_element_forward)')
  nmap <buffer> <M-K> <M-S-k>
  " TODO: <M-j> join
  " TODO: <M-w> split
  exe s:map('n', '<M-r>', '<Plug>(sexp_raise_element)')
  exe s:map('n', '<M-S-/>', '<Plug>(sexp_convolute)')
  nmap <buffer> <M-?> <M-S-/>
endfunction

function! s:setup() abort
  augroup sexp_mappings_for_regular_people
    autocmd!
    execute 'autocmd FileType' get(g:, 'sexp_filetypes', 'lisp,scheme,clojure') 'call s:sexp_mappings()'
  augroup END
endfunction

if has('vim_starting') && !exists('g:sexp_loaded')
  au VimEnter * call s:setup()
else
  call s:setup()
endif
