" after/plugin/sexp.vim - Sexp mappings for regular people
" Maintainer:   Tim Pope <code@tpope.net>
"
" Copied from https://github.com/tpope/vim-sexp-mappings-for-regular-people/blob/cc5923e357373ea6ef0c13eae82f44e6b9b1d374/plugin/sexp_mappings_for_regular_people.vim
" and then customized with my mappings.

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
  exe s:map_sexp_wrap('e', '<C-w>"', '"', '"', 0)
  exe s:map_sexp_wrap('e', '<C-w>(', '(', ')', 0)
  exe s:map_sexp_wrap('e', '<C-w>[', '[', ']', 0)
  exe s:map_sexp_wrap('e', '<C-w>{', '{', '}', 0)

  exe s:map('n', 'dsf', '<Plug>(sexp_splice_list)')

  if !get(g:, 'sexp_no_word_maps')
    exe s:map('n', 'B', '<Plug>(sexp_move_to_prev_element_head)')
    exe s:map('n', 'W', '<Plug>(sexp_move_to_next_element_head)')
    exe s:map('n', 'gE', '<Plug>(sexp_move_to_prev_element_tail)')
    exe s:map('n', 'E', '<Plug>(sexp_move_to_next_element_tail)')
    exe s:map('x', 'B', '<Plug>(sexp_move_to_prev_element_head)')
    exe s:map('x', 'W', '<Plug>(sexp_move_to_next_element_head)')
    exe s:map('x', 'gE', '<Plug>(sexp_move_to_prev_element_tail)')
    exe s:map('x', 'E', '<Plug>(sexp_move_to_next_element_tail)')
    exe s:map('o', 'B', '<Plug>(sexp_move_to_prev_element_head)')
    exe s:map('o', 'W', '<Plug>(sexp_move_to_next_element_head)')
    exe s:map('o', 'gE', '<Plug>(sexp_move_to_prev_element_tail)')
    exe s:map('o', 'E', '<Plug>(sexp_move_to_next_element_tail)')
  endif

  " Bind < with <lt>
  " Source https://www.reddit.com/r/vim/comments/8liqaz/comment/dzftwxr/
  exe s:map('n', '<lt>I', '<Plug>(sexp_insert_at_list_head)')
  exe s:map('n', '>I', '<Plug>(sexp_insert_at_list_tail)')
  exe s:map('n', '<lt>f', '<Plug>(sexp_swap_list_backward)')
  exe s:map('n', '>f', '<Plug>(sexp_swap_list_forward)')
  exe s:map('n', '<lt>e', '<Plug>(sexp_swap_element_backward)')
  exe s:map('n', '>e', '<Plug>(sexp_swap_element_forward)')
  exe s:map('n', '>(', '<Plug>(sexp_emit_head_element)')
  exe s:map('n', '<lt>)', '<Plug>(sexp_emit_tail_element)')
  exe s:map('n', '<lt>(', '<Plug>(sexp_capture_prev_element)')
  exe s:map('n', '>)', '<Plug>(sexp_capture_next_element)')

  exe s:map('n', '<M-h>', '<Plug>(sexp_move_to_prev_element_head)')
  exe s:map('n', '<M-l>', '<Plug>(sexp_move_to_next_element_head)')
  exe s:map('n', '<M-u>', '<Plug>(sexp_move_to_prev_bracket)')
  exe s:map('n', '<M-o>', '<Plug>(sexp_move_to_next_bracket)')
  exe s:map('n', '<M-n>', '<Plug>(sexp_flow_to_prev_leaf_head)')
  exe s:map('n', '<M-.>', '<Plug>(sexp_flow_to_next_leaf_head)')
  exe s:map('n', '<M-H>', '<Plug>(sexp_swap_element_backward)')
  exe s:map('n', '<M-L>', '<Plug>(sexp_swap_element_forward)')
  exe s:map('n', '<M-m>', '<Plug>(sexp_capture_prev_element)')
  exe s:map('n', '<M-,>', '<Plug>(sexp_capture_next_element)')
  exe s:map('n', '<M-M>', '<Plug>(sexp_emit_head_element)')
  " Bind < with <lt> or, with meta, <M-lt>
  " Bind > with meta as <M-char-62>
  " Source https://www.reddit.com/r/vim/comments/8liqaz/comment/dzgi1ma/
  exe s:map('n', '<M-lt>', '<Plug>(sexp_emit_tail_element)')
  exe s:map('n', '<M-r>', '<Plug>(sexp_raise_element)')
  exe s:map('n', '<M-s>', '<Plug>(sexp_splice_list)')
  exe s:map('n', '<M-c>', '<Plug>(sexp_convolute)')
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

