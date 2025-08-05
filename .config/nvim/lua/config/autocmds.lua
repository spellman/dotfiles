-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with
-- `lazyvim_` for the defaults).
-- E.g., vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- Remove LazyVim's default spell checking, which applies to
-- "text", "plaintex", "typst", "gitcommit", and "markdown" filetypes.
vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- Check spelling in gitcommit files; essentially, change the above autocmd to
-- only apply to gitcommit files.
vim.api.nvim_create_autocmd("FileType", {
  pattern = { "gitcommit" },
  callback = function()
    vim.opt_local.wrap = true
    vim.opt_local.spell = true
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "text",
  callback = function()
    vim.b.completion = false
    vim.b.snacks_indent = false

    -- Remove formatoptions that cause auto-indentation of new lines after
    -- bullet point lines that begin with `*`.
    vim.bo.formatoptions = vim.bo.formatoptions:gsub("[oO]", "")
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "clojure",
  callback = function()
    vim.b.snacks_indent = false
  end,
})
