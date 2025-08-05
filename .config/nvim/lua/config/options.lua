-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- NOTE: Inspect values with :lua print(vim.inspect(vim.opt.<value name>:get()))
--       Ex: :lua print(vim.inspect(vim.opt.undodir:get()))

vim.g.trouble_lualine = false
vim.g.snacks_animate = false

vim.opt.spell = false -- Disable spell checking.

-- Line Wrapping
vim.opt.wrap = true -- Wrap lines by default
vim.opt.breakindent = true -- Indent wrapped line from parent line

vim.opt.relativenumber = false

-- Status Line
vim.opt.laststatus = 2 -- Always show the status line and show a status line for each buffer.

vim.opt.swapfile = false -- Disable swap files.

-- Update time
-- The value of updatetime (in milliseconds) is the delay after your last
-- keypress before Neovim fires certain CursorHold-related events or performs
-- auto-save-like checks.
vim.opt.updatetime = 100 -- Decrease update time.

vim.opt.list = false -- Don't show representations of whitespace.

-- Conjure maps keys based on the values of various global variables.
-- Since those global variables have `#` characters and `#` is the length
-- operator in Lua, we use the table style with a string key to set the value.
-- Source: https://github.com/nanotee/nvim-lua-guide/discussions/66#discussioncomment-855208
-- Commented: https://github.com/Olical/conjure/issues/186#issuecomment-1728700445
vim.g["conjure#mapping#def_word"] = false
