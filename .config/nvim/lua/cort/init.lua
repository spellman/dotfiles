-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Setting leaders must happen before plugins are required (otherwise
-- the wrong leaders will be used).
vim.g.mapleader = " "
vim.g.maplocalleader = " "


-- Conjure maps keys based on the values of various global variables.
-- Since those global variables have `#` characters and `#` is the length
-- operator in Lua, we use the table style with a string key to set the value.
-- Source: https://github.com/nanotee/nvim-lua-guide/discussions/66#discussioncomment-855208
-- Commented: https://github.com/Olical/conjure/issues/186#issuecomment-1728700445
vim.g["conjure#mapping#def_word"] = false

-- Neovide is a nice idea but my keymapping of <M-u> was being recognized by
-- nvim as an umlaut, which is what MacOS generates for <M-u>.
-- Neovide has a MacOS-specific setting to make alt/option behave as meta so
-- that meta+u would be sent but the setting had no effect set to true or false
-- or being omitted.
if vim.g.neovide then
  vim.g.neovide_hide_mouse_when_typing = true
  vim.g.neovide_remember_window_size = true
  vim.g.neovide_input_macos_alt_is_meta = true
  vim.g.neovide_cursor_animation_length = 0
  vim.g.neovide_cursor_trail_size = 0
  vim.g.neovide_cursor_animate_in_insert_mode = false
  vim.g.neovide_cursor_animate_command_line = false
  vim.opt.guifont = "Monaco,JetBrainsMono:h13"
  vim.opt.linespace = 1.0
  vim.g.neovide_input_ime = false

  -- As per https://neovide.dev/faq.html#how-can-i-use-cmd-ccmd-v-to-copy-and-paste
  -- Enable cmd-c for copy and cmd-v for paste:
  vim.g.neovide_input_use_logo = true
  vim.keymap.set('n', '<D-s>', ':w<CR>') -- Save
  vim.keymap.set('v', '<D-c>', '"+y') -- Copy
  vim.keymap.set('n', '<D-v>', '"+P') -- Paste normal mode
  vim.keymap.set('v', '<D-v>', '"+P') -- Paste visual mode
  vim.keymap.set('c', '<D-v>', '<C-R>+') -- Paste command mode
  vim.keymap.set('i', '<D-v>', '<ESC>l"+Pli') -- Paste insert mode
  vim.api.nvim_set_keymap('', '<D-v>', '+p<CR>', { noremap = true, silent = true})
  vim.api.nvim_set_keymap('!', '<D-v>', '<C-R>+', { noremap = true, silent = true})
  vim.api.nvim_set_keymap('t', '<D-v>', '<C-R>+', { noremap = true, silent = true})
  vim.api.nvim_set_keymap('v', '<D-v>', '<C-R>+', { noremap = true, silent = true})
end

require("cort.settings")

-- Install package manager
--    https://github.com/folke/lazy.nvim
--    `:help lazy.nvim.txt` for more info
local lazypath = vim.fn.stdpath "data" .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- We provide a string with the name of the Lua module containinging the plugin spec. This tells lazy.nvim which packages to install.
require("lazy").setup("cort.plugins", {})

require("cort.key_mappings")

-- Packages are set up when the Lua code in ~/.config/nvim/after runs, which is, by default, the last entry in the nvim runtime path, which means it will run at the end of startup.
-- See
-- * Help on runtimepath or rtp
-- * https://www.youtube.com/watch?v=w7i4amO_zaE
-- https://github.com/ThePrimeagen/init.lua/commit/97c039bb88d8bbbcc9b1e3d0dc716a2ba202c6d2

-- [[ Highlight on yank ]]
-- See `:help vim.highlight.on_yank()`
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank()
  end,
  group = highlight_group,
  pattern = "*",
})

-- We set the quickfix list to appear as a full-width window, across the bottom
-- of the current tab.
-- Note that we only apply this to the quickfix list and not to location lists,
-- which also use the `qf` FileType.
-- The following lua is a translation of this vimscript:
-- ```vimscript
-- autocmd FileType qf if (getwininfo(win_getid())[0].loclist != 1) | wincmd J | endif
-- ```
-- Which is to say,
--     When the FileType event is fired because the FileType is set,
--     then if the buffer file type is "qf",
--     then make the buffer window full-width across the bottom of the tab.
vim.api.nvim_create_autocmd("FileType", {
  callback = function()
    local current_window_id = vim.fn.win_getid()
    local current_window_info = vim.fn.getwininfo(current_window_id)[1]
    -- Window info for the quickfix list includes {
    --   quickfix = 1,
    --   loclist = 0,
    -- }
    -- Window info for the location list includes {
    --   quickfix = 1,
    --   loclist = 1,
    -- }
    if current_window_info["loclist"] ~= 1 then
      vim.cmd("wincmd J")
    end
  end,
  pattern = "qf",
})
