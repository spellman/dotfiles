-- TODO:
-- * Neogit is good enough to use but it displays incorrect information much too
--   frequently. Try out other git workflows.
--   E.g., https://www.youtube.com/watch?v=IyBAuDPzdFY

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
