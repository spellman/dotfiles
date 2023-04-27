require("cort.settings")
require("cort.key_mappings")

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
