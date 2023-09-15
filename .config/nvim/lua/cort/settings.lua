-- See `:help vim.o`

-- Set highlight on search.
vim.opt.hlsearch = true

-- Highlight matches-so-far while typing search text.
vim.opt.incsearch = true

-- Make line numbers default.
vim.opt.number = true

-- Highlight the line on which the cursor is located.
vim.opt.cursorline = true

-- Column to highlight.
vim.opt.colorcolumn = "81"

-- Enable mouse mode.
vim.opt.mouse = "a"

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.opt.clipboard = "unnamedplus"

-- Enable break indent.
vim.opt.breakindent = true

-- Tabs
vim.opt.tabstop = 4
vim.opt.softtabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

-- Disable swap files.
vim.opt.swapfile = false

-- Disable creating backup files when overwriting a file.
vim.opt.backup = false

-- Save undo history.
vim.opt.undodir = os.getenv("HOME") .. "/.local/share/nvim/cort_undodir"
vim.opt.undofile = true

-- Case insensitive searching UNLESS /C or capital in search.
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default.
vim.opt.signcolumn = "yes"

-- Decrease update time.
vim.opt.updatetime = 100
vim.opt.timeout = true
vim.opt.timeoutlen = 300

-- Set completeopt to have a better completion experience.
vim.opt.completeopt = "menuone,noselect"

-- NOTE: You should make sure your terminal supports this
vim.opt.termguicolors = true

-- Lightbackground for doom-one theme.
vim.opt.background = "light"

-- Session
vim.opt.sessionoptions = "blank,curdir,folds,help,tabpages,winsize"

-- Open horizontal splits below.
vim.opt.splitbelow = true

-- Open vertical splits to the right.
vim.opt.splitright = true

-- listchars determines the printing of whitespace characters
-- vim.opt.list = true

-- Settings for https://github.com/kevinhwang91/nvim-ufo, as per
-- https://github.com/kevinhwang91/nvim-ufo/issues/4#issuecomment-1512772530
vim.o.fillchars = [[eob: ,fold: ,foldopen:,foldsep: ,foldclose:]]
-- https://github.com/kevinhwang91/nvim-ufo/blob/43e39ec74cd57c45ca9d8229a796750f6083b850/README.md
vim.o.foldcolumn = "1" -- "0" is also an alright setting.
vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
vim.o.foldlevelstart = 99
vim.o.foldenable = true
vim.o.foldmethod = "expr"
vim.o.foldexpr = "nvim_treesitter#foldexpr()"

-- From nvim-metals documentation:
-- Remove `F` from `shortmess`. `set shortmess-=F`
-- (for lua `vim.opt_global.shortmess:remove("F")`)
-- Without doing this, autocommands that deal with filetypes prohibit messages
-- from being shown, so some of the messages we show to help users get started
-- may not be shown. If you're confident you don't need help setting up, then
-- just remove this, and `nvim-metals` will work fine, but any log messages
-- won't actually be shown to you if something goes wrong with `nvim-metals`.
vim.opt_global.shortmess:remove("F")
vim.opt_global.shortmess:remove("S")

vim.opt.textwidth = 80
vim.opt.autoindent = true
vim.opt.formatoptions = "cjnoqr1/"
vim.opt.wrap = true

-- Allow navigating away from unsaved file.
-- https://vimtricks.com/p/what-is-set-hidden/
vim.opt.hidden = true
