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
vim.opt.updatetime = 50
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
