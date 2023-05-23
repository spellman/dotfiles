-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are required (otherwise wrong leader will be used)
vim.g.mapleader = " "
vim.g.maplocalleader = " "

-- local sections = {
--   l = { desc = " LSP" },
--   b = { desc = "󰓩 Buffers" },
--   d = { desc = " Debugger" },
--   g = { desc = "󰊢 Git" },
--   S = { desc = "󱂬 Session" },
--   t = { desc = " Terminal" },
--   u = { desc = " UI" },
-- }

-- Keymaps for better default experience
-- See `:help vim.keymap.set()`
vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

-- Remap for dealing with word wrap
vim.keymap.set({"n", "v"}, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set({"n", "v"}, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Window movement
vim.keymap.set("n", "<C-h>", "<C-w>h", { desc = "Switch to split to left", noremap = true })
vim.keymap.set("n", "<C-j>", "<C-w>j", { desc = "Switch to split below", noremap = true })
vim.keymap.set("n", "<C-k>", "<C-w>k", { desc = "Switch to split above", noremap = true })
vim.keymap.set("n", "<C-l>", "<C-w>l", { desc = "Switch to split to right", noremap = true })

-- Move visually selected lines up and down.
-- TODO: Why is this flaky?
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '>-2<CR>gv=gv")

-- Keep cursor fixed when appending next line to current line.
vim.keymap.set("n", "J", "mzJ`z")

-- Center cursor vertically in split when jumping forward or backward by half a
-- page.
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Clear search highlight.
vim.keymap.set("n", "<leader>/", vim.cmd.nohlsearch, { desc = "clear search highlight" })

-- Center cursor vertically in split when going to next or previous search term.
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

-- Paste without overwriting default register contents. So you can paste the
-- same thing again.
vim.keymap.set("x", "<S-M-p>", "\"_dP")

-- Q goes to ex mode, which isn't supposed to be useful interactively.
-- We disable it.
vim.keymap.set("n", "Q", "<nop>")

-- Location list naviation
local toggle_location_list = function()
  local winid = vim.fn.getloclist(0, { winid = 0 }).winid

  -- If winid == 0, then the window has no open location list.
  if winid == 0 then
    vim.cmd("lopen")
  else
    vim.cmd("lclose")
  end
end
vim.keymap.set({"n", "i"}, "<D-l>", toggle_location_list)
vim.keymap.set({"n", "i"}, "<D-j>", "<cmd>lnext<CR>zz")
vim.keymap.set({"n", "i"}, "<D-k>", "<cmd>lprev<CR>zz")

-- Quickfix list navigation
local toggle_quickfix_list = function()
  local does_quickfix_list_exist = false
  for _, win in pairs(vim.fn.getwininfo()) do
    if win["quickfix"] == 1 then
      does_quickfix_list_exist = true
    end
  end
  if does_quickfix_list_exist == true then
    vim.cmd("cclose")
  else
    vim.cmd("copen")
  end
end
vim.keymap.set({"n", "i"}, "<S-D-l>", toggle_quickfix_list)
vim.keymap.set({"n", "i"}, "<S-D-j>", "<cmd>cnext<CR>zz")
vim.keymap.set({"n", "i"}, "<S-D-k>", "<cmd>cprev<CR>zz")

-- Replace occurrences of word under cursor in buffer.
vim.keymap.set("n", "<leader>rw", [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]])
