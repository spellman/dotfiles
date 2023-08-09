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
vim.keymap.set({ "n", "v" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set({ "n", "v" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

local recovery_direction = {
  --
  h = "l",
  j = "l",
  k = "l",
  l = "h",
}

---@param direction string Direction in which to try to move: "h", "j", "k", or "l"
local function move_to_window_while_skipping_aerial_windows(direction)
  -- TODO: This works but it's really dumb. Also, my at-the-edge heuristic may
  -- not always hold. In fact, it is at least dependent on the aerial window
  -- being displayed to the left of the associated window.
  -- Can we instead get the window layout, determine the window to which to
  -- move, and then actually move the cursor?

  -- Move the cursor in the given direction to the adjacent window.
  vim.cmd("wincmd " .. direction)

  if vim.bo.filetype == "aerial" then
    -- If the new current window is an aerial window, then move the cursor again
    -- in the given direction to the adjacent window.
    vim.cmd("wincmd " .. direction)

    -- If we're still on an aerial window, infer that we must be at the edge of the tab.
    -- In that case, we should have never moved at all. Therefore, move the cursor
    -- one window in the recovery direction, restoring the cursor to the original window.
    if vim.bo.filetype == "aerial" then
      vim.cmd("wincmd " .. recovery_direction[direction])
    end
  end
end

-- Window movement
-- Skip over aerial windows when moving left.
vim.keymap.set("n", "<C-h>", function()
  move_to_window_while_skipping_aerial_windows("h")
end, { desc = "Move cursor to window to left" })

vim.keymap.set("n", "<C-j>", function()
  move_to_window_while_skipping_aerial_windows("j")
end , { desc = "Move cursor to window below" })

vim.keymap.set("n", "<C-k>", function()
  move_to_window_while_skipping_aerial_windows("k")
end, { desc = "Move cursor to window above" })

vim.keymap.set("n", "<C-l>", function()
  move_to_window_while_skipping_aerial_windows("l")
end, { desc = "Move cursor to window to right" })

-- Move visually selected lines up and down.
-- TODO: Why is this flaky?
vim.keymap.set("v", "J", ":m '>+1<CR>gv=gv")
vim.keymap.set("v", "K", ":m '>-2<CR>gv=gv")

local function current_character_and_neighbors(line_number, column_number)
  local line = vim.fn.getline(line_number)

  return {
    previous=line:sub(column_number - 1, column_number - 1),
    current=line:sub(column_number, column_number),
    next=line:sub(column_number + 1, column_number + 1)
  }
end

local opening_delimiters = {
  ["("] = true,
  ["["] = true,
  ["{"] = true,
}
local closing_delimiters = {
  [")"] = true,
  ["]"] = true,
  ["}"] = true,
}

--- The intent is to tidy up line joins to compensate for the default nvim
--- behavior leaving stray spaces.
---
--- Snug up text to opening delimiters:
--- [
---  :a]
---        ----> [:a]
---
--- Snug up opening delimiters to opening delimiters:
--- [
---  [:a]]
---        ----> [[:a]]
---
--- Snug up closing delimiters to closing delimiters:
--- [[:a]
---  ]
---        ----> [[:a]]
---
--- Otherwise, do nothing.
local function snug_up_text_and_delimiters_to_delimiters()
  -- position:[bufnum, lnum, col, off]
  local position = vim.fn.getpos(".")
  local line_number = position[2]
  local column_number = position[3]
  local characters = current_character_and_neighbors(line_number, column_number)

  if characters.current ~= " " then
    return
  end

  if opening_delimiters[characters.previous] or closing_delimiters[characters.next] then
    vim.cmd("normal! x")
  end
end

-- Keep cursor fixed when appending next line to current line.
vim.keymap.set("n", "J", "mzJ`z")
vim.keymap.set("n", "J", function()
  -- Add mark at current cursor position and join next line to current.line.
  -- This puts the cursor at the end of the current line, possibly with a space
  -- appended. (Default nvim behavior.)
  vim.cmd("normal! mzJ")

  snug_up_text_and_delimiters_to_delimiters()

  -- Move cursor to mark, which was the cursor position at the start of this
  -- operation.
  vim.cmd("normal! `z")
end)

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
vim.keymap.set({ "n", "i" }, "<D-l>", toggle_location_list)
vim.keymap.set({ "n", "i" }, "<D-j>", "<cmd>lnext<CR>zz")
vim.keymap.set({ "n", "i" }, "<D-k>", "<cmd>lprev<CR>zz")

-- Quickfix list navigation
local toggle_quickfix_list = function()
  local does_quickfix_list_exist = false
  for _, win in pairs(vim.fn.getwininfo()) do
    -- Window info for the quickfix list includes {
    --   quickfix = 1,
    --   loclist = 0,
    -- }
    -- Window info for the location list includes {
    --   quickfix = 1,
    --   loclist = 1,
    -- }
    if win["quickfix"] == 1 and win["loclist"] == 0 then
      does_quickfix_list_exist = true
    end
  end
  if does_quickfix_list_exist == true then
    vim.cmd("cclose")
  else
    vim.cmd("copen")
  end
end
vim.keymap.set({ "n", "i" }, "<S-D-l>", toggle_quickfix_list)
vim.keymap.set({ "n", "i" }, "<S-D-j>", "<cmd>cnext<CR>zz")
vim.keymap.set({ "n", "i" }, "<S-D-k>", "<cmd>cprev<CR>zz")

-- Replace occurrences of word under cursor in buffer.
require("which-key").register(
  {
    r = {
      w = { [[:%s/\<<C-r><C-w>\>/<C-r><C-w>/gI<Left><Left><Left>]], "Rename word under cursor in buffer" }
    }
  },
  {
    mode = "n",
    prefix = "<leader>",
  }
)

vim.keymap.set("n", '<C-w>"', 'ciw""<ESC>P', { desc = 'Wrap word under cursor in "' })


local function window_info()
  local current_window_id = vim.fn.win_getid()
  print("current_window_id: " .. current_window_id)
  local current_window_info = vim.fn.getwininfo(current_window_id)[1]
  print(require("cort.util").serialize_table(current_window_info))
end

vim.keymap.set("n", "<leader>wi", window_info, { desc = "Window info" })
