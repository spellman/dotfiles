-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local function del_if_exists(mode, lhs)
  for _, map in ipairs(vim.api.nvim_get_keymap(mode)) do
    if map.lhs == lhs then
      vim.keymap.del(mode, lhs)
      return
    end
  end
end

del_if_exists("n", "H")
del_if_exists("n", "L")

-- Q goes to ex mode, which isn't supposed to be useful interactively.
-- We disable it.
vim.keymap.set("n", "Q", "<nop>")

-- Center cursor vertically in split when jumping forward or backward by half a
-- page.
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")

-- Center cursor vertically in split when going to next or previous search term.
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")

-- Paste without overwriting default register contents. So you can paste the
-- same thing again.
vim.keymap.set("x", "<M-S-p>", '"_dP')
vim.keymap.set("x", "<M-P>", "<S-M-p>", { remap = true })

-- Replace occurrences of word under cursor in buffer.
require("which-key").add({
  { "<leader>r", group = "Rename/Reset" },
  {
    "<leader>rw",
    ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>",
    desc = "Rename word under cursor in buffer",
  },
})

vim.keymap.set("n", "<M-S-'>", 'ciw""<ESC>P', { desc = 'Wrap word under cursor in "' })
vim.keymap.set("n", '<M-">', "<M-S-'>", { desc = 'Wrap word under cursor in "', remap = true })

-------------------------------------------------------------------------------------
-- Begin Nice Line Join
-------------------------------------------------------------------------------------
local function current_character_and_neighbors(line_number, column_number)
  local line = vim.fn.getline(line_number)

  return {
    previous = line:sub(column_number - 1, column_number - 1),
    current = line:sub(column_number, column_number),
    next = line:sub(column_number + 1, column_number + 1),
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

-------------------------------------------------------------------------------------
-- End Nice Line Join
-------------------------------------------------------------------------------------

local function window_info()
  local current_window_id = vim.fn.win_getid()
  local current_window_info = vim.fn.getwininfo(current_window_id)[1]
  vim.print(current_window_info)
end

require("which-key").add({
  { "<leader>w", group = "Workspace/Window" },
  {
    "<leader>wi",
    window_info,
    desc = "Window info",
  },
})

vim.api.nvim_create_user_command("CopyFileName", function()
  vim.fn.setreg("+", vim.fn.expand("%:t"))
end, { nargs = 0 })

vim.api.nvim_create_user_command("CopyFilePath", function()
  vim.fn.setreg("+", vim.fn.expand("%:p"))
end, { nargs = 0 })

vim.keymap.set("n", "<leader>fn", function()
  local cwd = vim.fn.expand("%:p") -- current buffer's full path (works in netrw)
  cwd = vim.fn.fnamemodify(cwd, ":p:h") -- get the directory part

  vim.ui.input({
    prompt = "New file in " .. cwd .. "/",
    completion = "file",
  }, function(input)
    if input and #input > 0 then
      local new_path = cwd .. "/" .. input
      vim.cmd("edit " .. vim.fn.fnameescape(new_path))
    end
  end)
end, { desc = "New file in dir" })
