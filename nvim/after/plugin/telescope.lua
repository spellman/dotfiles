-- See `:help telescope` and `:help telescope.setup()`
require("telescope").setup({
  defaults = {
    mappings = {
      i = {
        ["<C-u>"] = false,
        ["<C-d>"] = false,
      },
    },
  },
})

local builtin = require("telescope.builtin")

-- telescope fzf native
require("telescope").load_extension("fzf")

-- telescope-file-browser plugin
-- telescope-file-browser used to be part of telescope but was moved to an
-- extension. As per https://github.com/ahmedkhalf/project.nvim/pull/59, we
-- seem to be in a transitional period where other extensions are adapting to
-- that.
-- As a workout THAT SHOULD BE TEMPORARY, we monkey patch
-- telescope-file-browser to be part of telescope.
local file_browser = require('telescope').load_extension('file_browser')
builtin.file_browser = file_browser.file_browser

-- project.nvim
require("telescope").load_extension("projects")

local which_key = require("which-key")
which_key.register(
  {
    f = { name = " 󰍉 Find" }
  },
  { prefix = "<leader>" }
)
vim.keymap.set("n", "<leader>?", builtin.oldfiles, { desc = "Find recently opened files" })
vim.keymap.set("n", "<leader><space>", builtin.buffers, { desc = "Find existing buffers" })
vim.keymap.set("n", "<leader>f<Cr>", builtin.resume, { desc = "Resume previous search" })
vim.keymap.set("n", "<leader>fa", function()
  local cwd = vim.fn.stdpath("config")
  builtin.find_files({
    prompt_title = "Nvim Config Files",
    find_command = {
      "rg",
      "--files", "--hidden", "--no-ignore-vcs",
      "--glob", "!.git"
    },
    cwd = cwd,
  })
end, { desc = "Find nvim config files" })
vim.keymap.set("n", "<leader>fb", function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  builtin.current_buffer_fuzzy_find(require("telescope.themes").get_ivy({
    winblend = 10,
  }))
end, { desc = "Find in buffer" })
vim.keymap.set("n", "<leader>fc", builtin.commands, { desc = "Find command" })
vim.keymap.set("n", "<leader>fC", builtin.command_history, { desc = "Find command in history" })
vim.keymap.set("n", "<leader>fd", builtin.diagnostics, { desc = "Find in diagnostics" })
vim.keymap.set("n", "<leader>fg", builtin.git_files, { desc = "Find files in Git" })
vim.keymap.set("n", "<leader>ff", function()
  builtin.find_files({
    find_command = {
      "rg",
      "--files", "--hidden", "--no-ignore-vcs",
      "--glob", "!.git"
    }
  })
end, { desc = "Find files in project" })
vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Find in help" })
vim.keymap.set("n", "<leader>fH", builtin.search_history, { desc = "Find history" })
vim.keymap.set("n", "<leader>fk", builtin.keymaps, { desc = "Find in keymaps" })
vim.keymap.set("n", "<leader>fm", builtin.marks, { desc = "Find in marks" })
vim.keymap.set("n", "<leader>fq", builtin.quickfix, { desc = "Find in quickfix" })
vim.keymap.set("n", "<leader>fQ", builtin.quickfixhistory, { desc = "Find in quickfix history" })
vim.keymap.set("n", "<leader>fr", builtin.registers, { desc = "Find registers" })
vim.keymap.set("n", "<leader>fs", builtin.live_grep, { desc = "Find search term in project" })
vim.keymap.set("n", "<leader>fw", builtin.grep_string, { desc = "Find word under cursor in project" })

local projects = require("telescope").extensions.projects
vim.keymap.set("n", "<leader>p", function()
  projects.projects({})
end, { desc = "Switch project" })
