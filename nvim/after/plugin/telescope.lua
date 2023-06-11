-- See `:help telescope` and `:help telescope.setup()`
require("telescope").setup({
  defaults = {
    mappings = {
      i = {
        ["<C-u>"] = false,
        ["<C-d>"] = false,
      },
    },
    path_display = { "truncate" },
    sorting_strategy = "ascending",
    layout_config = {
      horizontal = {
        prompt_position = "top",
        preview_width = 0.55,
      },
      vertical = {
        mirror = false,
      },
      width = 0.87,
      height = 0.80,
      preview_cutoff = 120,
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
local project_nvim_project = require("project_nvim.project")

local which_key = require("which-key")
which_key.register(
  {
    f = { name = " 󰍉 Find" }
  },
  { prefix = "<leader>" }
)
vim.keymap.set("n", "<leader><space>", builtin.buffers, { desc = "Find buffer" })
vim.keymap.set("n", "<leader>f<Cr>", builtin.resume, { desc = "Resume previous search" })
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
  -- (*)
  -- I typically want to consider a git repo to be a project and
  -- project_nvim_project.find_pattern_root seems to find the nearest containing
  -- git repo so I use that function.

  -- I first tried using project_nvim_project.get_project_root but that detects
  -- a project whenever an LSP server detects a project. For example, it detects
  -- each heuristics plugin as a project. That's great for LSP operations but
  -- it's not what I want for finding files.

  -- If I want to narrow searches, I think telescope provides a facility for
  -- that.
  local project_root, _ = project_nvim_project.find_pattern_root()
  builtin.find_files({
    prompt_title = "Find Files ("..project_root..")",
    find_command = {
      "fd",
      "--color", "never",
      "--no-ignore-vcs",
      "--exclude", ".git",
    },
    cwd = project_root,
  })
end, { desc = "Find files in project" })
vim.keymap.set("n", "<leader>fh", builtin.help_tags, { desc = "Find in help" })
vim.keymap.set("n", "<leader>fH", builtin.search_history, { desc = "Find history" })
vim.keymap.set("n", "<leader>fk", builtin.keymaps, { desc = "Find in keymaps" })
vim.keymap.set("n", "<leader>fm", builtin.marks, { desc = "Find marks" })
vim.keymap.set("n", "<leader>fn", function()
  local nvim_config_dir = vim.fn.stdpath("config")
  builtin.find_files({
    prompt_title = "Nvim Config Files ("..nvim_config_dir..")",
    find_command = {
      "fd",
      "--color", "never",
      "--no-ignore-vcs",
      "--exclude", ".git",
    },
    cwd = nvim_config_dir,
  })
end, { desc = "Find nvim config files" })
vim.keymap.set("n", "<leader>fq", builtin.quickfix, { desc = "Find in quickfix" })
vim.keymap.set("n", "<leader>fQ", builtin.quickfixhistory, { desc = "Find in quickfix history" })
vim.keymap.set("n", "<leader>fr", builtin.oldfiles, { desc = "Find recently opened files" })
vim.keymap.set("n", "<leader>fR", builtin.registers, { desc = "Find registers" })
vim.keymap.set("n", "<leader>fs", function()
  -- See (*) for why we use project_nvim_project.find_pattern_root.
  local project_root, _ = project_nvim_project.find_pattern_root()
  builtin.live_grep({
    prompt_title = "Search in Project",
    find_command = {
      "rg",
      "--color", "never",
      "--hidden", "--no-ignore-vcs",
      "--glob", "!.git"
    },
    cwd = project_root,
  })
end, { desc = "Find search term in project" })
vim.keymap.set("n", "<leader>fw", function()
  -- See (*) for why we use project_nvim_project.find_pattern_root.
  local project_root, _ = project_nvim_project.find_pattern_root()
  builtin.grep_string({
    find_command = {
      "rg",
      "--color", "never",
      "--hidden", "--no-ignore-vcs",
      "--glob", "!.git"
    },
    cwd = project_root,
  })
end, { desc = "Find word under cursor in project" })

local projects = require("telescope").extensions.projects
vim.keymap.set("n", "<leader>p", function()
  projects.projects({})
end, { desc = "Switch project" })
