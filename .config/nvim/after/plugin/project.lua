local project_nvim = require("project_nvim")

project_nvim.setup({
  patterns = { ".git", "_darcs", ".hg", ".bzr", ".svn", "package.json" },

  exclude_dirs = {"~"},

  -- Show hidden files in telescope
  show_hidden = true,

  -- What scope to change the directory, valid options are
  -- * global (default)
  -- * tab
  -- * win
  scope_chdir = "win",
})
