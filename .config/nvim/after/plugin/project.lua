local project_nvim = require("project_nvim")

project_nvim.setup({
  -- your configuration comes here
  -- or leave it empty to use the default settings
  -- refer to the configuration section below
  exclude_dirs = {"~"},

  -- Show hidden files in telescope
  show_hidden = true,

  -- What scope to change the directory, valid options are
  -- * global (default)
  -- * tab
  -- * win
  scope_chdir = "win",
})
