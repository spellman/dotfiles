return {
  "folke/snacks.nvim",
  opts = {
    picker = {
      sources = {
        files = {
          cmd = "fd",
          args = {
            "--color", "never",
            "--no-ignore-vcs",  -- Don't ignore files based on VCS ignore files
            "--hidden",         -- Include hidden files
            "--exclude", ".git", -- Exclude .git directory
            "--type", "file",   -- Only find files, not directories
          },
        },
        grep = {
          cmd = "rg",
          args = {
            "--no-ignore-vcs",  -- Don't ignore files based on VCS ignore files
            "--hidden",         -- Include hidden files
            "--glob", "!.git",  -- Exclude .git directory with glob pattern
          },
        },
      },
    },
  },
}