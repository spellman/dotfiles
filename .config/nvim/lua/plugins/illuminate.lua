return {
  "RRethy/vim-illuminate",
  event = "LazyFile",
  opts = function(_, opts)
    -- Merge with any existing opts from LazyVim
    return vim.tbl_deep_extend("force", opts or {}, {
      -- providers: provider used to get references in the buffer, ordered by priority
      providers = {
        "lsp",
        "treesitter",
        "regex",
      },
      -- delay: delay in milliseconds
      delay = 100,
      -- Ensure clojure is not in the denylist and treesitter works
      filetypes_denylist = {
        "", -- some files have empty filetype. E.g., .log files
        "dirbuf",
        "dirvish",
        "fugitive",
        "markdown",
        "NeogitCommitView",
        "NeogitStatus",
        "qf", -- quickfix and location list buffers
        "TelescopePrompt",
        "text",
        "undotree",
      },
      -- Clojure-specific overrides to ensure it works
      filetype_overrides = {
        clojure = {
          providers = {
            "lsp",
            "treesitter",
            "regex",
          },
          delay = 50, -- Faster for Clojure
        },
      },
      -- Force enable for clojure specifically
      filetypes_allowlist = {},
      -- Make sure it works under cursor
      under_cursor = false,
      -- Minimum count to highlight (useful for clojure's short symbols)
      min_count_to_highlight = 1,
      -- Enable case insensitive regex for better matching
      case_insensitive_regex = false,
    })
  end,
  config = function(_, opts)
    require("illuminate").configure(opts)
  end,
}

