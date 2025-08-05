return {
  -- Clojure LSP setup
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers = opts.servers or {}
      opts.servers.clojure_lsp = {
        cmd = { "clojure-lsp" },
        filetypes = { "clojure", "edn" },
        root_dir = function(fname)
          local util = require("lspconfig.util")
          return util.root_pattern("project.clj", "deps.edn", "bb.edn", "shadow-cljs.edn", ".git")(fname)
        end,
        settings = {
          ["clojure-lsp"] = {
            -- Enable semantic tokens for better syntax highlighting
            ["semantic-tokens?"] = true,
            -- Clean namespace on save
            ["clean"] = {
              ["automatically-after-ns-refactor"] = true,
              ["ns-inner-blocks-indentation"] = "next-line",
              ["ns-import-classes-indentation"] = "next-line",
            },
            -- Completion settings
            ["completion"] = {
              ["analysis-type"] = "fast-and-full",
            },
            -- Diagnostics
            ["diagnostics"] = {
              ["range-type"] = "simple",
            },
          },
        },
      }
    end,
  },

  -- Ensure clojure-lsp is installed via Mason
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, {
        "clojure-lsp",
      })
    end,
  },

  -- Optional: Add clj-kondo for additional linting (works with clojure-lsp)
  {
    "mfussenegger/nvim-lint",
    optional = true,
    opts = function(_, opts)
      opts.linters_by_ft = opts.linters_by_ft or {}
      opts.linters_by_ft.clojure = { "clj-kondo" }
    end,
  },

  -- Ensure clj-kondo is also installed if nvim-lint is being used
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, {
        "clj-kondo",  -- Static analysis and linting
      })
    end,
  },
}