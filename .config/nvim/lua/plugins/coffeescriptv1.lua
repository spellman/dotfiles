return {
  ---------------------------------------------------------------------------
  -- ①  Syntax, indent & ft‑detection
  ---------------------------------------------------------------------------
  {
    "kchmck/vim-coffee-script",
    ft = { "coffee" },
  },

  ---------------------------------------------------------------------------
  -- ②  LSP (CoffeeSense) ---------------------------------------------------
  ---------------------------------------------------------------------------
  {
    "neovim/nvim-lspconfig",
    opts = function(_, opts)
      opts.servers = opts.servers or {}
      opts.servers.coffeesense = {
        settings = {
          coffeesense = {
            coffeeScriptVersion = "1.12.7", -- target runtime
            diagnostics = { enable = true },
            suggestions = { enable = true },
          },
        },
      }
    end,
  },

  ---------------------------------------------------------------------------
  -- ③  Extra tooling via Mason --------------------------------------------
  ---------------------------------------------------------------------------
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      opts.ensure_installed = opts.ensure_installed or {}
      vim.list_extend(opts.ensure_installed, {
        "coffeesense-language-server", -- LSP backend
        "prettierd", -- formatting daemon
        -- NOTE: We can not install coffeelint this way because it's not in the
        -- Mason registry.
      })
    end,
  },

  ---------------------------------------------------------------------------
  -- ④  Lint integration (nvim‑lint) ---------------------------------------
  --     Uses project‑local coffeelint pinned in ~/.config/nvim/tools/coffeelint
  --
  -- This coffeelint was installed via
  -- ```shell
  -- mkdir -p ~/.config/nvim/tools/coffeelint
  -- cd ~/.config/nvim/tools/coffeelint
  -- npm init -y  >/dev/null
  -- npm i coffeelint@^2.1.0 --no-save
  -- ```
  ---------------------------------------------------------------------------
  {
    "mfussenegger/nvim-lint",
    opts = function(_, opts)
      -- Register a minimal linter definition, if it isn't built-in, by
      -- mutating the `lint.linters` table.
      -- Lua modules are cached so, after the first require, every later
      -- reference -- including nvim-lint’s own runtime code -- sees the
      -- updated `lint.linters` table.
      local lint = require("lint") -- requires the `lint` module from nvim-lint

      if not lint.linters.coffeelint then
        lint.linters.coffeelint = {
          name = "coffeelint",
          -- cmd = "coffeelint",      -- Will be resolved via $PATH
          cmd = vim.fn.expand("~/.config/nvim/tools/coffeelint/node_modules/.bin/coffeelint"),
          stdin = false, -- coffeelint must read the file from disk
          append_fname = true, -- Allows coffeelint to read the file from disk
          ignore_exitcode = true, -- coffeelint exits 1 on warnings
          -- super-simple CSV parser: path,line,col,msg
          parser = require("lint.parser").from_pattern(
            "([^,]+),(\\d+),(\\d+),(.+)",
            { "path", "lnum", "col", "message" },
            nil,
            { ["source"] = "coffeelint" }
          ),
        }
      end

      opts.linters_by_ft = opts.linters_by_ft or {}
      opts.linters_by_ft.coffee = { "coffeelint" }
    end,
  },

  ---------------------------------------------------------------------------
  -- ⑤  Format integration (Conform.nvim) ----------------------------------
  --     Uses project‑local prettier pinned in ~/.config/nvim/tools/prettier-coffee
  --
  -- This prettier was installed via
  -- ```shell
  -- mkdir -p ~/.config/nvim/prettier-coffee
  -- cd       ~/.config/nvim/prettier-coffee
  -- npm init -y  >/dev/null
  -- npm install github:helixbass/prettier#prettier-v2.1.0-dev.100-gitpkg \
  --             prettier-plugin-coffeescript \
  --             coffeescript@^2.5.0
  -- ```
  ---------------------------------------------------------------------------
  {
    "stevearc/conform.nvim",
    opts = function(_, opts)
      opts.formatters_by_ft = opts.formatters_by_ft or {}

      -- custom formatter definition for CoffeeScript -----------------------
      opts.formatters_by_ft.coffee = {
        function(_)
          return {
            exe = "prettierd",
            -- Explicitly tell prettierd which Prettier binary to invoke
            args = {
              "--prettier",
              vim.fn.expand("~/.config/nvim/tools/prettier-coffee/node_modules/.bin/prettier"),
            },
            stdin = true,
          }
        end,
      }
    end,
  },
}
