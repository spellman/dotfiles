-- NOTE: Here is where you install your plugins.
--  You can configure plugins using the `config` key.
--
--  You can also configure plugins after the setup call,
--    as they will be available in your neovim runtime.
return {
  -- Git related plugins
  "tpope/vim-fugitive",
  -- "tpope/vim-rhubarb",

  "nvim-tree/nvim-web-devicons",

  -- Detect tabstop and shiftwidth automatically
  "tpope/vim-sleuth",

  "tpope/vim-surround",

  "tpope/vim-vinegar",

  {
    "neovim/nvim-lspconfig",
    dependencies = {
      -- Automatically install LSPs to stdpath for neovim
      { "williamboman/mason.nvim", config = true },
      "williamboman/mason-lspconfig.nvim",

      -- Useful status updates for LSP
      -- NOTE: `opts = {}` is the same as calling `require("fidget").setup({})`
      { "j-hui/fidget.nvim", opts = {} },

      -- Additional lua configuration, makes nvim stuff amazing!
      "folke/neodev.nvim",

      -- We will map keys to telescope commands in the on_attach function provided to mason.
      "nvim-telescope/telescope.nvim",
    },
  },

  {
    "hrsh7th/nvim-cmp",
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-cmdline",
      "hrsh7th/cmp-path",
      "L3MON4D3/LuaSnip",
      "saadparwaiz1/cmp_luasnip",

      -- We will use autopairs functionality in the configuration of nvim-cmp.
      "windwp/nvim-autopairs",
    },
  },

  -- Useful status updates for LSP
  -- NOTE: `opts = {}` is the same as calling `require("fidget").setup({})`
  { "j-hui/fidget.nvim", opts = {} },

  -- Additional lua configuration, makes nvim stuff amazing!
  "folke/neodev.nvim",

  -- Useful plugin to show you pending keybinds.
  { "folke/which-key.nvim", opts = {} },

  { -- Adds git releated signs to the gutter, as well as utilities for managing changes
    "lewis6991/gitsigns.nvim",
    opts = {
      -- See `:help gitsigns.txt`
      signs = {
        add = { text = "+" },
        change = { text = "~" },
        delete = { text = "_" },
        topdelete = { text = "‾" },
        changedelete = { text = "~" },
      },
    },
  },

  { -- Theme inspired by Atom
    "navarasu/onedark.nvim",
    priority = 1000,
    config = function()
      vim.cmd.colorscheme("onedark")
    end,
  },

  { -- Set lualine as statusline
    "nvim-lualine/lualine.nvim",
    -- See `:help lualine.txt`
    opts = {
      options = {
        icons_enabled = true,
        theme = "onedark",
        component_separators = "|",
        section_separators = "",
      },
    },
  },

  { -- Add indentation guides even on blank lines
    "lukas-reineke/indent-blankline.nvim",
    -- Enable `lukas-reineke/indent-blankline.nvim`
    -- See `:help indent_blankline.txt`
    opts = {
      char = "┊",
      show_trailing_blankline_indent = false,
    },
  },

  -- "gc" to comment visual regions/lines
  {
    "numToStr/Comment.nvim",
    opts = {}
  },

  -- Fuzzy Finder (files, lsp, etc)
  {
    "nvim-telescope/telescope.nvim",
    version = "*",
    dependencies = {
      "nvim-lua/plenary.nvim"
    }
  },

  -- Fuzzy Finder Algorithm which requires local dependencies to be built.
  -- Only load if `make` is available. Make sure you have the system
  -- requirements installed.
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    -- NOTE: If you are having trouble with this installation,
    --       refer to the README for telescope-fzf-native for more instructions.
    build = "make",
    cond = function()
      return vim.fn.executable("make") == 1
    end,
  },

  { -- Highlight, edit, and navigate code
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",

      -- We will configure treesitter to use nvim-treesitter-endwise so we list it as a dependency.
      "RRethy/nvim-treesitter-endwise",
    },
    config = function()
      pcall(require("nvim-treesitter.install").update({ with_sync = true }))
    end,
  },

  -- Clojure
  "Olical/conjure",
  "PaterJason/cmp-conjure",
  "clojure-vim/clojure.vim",
  "clojure-vim/vim-jack-in",
  {
    "radenling/vim-dispatch-neovim",
    dependencies = {
      "tpope/vim-dispatch"
    }
  },
  { url = "https://gitlab.com/invertisment/conjure-clj-additions-cider-nrepl-mw.git" },
  { url = "https://gitlab.com/invertisment/conjure-clj-additions-vanilla.git" },
  -- {
  --   "w0rp/ale",
  --   config = function()
  --     vim.g.ale_linters = { "clj-kondo" }
  --   end
  -- },

  -- Metals
  {
    "scalameta/nvim-metals",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "mfussenegger/nvim-dap"
    }
  },

  -- project.nvim for setting the current directory and telescope integration
  {
    "ahmedkhalf/project.nvim",
    config = function()
      require("project_nvim").setup({
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
        scope_chdir = "tab",
      })
    end
  },

  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" }
  },

  "mbbill/undotree",

  -- Source
  -- https://alpha2phi.medium.com/neovim-for-beginner-auto-pairs-c09e87a4d511
  {
    "windwp/nvim-autopairs",
    config = function()
      local autopairs = require("nvim-autopairs")
      autopairs.setup({ check_ts = true })
      autopairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))
    end
  },

  -- Source
  -- https://alpha2phi.medium.com/neovim-for-beginner-auto-pairs-c09e87a4d511
  {
    "windwp/nvim-ts-autotag",
    dependencies = {
      "nvim-treesitter"
    },
    event = "InsertEnter",
    config = function()
      require("nvim-ts-autotag").setup({ enable = true })
    end,
  },

  -- Source
  -- https://alpha2phi.medium.com/neovim-for-beginner-auto-pairs-c09e87a4d511
  {
    "RRethy/nvim-treesitter-endwise",
    event = "InsertEnter",
  },

  -- Highlight occurrences of the word under cursor.
  "RRethy/vim-illuminate",

  "nvim-tree/nvim-web-devicons",

  {
    "folke/todo-comments.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("todo-comments").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end
  },

}

