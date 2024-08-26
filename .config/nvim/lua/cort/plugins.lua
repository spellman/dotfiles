-- NOTE: `opts = {}` is the same as calling `require(<package>).setup({})`
return {
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
      "j-hui/fidget.nvim",

      -- Additional lua configuration, makes nvim stuff amazing!
      "folke/neodev.nvim",

      -- We will map keys to telescope commands in the on_attach function provided to mason.
      "nvim-telescope/telescope.nvim",
    },
  },

  {
    "L3MON4D3/LuaSnip",
    -- follow latest release.
    version = "v2.*",                -- Replace <CurrentMajor> by the latest released major (first number of latest release)
    build = "make install_jsregexp", -- install jsregexp (optional!)
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

  {
    "hrsh7th/cmp-nvim-lsp-signature-help",
    dependencies = {
      "hrsh7th/nvim-cmp",
    },
  },

  {
    "danymat/neogen",
    config = true,
    -- Uncomment next line if you want to follow only stable versions
    version = "*"
  },

  -- Useful status updates for LSP
  {
    "j-hui/fidget.nvim",
    tag = "v1.4.5",
  },

  -- Additional lua configuration, makes nvim stuff amazing!
  "folke/neodev.nvim",

  -- Useful plugin to show you pending keybinds.
  {
    "folke/which-key.nvim",
    opts = {}
  },

  {
    -- Adds git releated signs to the gutter, as well as utilities for managing changes
    "lewis6991/gitsigns.nvim",
  },

  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "sindrets/diffview.nvim",
    },
  },

  "sindrets/diffview.nvim",

  {
    "catppuccin/nvim",
    name = "catppuccin",
    priority = 1000,
  },

  "nvim-lualine/lualine.nvim",

  -- "gc" to comment visual regions/lines
  {
    "numToStr/Comment.nvim",
    opts = {}
  },

  {
    -- Keep an eye on https://github.com/PaterJason/nvim-treesitter-sexp as a
    -- replacement.
    "guns/vim-sexp",
    dependencies = {
      "tpope/vim-repeat"
    }
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

  {
    "nvim-telescope/telescope-file-browser.nvim",
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "nvim-lua/plenary.nvim"
    }
  },

  {
    -- Highlight, edit, and navigate code
    "nvim-treesitter/nvim-treesitter",
    dependencies = {
      "nvim-treesitter/nvim-treesitter-textobjects",

      -- We will configure treesitter to use nvim-treesitter-endwise so we list it as a dependency.
      "RRethy/nvim-treesitter-endwise",
      {
        -- Source: https://code.mehalter.com/AstroNvim_user/~files/master/plugins/treesitter.lua
        "andymass/vim-matchup",
        init = function() vim.g.matchup_matchparen_deferred = 1 end,
      },
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

  -- project.nvim for setting the current directory and telescope integration
  "ahmedkhalf/project.nvim",

  "mbbill/undotree",

  -- Source
  -- https://alpha2phi.medium.com/neovim-for-beginner-auto-pairs-c09e87a4d511
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
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
  },

  -- {
  --   "kevinhwang91/nvim-bqf",
  --   ft = "qf"
  -- },

  -- {
  --   "yorickpeterse/nvim-pqf",
  --   opts = {},
  -- },

  "HiPhish/rainbow-delimiters.nvim",

  "luukvbaal/statuscol.nvim",

  {
    "kevinhwang91/nvim-ufo",
    dependencies = {
      "kevinhwang91/promise-async",
      "luukvbaal/statuscol.nvim",
    },
    event = "VeryLazy",
  },

  {
    "utilyre/barbecue.nvim",
    name = "barbecue",
    dependencies = {
      "SmiteshP/nvim-navic",
      "nvim-tree/nvim-web-devicons", -- optional dependency
    },
    opts = {},
  },

  {
    "gennaro-tedesco/nvim-jqx",
    ft = { "json", "yaml" },
  },

  {
    "dhruvasagar/vim-table-mode",
  },

  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
  },

  -- Indent guides
  {
    "lukas-reineke/indent-blankline.nvim",
    main = "ibl",
    ---@module "ibl"
    ---@type ibl.config
    opts = {},
    config = function()
      require("ibl").setup()
    end
  }
}
