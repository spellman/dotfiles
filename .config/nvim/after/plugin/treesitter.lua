-- See `:help nvim-treesitter`
require("nvim-treesitter.configs").setup({
  -- Add languages to be installed here that you want installed for treesitter
  ensure_installed = {
    "clojure",
    "css",
    "dockerfile",
    "html",
    "java",
    "javascript",
    "json",
    "lua",
    "markdown",
    "markdown_inline",
    "python",
    "regex",
    "scss",
    "toml",
    "tsx",
    "typescript",
    "vim",
    "vimdoc",
    "yaml",
  },
  -- Autoinstall languages that are not installed. Defaults to false (but you can change for yourself!)
  auto_install = true,
  autopairs = { enable = true, },
  endwise = { enable = true, },
  -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
  -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
  -- Using this option may slow down your editor, and you may see some duplicate highlights.
  -- Instead of true it can also be a list of languages.
  highlight = {
    enable = true,
    additional_vim_regex_highlighting = false
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-space>",
      node_incremental = "<C-space>",
      scope_incremental = "<C-s>",
      node_decremental = "<M-space>",
    },
  },
  indent = {
    enable = true,
  },
  matchup = { enable = true },
  textobjects = {
    lsp_interop = {
      enable = true,
      border = "single",
      peek_definition_code = {
        ["<leader>lp"] = {
          query = "@function.outer",
          desc = "Peek function definition"
        },
        ["<leader>lP"] = {
          query = "@class.outer",
          desc = "Peek class definition"
        },
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]b"] = { query = "@block.outer", desc = "Next block start" },
        ["]f"] = { query = "@function.outer", desc = "Next function start" },
        ["]p"] = { query = "@parameter.outer", desc = "Next parameter start" },
        ["]x"] = { query = "@class.outer", desc = "Next class start" },
        ["]c"] = { query = "@comment.outer", desc = "Next comment start" },
      },
      goto_next_end = {
        ["]B"] = { query = "@block.outer", desc = "Next block end" },
        ["]F"] = { query = "@function.outer", desc = "Next function end" },
        ["]P"] = { query = "@parameter.outer", desc = "Next parameter end" },
        ["]X"] = { query = "@class.outer", desc = "Next class end" },
        ["]C"] = { query = "@comment.outer", desc = "Next comment end" },
      },
      goto_previous_start = {
        ["[b"] = { query = "@block.outer", desc = "Previous block start" },
        ["[f"] = { query = "@function.outer", desc = "Previous function start" },
        ["[p"] = { query = "@parameter.outer", desc = "Previous parameter start" },
        ["[x"] = { query = "@class.outer", desc = "Previous class start" },
        ["[c"] = { query = "@comment.outer", desc = "Previous comment start" },
      },
      goto_previous_end = {
        ["[B"] = { query = "@block.outer", desc = "Previous block end" },
        ["[F"] = { query = "@function.outer", desc = "Previous function end" },
        ["[P"] = { query = "@parameter.outer", desc = "Previous parameter end" },
        ["[X"] = { query = "@class.outer", desc = "Previous class end" },
        ["[C"] = { query = "@comment.outer", desc = "Previous comment end" },
      },
    },
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
          ["aA"] = "@attribute.outer",
          ["iA"] = "@attribute.inner",
          ["aB"] = "@block.outer",
          ["iB"] = "@block.inner",
          ["aD"] = "@conditional.outer",
          ["iD"] = "@conditional.inner",
          ["aF"] = "@function.outer",
          ["iF"] = "@function.inner",
          ["aL"] = "@loop.outer",
          ["iL"] = "@loop.inner",
          ["aP"] = "@parameter.outer",
          ["iP"] = "@parameter.inner",
          ["aR"] = "@regex.outer",
          ["iR"] = "@regex.inner",
          ["aX"] = "@class.outer",
          ["iX"] = "@class.inner",

          ["aS"] = "@statement.outer",
          ["iS"] = "@statement.outer",
          ["aN"] = "@number.inner",
          ["iN"] = "@number.inner",
          ["aC"] = "@comment.outer",
          ["iC"] = "@comment.outer",
        },
    },
    swap = {
      enable = true,
      swap_next = {
        [">B"] = { query = "@block.outer", desc = "Swap next block" },
        [">F"] = { query = "@function.outer", desc = "Swap next function" },
        [">P"] = { query = "@parameter.inner", desc = "Swap next parameter" },
      },
      swap_previous = {
        ["<B"] = { query = "@block.outer", desc = "Swap previous block" },
        ["<F"] = { query = "@function.outer", desc = "Swap previous function" },
        ["<P"] = { query = "@parameter.inner", desc = "Swap previous parameter" },
      },
    },
  },
})
