require("catppuccin").setup({
  flavour = "latte", -- latte, frappe, macchiato, mocha
  background = { -- :h background
    light = "latte",
    dark = "mocha",
  },
  transparent_background = false, -- disables setting the background color.
  show_end_of_buffer = false, -- shows the '~' characters after the end of buffers
  term_colors = true, -- sets terminal colors (e.g. `g:terminal_color_0`)
  dim_inactive = {
    enabled = false, -- dims the background color of inactive window
    shade = "dark",
    percentage = 0.05, -- percentage of the shade to apply to the inactive window
  },
  no_italic = false, -- Force no italic
  no_bold = false, -- Force no bold
  no_underline = false, -- Force no underline
  styles = { -- Handles the styles of general hi groups (see `:h highlight-args`):
    comments = {}, -- Change the style of comments
    conditionals = {},
    loops = {},
    functions = {},
    keywords = {},
    strings = {},
    variables = {},
    numbers = {},
    booleans = {},
    properties = {},
    types = {},
    operators = {},
  },
  color_overrides = {
    latte = {
      base = "#ffffff",
      mantle = "#f0f0f0",
      crust = "#e4e4e4",
    }
  },
  custom_highlights = {},
  integrations = {
    -- For more plugin integrations see
    -- https://github.com/catppuccin/nvim#integrations
    barbecue = {
      dim_dirname = true, -- directory name is dimmed by default
      bold_basename = true,
      dim_context = false,
      alt_background = false,
    },
    cmp = true,
    fidget = true,
    gitsigns = true,
    illuminate = false,
    mason = true,
    native_lsp = {
      enabled = true,
      virtual_text = {
        errors = { "italic" },
        hints = { "italic" },
        warnings = { "italic" },
        information = { "italic" },
      },
      underlines = {
        errors = { "underline" },
        hints = { "underline" },
        warnings = { "underline" },
        information = { "underline" },
      },
      inlay_hints = {
        background = true,
      },
    },
    neogit = true,
    nvimtree = true,
    treesitter = true,
    treesitter_context = true,
    rainbow_delimiters = true,
    telescope = {
      enabled = true,
    },
    which_key = true,
  },
})

require("which-key").register(
  {
    u = {
      t = { "Toggle theme style" }
    }
  },
  { prefix = "<leader>" }
)

vim.cmd.colorscheme("catppuccin")
