require("catppuccin").setup({
  flavour = "latte", -- latte, frappe, macchiato, mocha
  background = {     -- :h background
    light = "latte",
    dark = "mocha",
  },
  transparent_background = false, -- disables setting the background color.
  show_end_of_buffer = false,     -- shows the '~' characters after the end of buffers
  term_colors = true,             -- sets terminal colors (e.g. `g:terminal_color_0`)
  dim_inactive = {
    enabled = false,              -- dims the background color of inactive window
    shade = "dark",
    percentage = 0.05,            -- percentage of the shade to apply to the inactive window
  },
  no_italic = false,              -- Force no italic
  no_bold = false,                -- Force no bold
  no_underline = false,           -- Force no underline
  styles = {                      -- Handles the styles of general hi groups (see `:h highlight-args`):
    comments = {},                -- Change the style of comments
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
    },
  },
  highlight_overrides = {
    latte = function(colors)
      local color_utils = require("catppuccin.utils.colors")
      return {
        NeogitBranch = {
          fg = colors.peach,
          style = { "bold" },
        },
        NeogitRemote = {
          fg = colors.green,
          style = { "bold" },
        },
        NeogitUnmergedInto = {
          link = "Function",
        },
        NeogitUnpulledFrom = {
          link = "Function",
        },
        NeogitObjectId = {
          link = "Comment",
        },
        NeogitStash = {
          link = "Comment",
        },
        NeogitRebaseDone = {
          link = "Comment",
        },
        NeogitHunkHeader = {
          bg = color_utils.darken(colors.blue, 0.095, colors.base),
          fg = color_utils.darken(colors.blue, 0.500, colors.base),
        },
        NeogitHunkHeaderHighlight = {
          bg = color_utils.darken(colors.blue, 0.215, colors.base),
          fg = colors.blue,
        },
        NeogitDiffContextHighlight = {
          bg = colors.base,
        },
        NeogitDiffDeleteHighlight = {
          bg = color_utils.darken(colors.red, 0.125, colors.base),
          fg = color_utils.lighten(colors.red, 0.850, colors.text),
        },
        NeogitDiffAddHighlight = {
          bg = color_utils.darken(colors.green, 0.125, colors.base),
          fg = color_utils.lighten(colors.green, 0.850, colors.text),
        },
        NeogitDiffContext = {
          bg = colors.base,
          fg = color_utils.lighten(colors.base, 0.180, colors.text),
        },
        NeogitDiffDelete = {
          bg = color_utils.darken(colors.red, 0.050, colors.base),
          fg = color_utils.darken(colors.red, 0.575, colors.base),
        },
        NeogitDiffAdd = {
          bg = color_utils.darken(colors.green, 0.050, colors.base),
          fg = color_utils.darken(colors.green, 0.575, colors.base),
        },
        NeogitCommitViewHeader = {
          bg = color_utils.darken(colors.blue, 0.300, colors.base),
          fg = color_utils.lighten(colors.blue, 0.800, colors.text),
        },
        NeogitChangeModified = {
          fg = colors.blue,
          style = { "bold" },
        },
        NeogitChangeDeleted = {
          fg = colors.red,
          style = { "bold" },
        },
        NeogitChangeAdded = {
          fg = colors.green,
          style = { "bold" },
        },
        NeogitChangeRenamed = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitChangeUpdated = {
          fg = colors.peach,
          style = { "bold" },
        },
        NeogitChangeCopied = {
          fg = colors.pink,
          style = { "bold" },
        },
        NeogitChangeBothModified = {
          fg = colors.yellow,
          style = { "bold" },
        },
        NeogitChangeNewFile = {
          fg = colors.green,
          style = { "bold" },
        },
        NeogitUntrackedfiles = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitUnstagedchanges = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitUnmergedchanges = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitUnpulledchanges = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitRecentcommits = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitStagedchanges = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitStashes = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitRebasing = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitNotificationInfo = {
          fg = colors.blue,
        },
        NeogitNotificationWarning = {
          fg = colors.yellow,
        },
        NeogitNotificationError = {
          fg = colors.red,
        },
        NeogitGraphRed = {
          fg = colors.red,
        },
        NeogitGraphWhite = {
          fg = colors.base,
        },
        NeogitGraphYellow = {
          fg = colors.yellow,
        },
        NeogitGraphGreen = {
          fg = colors.green,
        },
        NeogitGraphCyan = {
          fg = colors.blue,
        },
        NeogitGraphBlue = {
          fg = colors.blue,
        },
        NeogitGraphPurple = {
          fg = colors.lavender,
        },
        NeogitGraphGray = {
          fg = colors.subtext1,
        },
        NeogitGraphOrange = {
          fg = colors.peach,
        },
        NeogitGraphBoldRed = {
          fg = colors.red,
          style = { "bold" },
        },
        NeogitGraphBoldWhite = {
          fg = colors.white,
          style = { "bold" },
        },
        NeogitGraphBoldYellow = {
          fg = colors.yellow,
          style = { "bold" },
        },
        NeogitGraphBoldGreen = {
          fg = colors.green,
          style = { "bold" },
        },
        NeogitGraphBoldCyan = {
          fg = colors.blue,
          style = { "bold" },
        },
        NeogitGraphBoldBlue = {
          fg = colors.blue,
          style = { "bold" },
        },
        NeogitGraphBoldPurple = {
          fg = colors.lavender,
          style = { "bold" },
        },
        NeogitGraphBoldGray = {
          fg = colors.subtext1,
          style = { "bold" },
        },
        NeogitPopupBold = {
          style = { "bold" },
        },
        NeogitPopupSwitchKey = {
          fg = colors.lavender,
        },
        NeogitPopupOptionKey = {
          fg = colors.lavender,
        },
        NeogitPopupConfigKey = {
          fg = colors.lavender,
        },
        NeogitPopupActionKey = {
          fg = colors.lavender,
        },
        NeogitFilePath = {
          fg = colors.blue,
          style = { "italic" },
        },
        NeogitDiffHeader = {
          bg = colors.base,
          fg = colors.blue,
          style = { "bold" },
        },
        NeogitDiffHeaderHighlight = {
          bg = colors.base,
          fg = colors.peach,
          style = { "bold" },
        },
        NeogitUnpushedTo = {
          fg = colors.lavender,
          style = { "bold" },
        },
        NeogitFold = {
          fg = colors.none,
          bg = colors.none,
        },
        NeogitSectionHeader = {
          fg = colors.mauve,
          style = { "bold" },
        },
        NeogitTagName = {
          fg = colors.yellow,
        },
        NeogitTagDistance = {
          fg = colors.blue,
        },
      }
    end
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
    -- This setting seems to let me use my NeoGit highlight overrides, defined
    -- above in `highlight_overrides`.
    neogit = {},
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

require("which-key").add({ "<leader>ut", desc = "Toggle theme style" })

vim.cmd.colorscheme("catppuccin")
