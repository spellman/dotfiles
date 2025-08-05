require("lualine").setup({
  options = {
    disabled_filetypes = {
      statusline = {
        "qf", -- quickfix and location list buffers
        "NeogitCommitView",
        "NeogitStatus",
        "TelescopePrompt",
        "undotree",
      },
      winbar = {
        "qf", -- quickfix and location list buffers
        "NeogitCommitView",
        "NeogitStatus",
        "TelescopePrompt",
        "undotree",
      },
    },
    icons_enabled = true,
    theme = "auto",
    component_separators = "|",
    section_separators = "|",
    -- always_divide_middle = true,
  },
  sections = {
    lualine_a = {"filename"},
    lualine_b = {"location", "progress"},
    lualine_c = {"filetype"},
    lualine_x = {"branch"},
    lualine_y = {"diagnostics"},
    lualine_z = {"mode"},
  },
  inactive_sections = {
    lualine_a = {"filename"},
    lualine_b = {"location"},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {},
    lualine_z = {}
  }
})

