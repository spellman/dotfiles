require("onedark").setup({
  style = "light",
  toggle_style_key = "<leader>ut",

  -- Plugins Config --
  diagnostics = {
    darker = true, -- darker colors for diagnostic?
    undercurl = false,   -- undercurl vs underline for diagnostics
    background = true,    -- use background color for virtual text
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

vim.cmd.colorscheme("onedark")
