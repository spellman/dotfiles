local builtin = require("statuscol.builtin")

-- Hide status-column numbers that show the folding level, as per
-- https://github.com/kevinhwang91/nvim-ufo/issues/4#issuecomment-1512772530
require("statuscol").setup({
  ft_ignore = {
    "netrw",
    "Neogit*"
  },
  segments = {
    {
      text = { builtin.foldfunc, " " },
      click = "v:lua.ScFa",
      auto = false
    },
    {
      sign = {
        namespace = { "diagnostic/signs" },
        auto = false
      },
      click = "v:lua.ScSa"
    },
    {
      sign = {
        namespace = { "gitsigns" },
        colwidth = 2,
        auto = false
      },
      click = "v:lua.ScSa"
    },
    {
      text = { builtin.lnumfunc, " " },
      click = "v:lua.ScLa",
    },
  },
})
