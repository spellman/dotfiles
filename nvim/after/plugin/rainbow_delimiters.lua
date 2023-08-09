local rainbow_delimiters = require("rainbow-delimiters")

-- [""] is the default query.
-- Source: https://github.com/HiPhish/rainbow-delimiters.nvim#migrating-from-nvim-ts-rainbow2
vim.g.rainbow_delimiters = {
  strategy = {
    [""] = rainbow_delimiters.strategy["global"],
    vim = rainbow_delimiters.strategy["local"],
  },
  query = {
    [""] = "rainbow-delimiters",
    jsx = "rainbow-delimiters-react",
    lua = "rainbow-blocks",
    tsx = "rainbow-delimiters-react",
  },
  highlight = {
    "RainbowDelimiterBlue",
    "RainbowDelimiterOrange",
    "RainbowDelimiterGreen",
    "RainbowDelimiterViolet",
    "RainbowDelimiterCyan",
    "RainbowDelimiterRed",
    "RainbowDelimiterYellow",
  },
}
