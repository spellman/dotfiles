return {
  "HiPhish/rainbow-delimiters.nvim",
  event = "LazyFile",
  config = function()
    local rainbow_delimiters = require("rainbow-delimiters")

    -- Configure rainbow delimiters
    -- Source: https://github.com/HiPhish/rainbow-delimiters.nvim#migrating-from-nvim-ts-rainbow2
    vim.g.rainbow_delimiters = {
      strategy = {
        [""] = rainbow_delimiters.strategy["global"],  -- Default strategy
        vim = rainbow_delimiters.strategy["local"],    -- Local strategy for Vim files
      },
      query = {
        [""] = "rainbow-delimiters",                   -- Default query
        jsx = "rainbow-delimiters-react",              -- React JSX
        lua = "rainbow-blocks",                        -- Lua blocks
        tsx = "rainbow-delimiters-react",              -- React TSX
      },
      -- Custom highlight groups (commented out to use theme defaults)
      -- highlight = {
      --   "RainbowDelimiterBlue",
      --   "RainbowDelimiterOrange", 
      --   "RainbowDelimiterGreen",
      --   "RainbowDelimiterViolet",
      --   "RainbowDelimiterCyan",
      --   "RainbowDelimiterRed",
      --   "RainbowDelimiterYellow",
      -- },
    }
  end,
}