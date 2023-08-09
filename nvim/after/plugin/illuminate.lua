require("illuminate").configure({
  delay = 50,
  filetypes_denylist = {
    "aerial",
    "qf", -- quickfix and location list buffers
    "markdown",
    "NeogitCommitView",
    "NeogitStatus",
    "TelescopePrompt",
    "text",
  },
  under_cursor = false,
})

-- Manual background settings for onedark theme.
-- vim.cmd([[
-- highlight IlluminatedWordText guifg=none guibg=#484858 gui=none
-- highlight IlluminatedWordRead guifg=none guibg=#484858 gui=none
-- highlight IlluminatedWordWrite guifg=none guibg=#484858 gui=none
-- ]])

-- Source: https://ansidev.xyz/posts/2023-04-25-how-to-change-the-highlight-style-using-vim-illuminate
-- change the highlight style
vim.api.nvim_set_hl(0, "IlluminatedWordText", { link = "Visual" })
vim.api.nvim_set_hl(0, "IlluminatedWordRead", { link = "Visual" })
vim.api.nvim_set_hl(0, "IlluminatedWordWrite", { link = "Visual" })

--- auto update the highlight style on colorscheme change
vim.api.nvim_create_autocmd({ "ColorScheme" }, {
  pattern = { "*" },
  callback = function(ev)
    vim.api.nvim_set_hl(0, "IlluminatedWordText", { link = "Visual" })
    vim.api.nvim_set_hl(0, "IlluminatedWordRead", { link = "Visual" })
    vim.api.nvim_set_hl(0, "IlluminatedWordWrite", { link = "Visual" })
  end
})
