local neogit = require("neogit")

neogit.setup({
  disable_hint = true,
  disable_insert_on_commit = false,

  integrations = {
    diffview = true
  },

  -- Setting any section to `false` will make the section not render at all
  sections = {
    rebase = {
      hidden = false,
      folded = false
    },
    recent = {
      hidden = false,
      folded = false
    },
    staged = {
      hidden = false,
      folded = false
    },
    stashes = {
      hidden = false,
      folded = false
    },
    unmerged_pushRemote = {
      hidden = false,
      folded = false
    },
    unmerged_upstream = {
      hidden = false,
      folded = false
    },
    unpulled_pushRemote = {
      hidden = false,
      folded = false
    },
    unpulled_upstream = {
      hidden = false,
      folded = false
    },
    unstaged = {
      hidden = false,
      folded = false
    },
    untracked = {
      hidden = false,
      folded = false
    },
  },
})

local which_key = require("which-key")
which_key.add({ "<leader>g", group = " 󰊢 Git" })
vim.keymap.set("n", "<leader>gg", neogit.open, { desc = "Neogit status" })

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "Neogit*" },
  callback = function()
    require("ufo").detach()
    vim.opt_local.foldenable = false
    vim.opt_local.foldcolumn = "0"
  end,
})
