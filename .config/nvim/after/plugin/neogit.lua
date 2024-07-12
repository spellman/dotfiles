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
  -- Disables a second set of signs, to the right of those configured below.
  -- Why do they exist?
  disable_signs = true,
  -- Configures signs that still appear even when disable_signs is set to true.
  signs = {
    -- { CLOSED, OPENED }
    section = { ">", "v" },
    item = { ">", "v" },
    hunk = { ">", "v" },
  },
})

local which_key = require("which-key")
which_key.register(
  {
    g = { name = " 󰊢 Git" }
  },
  { prefix = "<leader>" }
)
vim.keymap.set("n", "<leader>gg", neogit.open, { desc = "Neogit status" })
