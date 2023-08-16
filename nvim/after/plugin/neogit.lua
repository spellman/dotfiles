local neogit = require("neogit")

neogit.setup({
  commit_popup = {
    kind = "split_above",
  },
  disable_hint = true,
  disable_insert_on_commit = false,

  -- Setting any section to `false` will make the section not render at all
  sections = {
    rebase = {
      folded = false
    },
    recent = {
      folded = false
    },
    staged = {
      folded = false
    },
    stashes = {
      folded = false
    },
    unmerged = {
      folded = false
    },
    unmerged_pushRemote = {
      folded = false
    },
    unmerged_upstream = {
      folded = false
    },
    unpulled = {
      folded = false
    },
    unpulled_pushRemote = {
      folded = false
    },
    unpulled_upstream = {
      folded = false
    },
    unstaged = {
      folded = false
    },
    untracked = {
      folded = false
    },
  },
  signs = {
    -- { CLOSED, OPENED }
    section = { ">", "v" },
    item = { ">", "v" },
    hunk = { ">", "v" },
  },
})
