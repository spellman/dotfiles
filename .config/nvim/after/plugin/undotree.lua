require("which-key").add({
  { "<leader>u",  group = "Undo tree" },
  { "<leader>uu", vim.cmd.UndotreeToggle, desc = "Toggle Undotree" }
})
