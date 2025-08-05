require("which-key").add({
  { "<leader>uu", vim.cmd.UndotreeToggle, desc = "Toggle Undotree" },
})

return {
  "mbbill/undotree",
}
