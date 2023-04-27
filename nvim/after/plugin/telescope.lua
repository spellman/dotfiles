-- See `:help telescope` and `:help telescope.setup()`
require("telescope").setup({
  defaults = {
    mappings = {
      i = {
        ["<C-u>"] = false,
        ["<C-d>"] = false,
      },
    },
  },
})

-- Enable telescope fzf native
require("telescope").load_extension("fzf")

-- project.nvim Telescope integration
require("telescope").load_extension("projects")

-- See `:help telescope.builtin`
local builtin = require("telescope.builtin")
vim.keymap.set("n", "<leader>?", builtin.oldfiles, { desc = "[?] Find recently opened files" })
vim.keymap.set("n", "<leader><space>", builtin.buffers, { desc = "[ ] Find existing buffers" })
vim.keymap.set("n", "<leader>sb", function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  builtin.current_buffer_fuzzy_find(require("telescope.themes").get_ivy({
    winblend = 10,
  }))
end, { desc = "[s]each [b]uffer" })

vim.keymap.set("n", "<C-p>", builtin.git_files, { desc = "Fuzzy find Git files" })
vim.keymap.set("n", "<leader>sf", builtin.find_files, { desc = "[s]earch [f]iles" })
vim.keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "[s]earch [h]elp" })
vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[s]earch current [w]ord" })
vim.keymap.set("n", "<leader>ss", function ()
  builtin.grep_string({ search = vim.fn.input("Grep term: ") });
end, { desc = "[s]earch current [w]ord" })
vim.keymap.set("n", "<leader>sg", builtin.live_grep, { desc = "[s]earch by [g]rep" })
vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[s]earch [d]iagnostics" })
vim.keymap.set("n", "<leader>sc", builtin.commands, { desc = "[s]earch [c]ommands" })
vim.keymap.set("n", "<leader>sC", builtin.command_history, { desc = "[s]earch [C]ommand_history" })
vim.keymap.set("n", "<leader>sS", builtin.search_history, { desc = "[s]earch [S]earch_history" })
vim.keymap.set("n", "<leader>sm", builtin.marks, { desc = "[s]earch [s]earch_marks" })
vim.keymap.set("n", "<leader>sq", builtin.quickfix, { desc = "[s]earch [q]uickfix" })
vim.keymap.set("n", "<leader>sQ", builtin.quickfixhistory, { desc = "[s]earch [Q]uickfix_history" })
vim.keymap.set("n", "<leader>sr", builtin.resume, { desc = "[s]earch [r]esume" })
vim.keymap.set("n", "<leader>sR", builtin.registers, { desc = "[s]earch [R]egisters" })

