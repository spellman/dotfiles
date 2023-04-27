local mark = require("harpoon.mark")
local ui = require ("harpoon.ui")

vim.keymap.set("n", "<leader>a", mark.add_file)
vim.keymap.set("n", "<D-e>", ui.toggle_quick_menu)
vim.keymap.set("n", "<D-1>", function() ui.nav_file(1) end)
vim.keymap.set("n", "<D-2>", function() ui.nav_file(2) end)
vim.keymap.set("n", "<D-3>", function() ui.nav_file(3) end)
vim.keymap.set("n", "<D-4>", function() ui.nav_file(4) end)
