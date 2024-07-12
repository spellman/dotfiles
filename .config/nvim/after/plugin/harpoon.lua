local harpoon = require("harpoon")

harpoon:setup({})

vim.keymap.set("n", "<leader>a", function() harpoon:list():add() end, { desc = "Add location to harpoon" })
vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)

-- Toggle previous and next buffers stored within the Harpoon list.
vim.keymap.set("n", "<C-S-P>", function() harpoon:list():prev() end)
vim.keymap.set("n", "<C-S-N>", function() harpoon:list():next() end)

harpoon:extend({
  UI_CREATE = function(cx)
    vim.keymap.set("n", "<C-v>", function()
      harpoon.ui:select_menu_item({ vsplit = true })
    end, { buffer = cx.bufnr })

    vim.keymap.set("n", "<C-x>", function()
      harpoon.ui:select_menu_item({ split = true })
    end, { buffer = cx.bufnr })

    vim.keymap.set("n", "<C-t>", function()
      harpoon.ui:select_menu_item({ tabedit = true })
    end, { buffer = cx.bufnr })
  end,
})
