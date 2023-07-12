local util = require("cort.util")

local debug = false

local function debug_print(x)
  if debug then
    print(x)
  end
end

local parent_kinds_on_which_to_recurse = {
  Class = true,
  Namespace = true,
  Package = true,
}

--- Doing this well is going to be language-specific and will require iteration.
--- Be patient and make 2-second improvements :D
---
--- NOTE: Let's leave the printing in place, governed by the debug flag, until
--- we are confident we won't need to change and debug this function frequently.
--- Invoked after each symbol is parsed, can be used to modify the parsed item,
--- or to filter it by returning false.
---
--- bufnr: a neovim buffer number
--- item: of type aerial.Symbol
--- ctx: a record containing the following fields:
---   * backend_name: treesitter, lsp, man...
---   * lang: info about the language
---   * symbols?: specific to the lsp backend
---   * symbol?: specific to the lsp backend
---   * syntax_tree?: specific to the treesitter backend
---   * match?: specific to the treesitter backend, TS query match
---
--- The aerial.Symbol type is:
--- {
---   kind: SymbolKind,
---   name: string,
---   level: number,
---   parent: aerial.Symbol,
---   lnum: number,
---   end_lnum: number,
---   col: number,
---   end_col: number
--- }
local function post_parse_symbol(bufnr, item, ctx)
  debug_print("item kind:" .. item["kind"])
  debug_print("item name:" .. item["name"])
  debug_print("item level:" .. item["level"])
  if item["parent"] then
    debug_print("item parent kind:" .. item["parent"]["kind"])
  end

  if item["name"] == "if" and item["kind"] == "Package" then
    -- This may be lua-specific. That's ok if it is.
    debug_print("if - false")
    debug_print([[

    ]])
    return false
  elseif item["level"] == 0 then
    debug_print("top level - true")
    debug_print([[

    ]])
    return true
  elseif item["parent"] and parent_kinds_on_which_to_recurse[item["parent"]["kind"]] then
    debug_print("will check parent...")
    return post_parse_symbol(bufnr, item["parent"], ctx)
  else
    debug_print("other - false")
    debug_print([[

    ]])
    return false
  end
end

local aerial = require("aerial")
aerial.setup({
  -- Priority list of preferred backends for aerial.
  -- This can be a filetype map (see :help aerial-filetype-map)
  backends = { "lsp", "treesitter", "markdown", "man" },

  -- List of enum values that configure when to auto-close the aerial window
  --   unfocus       - close aerial when you leave the original source window
  --   switch_buffer - close aerial when you change buffers in the source window
  --   unsupported   - close aerial when attaching to a buffer that has no symbol source
  close_automatic_events = { "unsupported" },

  
  -- A list of all symbols to display. Set to false to display all symbols.
  -- This can be a filetype map (see :help aerial-filetype-map)
  -- To see all available values, see :help SymbolKind
  filter_kind = false,

  keymaps = {
    ["<C-v>"] = "actions.jump_vsplit",
    ["<C-x>"] = "actions.jump_split",
  },
  layout = {
    min_width = 20,

    -- Determines the default direction to open the aerial window. The 'prefer'
    -- options will open the window in the other direction *if* there is a
    -- different buffer in the way of the preferred direction
    -- Enum: prefer_right, prefer_left, right, left, float
    default_direction = "left",
  },

  -- Use symbol tree for folding. Set to true or false to enable/disable
  -- Set to "auto" to manage folds if your previous foldmethod was 'manual'
  -- This can be a filetype map (see :help aerial-filetype-map)
  manage_folds = false,

  -- optionally use on_attach to set keymaps when aerial has attached to a buffer
  on_attach = function(bufnr)
    -- Jump forwards/backwards with "{" and "}"
    vim.keymap.set("n", "{", "<cmd>AerialPrev<CR>", { buffer = bufnr })
    vim.keymap.set("n", "}", "<cmd>AerialNext<CR>", { buffer = bufnr })
  end,

  -- Automatically open aerial when entering supported buffers.
  -- This can be a function (see :help aerial-open-automatic)
  open_automatic = function(bufnr)
    -- bufnr 0 (which should mean the alternate buffer of the current buffer) is
    -- often seen here but I think that's because the correct bufnr isn't being
    -- obtained and 0 is instead being used to signify "current buffer".
    local bufnr = vim.api.nvim_get_current_buf()

    debug_print([[

    ]])
    debug_print("after/plugin/aerial.lua > open_automatic function called")
    debug_print("bufnr: " .. bufnr)
    local was_closed_value = aerial.was_closed(bufnr, nil)
    debug_print(util.serialize_table(was_closed_value))
    debug_print("is_ignored_buf: " .. tostring(require("aerial.util").is_ignored_buf(bufnr)))
    debug_print("was_closed_value: " .. tostring(was_closed_value))
    debug_print("will return: " .. tostring(not require("aerial.util").is_ignored_buf(bufnr)
      and not was_closed_value))

    return not require("aerial.util").is_ignored_buf(bufnr)
      and not was_closed_value
  end,
  -- open_automatic = false,

  -- Run this command after jumping to a symbol (false will disable)
  post_jump_cmd = "normal! zt",

  post_parse_symbol = post_parse_symbol,

  -- Show box drawing characters for the tree hierarchy
  show_guides = true,
})

vim.keymap.set("n", "<leader>oo", function()
  aerial.toggle(vim.api.nvim_get_current_buf())
end, { desc = "Toggle symbol outline" })

vim.keymap.set("n", "go", function()
  aerial.open(vim.api.nvim_get_current_buf(), { focus = true })
end, { desc = "Go to symbol outline" })

vim.keymap.set("n", "<leader>oc", function()
  aerial.close(vim.api.nvim_get_current_buf())
end, { desc = "Close symbol outline" })
