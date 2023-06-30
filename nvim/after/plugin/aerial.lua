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

require("aerial").setup({
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
  -- TODO: This doesn't seem to actually be "open automatically on entering".
  -- Rather, it seems to actually be "open automatically on entering and when
  -- various events occur so that manual toggling off is defeated within a
  -- short amount of time."
  -- Can I change the events that trigger automatic opening or otherwise make it
  -- do what it claims to do?
  open_automatic = false,

  -- Run this command after jumping to a symbol (false will disable)
  post_jump_cmd = "normal! zt",

  post_parse_symbol = post_parse_symbol,

  -- Show box drawing characters for the tree hierarchy
  show_guides = true,
})

-- You probably also want to set a keymap to toggle aerial
vim.keymap.set("n", "<leader>a", "<cmd>AerialToggle!<CR>")
