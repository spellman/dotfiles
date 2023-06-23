-- We listed telescope as a dependency for "neovim/nvim-lspconfig" (and before mason, in case that matters) so that it will be available here.
local validate = vim.validate
local util = require("vim.lsp.util")
local telescope_builtin = require("telescope.builtin")

-- I want to be able to compose LSP operations with editor operations (and,
-- really, with anything else).
-- The low-level LSP interactions are requests and responses that are either
-- * Asynchronous and accept callback functions, or
-- * Synchronous and return results
-- However, the functionality above that, up to the user-facing functions, are
-- fire-and-forget, which doesn't compose.
-- As such, I re-implement the relevant functionality on top of the low-level
-- LSP interactions:

---@private
--- Sends an async request to all active clients attached to the current
--- buffer.
---
---@param method (string) LSP method name
---@param params (table|nil) Parameters to send to the server
---@param handler (function|nil) See |lsp-handler|. Follows |lsp-handler-resolution|
--
---@returns 2-tuple:
---  - Map of client-id:request-id pairs for all successful requests.
---  - Function which can be used to cancel all the requests. You could instead
---    iterate all clients and call their `cancel_request()` methods.
---
---@see |vim.lsp.buf_request()|
local function request(method, params, handler)
  validate({
    method = { method, 's' },
    handler = { handler, 'f', true },
  })
  return vim.lsp.buf_request(0, method, params, handler)
end

local function request_with_options(name, params, options)
  local req_handler

  if options then
    local callback

    if options["callback"] then
      callback = options["callback"]
      options["callback"] = nil
    end

    req_handler = function(err, result, ctx, config)
      local client = vim.lsp.get_client_by_id(ctx.client_id)
      local handler = client.handlers[name] or vim.lsp.handlers[name]
      handler(err, result, ctx, vim.tbl_extend('force', config or {}, options))
      if callback then
        callback(err, result, ctx, config)
      end
    end
  end

  request(name, params, req_handler)
end

--- Version of built-in function that takes an optional callback.
--- Jumps to the definition of the symbol under the cursor.
---
---@param options table|nil additional options
---     - reuse_win: (boolean) Jump to existing window if buffer is already open.
---     - on_list: (function) handler for list results. See |lsp-on-list-handler|
---     - callback: (function) function of err, result, ctx, config arguments to
--                  execute after the LSP response.
local function definition(options)
  local params = util.make_position_params()
  request_with_options("textDocument/definition", params, options)
end

--- Version of built-in function that takes an optional callback.
--- Jumps to the declaration of the symbol under the cursor.
---@note Many servers do not implement this method. Generally, see |vim.lsp.buf.definition()| instead.
---
---@param options table|nil additional options
---     - reuse_win: (boolean) Jump to existing window if buffer is already open.
---     - on_list: (function) handler for list results. See |lsp-on-list-handler|
---     - callback: (function) function of err, result, ctx, config arguments to
--                  execute after the LSP response.
local function declaration(options)
  local params = util.make_position_params()
  request_with_options("textDocument/declaration", params, options)
end

local function in_split(fn)
  return function()
    vim.cmd("vsplit")
    fn()
  end
end

local function position_cursor_at_top()
  vim.cmd("norm! zt")
end

local function on_attach(client, bufnr)
  local function opts(description)
    return { buffer = bufnr, remap = false, desc = description }
  end

  vim.keymap.set("n", "gd", function()
    definition({ callback = position_cursor_at_top })
  end, opts("Goto definition"))
  vim.keymap.set("n", "gD", function()
    declaration({ callback = position_cursor_at_top })
  end, opts("Goto declaration"))
  vim.keymap.set("n", "gR", vim.lsp.buf.references, opts("Goto references"))
  vim.keymap.set("n", "gr", telescope_builtin.lsp_references, opts("Goto references with Telescope"))
  vim.keymap.set("n", "gI", vim.lsp.buf.implementation, opts("Goto implementation"))

  vim.keymap.set("n", "gsd", in_split(function()
    definition({ callback = position_cursor_at_top })
  end), opts("Goto definition in split"))
  vim.keymap.set("n", "gsD", in_split(function()
    declaration({ callback = position_cursor_at_top })
  end), opts("Goto declaration in split"))
  vim.keymap.set("n", "gsR", in_split(vim.lsp.buf.references), opts("Goto references in split"))
  vim.keymap.set("n", "gsr", in_split(telescope_builtin.lsp_references), opts("Goto references with Telescope in split"))
  vim.keymap.set("n", "gsI", in_split(vim.lsp.buf.implementation), opts("Goto implementation in split"))

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts("Hover documentation"))
  vim.keymap.set({ "n", "i" }, "<M-k>", vim.lsp.buf.signature_help, opts("Signature documentation"))

  vim.keymap.set("n", "gtd", function()
    type_definition({ callback = position_cursor_at_top })
  end, opts("Goto type definition"))
  vim.keymap.set("n", "gstd", in_split(function()
    type_definition({ callback = position_cursor_at_top })
  end), opts("Goto type definition in split"))

  vim.keymap.set("n", "<leader>ds", telescope_builtin.lsp_document_symbols, opts("Document symbols"))
  vim.keymap.set("n", "<leader>ws", telescope_builtin.lsp_dynamic_workspace_symbols,
    opts("Workspace symbols with Telescope"))

  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts("Code action"))
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts("Rename symbol"))

  -- Lesser used LSP functionality
  vim.keymap.set("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, opts("Workspace add folder"))
  vim.keymap.set("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, opts("Workspace remove folder"))
  vim.keymap.set("n", "<leader>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, opts("Workspace list folders"))

  vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format, opts("Format buffer"))

  -- Diagnostic keymaps
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts("Go to previous diagnostic message"))
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts("Go to next diagnostic message"))
  vim.keymap.set("n", "<D-f>", vim.diagnostic.open_float, opts("Open floating diagnostic message"))
  vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts("Open diagnostics in location list"))
end

local function server_dependent_on_attach(server_name)
  return function(client, bufnr)
    if server_name == "ruff-lsp" then
      -- Disable hover in favor of Pyright
      client.server_capabilities.hoverProvider = false
    end

    on_attach(client, bufnr)
  end
end

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
local servers = {
  -- clangd = {},
  clojure_lsp = {},
  gopls = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
  pyright = {},
  -- I could also use lsp-zero, which sets up linting as well as diagnostics.
  ruff_lsp = {},
  rust_analyzer = {},
  -- tsserver = {},
}

-- Setup neovim lua configuration
require("neodev").setup()

-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

-- Setup mason so it can manage external tooling
require("mason").setup()

-- Ensure the servers above are installed
local mason_lspconfig = require("mason-lspconfig")

mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(servers),
})

mason_lspconfig.setup_handlers({
  function(server_name)
    require("lspconfig")[server_name].setup({
      capabilities = capabilities,
      on_attach = server_dependent_on_attach(server_name),
      settings = servers[server_name],
    })
  end,
})

