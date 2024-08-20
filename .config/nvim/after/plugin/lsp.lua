-- UFO provides LSP-based code folds.
-- As per
-- https://github.com/kevinhwang91/nvim-ufo/blob/43e39ec74cd57c45ca9d8229a796750f6083b850/README.md#minimal-configuration
local ufo = require("ufo")
vim.keymap.set('n', 'zR', ufo.openAllFolds)
vim.keymap.set('n', 'zM', ufo.closeAllFolds)

local api = vim.api
local validate = vim.validate
local util = require("vim.lsp.util")

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
local function go_to_definition(options)
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
local function go_to_declaration(options)
  local params = util.make_position_params()
  request_with_options("textDocument/declaration", params, options)
end

--- Jumps to the definition of the type of the symbol under the cursor.
---
---@param options table|nil additional options
---     - reuse_win: (boolean) Jump to existing window if buffer is already open.
---     - on_list: (function) handler for list results. See |lsp-on-list-handler|
---     - callback: (function) function of err, result, ctx, config arguments to
--                  execute after the LSP response.
local function go_to_type_definition(options)
  local params = util.make_position_params()
  request_with_options('textDocument/typeDefinition', params, options)
end

--- Renames all references to the symbol under the cursor.
---
---@param new_name string|nil If not provided, the user will be prompted for a new
---                name using |vim.ui.input()|.
---@param options table|nil additional options
---     - filter (function|nil):
---         Predicate used to filter clients. Receives a client as argument and
---         must return a boolean. Clients matching the predicate are included.
---     - name (string|nil):
---         Restrict clients used for rename to ones where client.name matches
---         this field.
---     - callback: (function) function of err, result, ctx, config arguments to
--                  execute after the LSP response.
local rename = function(new_name, options)
  options = options or {}
  local callback

  if options["callback"] then
    callback = options["callback"]
    options["callback"] = nil
  end

  local bufnr = options.bufnr or api.nvim_get_current_buf()
  local clients = vim.lsp.get_active_clients({
    bufnr = bufnr,
    name = options.name,
  })
  if options.filter then
    clients = vim.tbl_filter(options.filter, clients)
  end

  -- Clients must at least support rename, prepareRename is optional
  clients = vim.tbl_filter(function(client)
    return client.supports_method('textDocument/rename')
  end, clients)

  if #clients == 0 then
    vim.notify('[LSP] Rename, no matching language servers with rename capability.')
  end

  local win = api.nvim_get_current_win()

  -- Compute early to account for cursor movements after going async
  local cword = vim.fn.expand('<cword>')

  ---@private
  local function get_text_at_range(range, offset_encoding)
    return api.nvim_buf_get_text(
      bufnr,
      range.start.line,
      util._get_line_byte_from_position(bufnr, range.start, offset_encoding),
      range['end'].line,
      util._get_line_byte_from_position(bufnr, range['end'], offset_encoding),
      {}
    )[1]
  end

  local try_use_client
  try_use_client = function(idx, client)
    if not client then
      return
    end

    ---@private
    local function rename(name)
      local params = util.make_position_params(win, client.offset_encoding)
      params.newName = name
      local handler = client.handlers['textDocument/rename']
          or vim.lsp.handlers['textDocument/rename']

      -- I think `...` here is err, result, ctx, config
      client.request('textDocument/rename', params, function(...)
        handler(...)
        if callback then
          callback(...)
        end
        try_use_client(next(clients, idx))
      end, bufnr)
    end

    if client.supports_method('textDocument/prepareRename') then
      local params = util.make_position_params(win, client.offset_encoding)
      client.request('textDocument/prepareRename', params, function(err, result)
        if err or result == nil then
          if next(clients, idx) then
            try_use_client(next(clients, idx))
          else
            local msg = err and ('Error on prepareRename: ' .. (err.message or ''))
                or 'Nothing to rename'
            vim.notify(msg, vim.log.levels.INFO)
          end
          return
        end

        if new_name then
          rename(new_name)
          return
        end

        local prompt_opts = {
          prompt = 'New Name: ',
        }
        -- result: Range | { range: Range, placeholder: string }
        if result.placeholder then
          prompt_opts.default = result.placeholder
        elseif result.start then
          prompt_opts.default = get_text_at_range(result, client.offset_encoding)
        elseif result.range then
          prompt_opts.default = get_text_at_range(result.range, client.offset_encoding)
        else
          prompt_opts.default = cword
        end
        vim.ui.input(prompt_opts, function(input)
          if not input or #input == 0 then
            return
          end
          rename(input)
        end)
      end, bufnr)
    else
      assert(
        client.supports_method('textDocument/rename'),
        'Client must support textDocument/rename'
      )
      if new_name then
        rename(new_name)
        return
      end

      local prompt_opts = {
        prompt = 'New Name: ',
        default = cword,
      }
      vim.ui.input(prompt_opts, function(input)
        if not input or #input == 0 then
          return
        end
        rename(input)
      end)
    end
  end

  try_use_client(next(clients))
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

local function write_changed_buffers()
  vim.cmd("silent! wa")
end

-- Setup neovim lua configuration
require("neodev").setup()

local function default_capabilities()
  -- nvim-cmp supports additional completion capabilities, so broadcast that to servers
  local capabilities = vim.lsp.protocol.make_client_capabilities()
  capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
  -- Add folding capability as per
  -- https://github.com/kevinhwang91/nvim-ufo/blob/43e39ec74cd57c45ca9d8229a796750f6083b850/README.md#minimal-configuration
  capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true
  }
  return capabilities
end

local utils = require("cort.utils")
local function server_dependent_capabilities(server_name)
  local capabilities = default_capabilities()

  -- Source:
  -- * https://www.reddit.com/r/neovim/comments/179vv49/comment/kbfp7rz/
  -- * https://www.reddit.com/r/neovim/comments/179vv49/comment/kowhsfc/
  if server_name == "gopls" then
    local additional_capabilities = {
      workspace = {
        workspaceFolders = true,
        didChangeWatchedFiles = {
          dynamicRegistration = true,
        }
      }
    }

    return utils.deep_merge_tables(capabilities, additional_capabilities)
  end

  return capabilities
end

-- We listed telescope as a dependency for "neovim/nvim-lspconfig" (and before
-- mason, in case that matters) so that it will be available here.
local telescope_builtin = require("telescope.builtin")

local function on_attach(client, bufnr)
  local function opts(description)
    return { buffer = bufnr, remap = false, desc = description }
  end

  vim.keymap.set("n", "gd", function()
    go_to_definition({
      callback = function(err, result, ctx, config)
        if err ~= nil then
          vim.print(err)
        else
          position_cursor_at_top()
        end
      end
    })
  end, opts("Goto definition"))

  vim.keymap.set("n", "gsd", in_split(function()
    go_to_definition({
      callback = function(err, result, ctx, config)
        if err ~= nil then
          vim.print(err)
        else
          position_cursor_at_top()
        end
      end
    })
  end), opts("Goto definition in split"))

  vim.keymap.set("n", "gD", function()
    go_to_declaration({
      callback = function(err, result, ctx, config)
        if err ~= nil then
          vim.print(err)
        else
          position_cursor_at_top()
        end
      end
    })
  end, opts("Goto declaration"))

  vim.keymap.set("n", "gsD", in_split(function()
    go_to_declaration({
      callback = function(err, result, ctx, config)
        if err ~= nil then
          vim.print(err)
        else
          position_cursor_at_top()
        end
      end
    })
  end), opts("Goto declaration in split"))

  vim.keymap.set("n", "gR", vim.lsp.buf.references, opts("Goto references"))
  vim.keymap.set("n", "gsR", in_split(vim.lsp.buf.references), opts("Goto references in split"))

  vim.keymap.set("n", "gr", function()
    telescope_builtin.lsp_references({
      prompt_title = "References (" .. vim.fn.expand('<cword>') .. ")",
    })
  end, opts("Goto references with Telescope"))
  vim.keymap.set("n", "gsr", in_split(function()
    telescope_builtin.lsp_references({
      prompt_title = "References (" .. vim.fn.expand('<cword>') .. ")",
    })
  end), opts("Goto references with Telescope in split"))

  vim.keymap.set("n", "gI", vim.lsp.buf.implementation, opts("Goto implementation"))
  vim.keymap.set("n", "gsI", in_split(vim.lsp.buf.implementation), opts("Goto implementation in split"))

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts("Hover documentation"))
  vim.keymap.set({ "n", "i" }, "<M-k>", vim.lsp.buf.signature_help, opts("Signature documentation"))

  vim.keymap.set("n", "gp", function()
    go_to_type_definition({
      callback = function(err, result, ctx, config)
        if err ~= nil then
          vim.print(err)
        else
          position_cursor_at_top()
        end
      end
    })
  end, opts("Goto type definition"))

  vim.keymap.set("n", "gsp", in_split(function()
    go_to_type_definition({
      callback = function(err, result, ctx, config)
        if err ~= nil then
          vim.print(err)
        else
          position_cursor_at_top()
        end
      end
    })
  end), opts("Goto type definition in split"))

  vim.keymap.set("n", "<leader>ds", function()
    telescope_builtin.lsp_document_symbols({
      prompt_title = "LSP Document Symbols - " .. vim.fn.expand("%")
    })
  end, opts("Document symbols"))
  vim.keymap.set("n", "<leader>ws", telescope_builtin.lsp_dynamic_workspace_symbols,
    opts("Workspace symbols with Telescope"))

  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts("Code action"))

  vim.keymap.set("n", "<leader>rn", function()
    rename(nil, { callback = write_changed_buffers })
  end, opts("Rename symbol"))

  -- Lesser used LSP functionality
  vim.keymap.set("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, opts("Workspace add folder"))
  vim.keymap.set("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, opts("Workspace remove folder"))
  vim.keymap.set("n", "<leader>wl", function()
    vim.print(vim.lsp.buf.list_workspace_folders())
  end, opts("Workspace list folders"))
  vim.keymap.set("n", "<leader>lI", "<cmd>LspInfo<CR>", opts("LSP info"))

  vim.keymap.set("n", "<leader>lf", vim.lsp.buf.format, opts("Format buffer"))

  -- Diagnostic keymaps
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts("Go to previous diagnostic message"))
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts("Go to next diagnostic message"))
  vim.keymap.set("n", "<D-f>", vim.diagnostic.open_float, opts("Open floating diagnostic message"))
  vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts("Open diagnostics in location list"))
end

-- Source https://github.com/Shopify/ruby-lsp/blob/b6f5f4f0d81a69b626933e2d24983f7b6b61b139/EDITORS.md#Neovim-LSP
local function add_ruby_deps_command(client, bufnr)
  vim.api.nvim_buf_create_user_command(bufnr, "ShowRubyDeps", function(opts)
      local params = vim.lsp.util.make_text_document_params()
      local showAll = opts.args == "all"

      client.request("rubyLsp/workspace/dependencies", params, function(error, result)
        if error then
          print("Error showing deps: " .. error)
          return
        end

        local qf_list = {}
        for _, item in ipairs(result) do
          if showAll or item.dependency then
            table.insert(qf_list, {
              text = string.format("%s (%s) - %s", item.name, item.version, item.dependency),
              filename = item.path
            })
          end
        end

        vim.fn.setqflist(qf_list)
        vim.cmd('copen')
      end, bufnr)
    end,
    { nargs = "?", complete = function() return { "all" } end })
end

local function server_dependent_on_attach(server_name)
  return function(client, bufnr)
    if server_name == "ruby_lsp" then
      add_ruby_deps_command(client, bufnr)
    end

    on_attach(client, bufnr)
  end
end

local function is_conjure_buffer(bufname)
  -- `%` escapes dash and dot characters, as per
  -- https://www.lua.org/pil/20.2.html
  return string.match(bufname, "^conjure%-log%-[0-9]+%.")
end

-- I don't want diagnostics for conjure log buffers. They are for displaying
-- results and docs, not code, so every line results in a diagnostic.
-- Source: https://www.reddit.com/r/neovim/comments/xqogsu/comment/jbyqyoc/
local lsp_handlers = {
  ["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics,
    {
      virtual_text = function(namespace, bufnr)
        return not is_conjure_buffer(vim.fn.bufname(bufnr))
      end,
      underline = function(namespace, bufnr)
        return not is_conjure_buffer(vim.fn.bufname(bufnr))
      end,
      signs = function(namespace, bufnr)
        return not is_conjure_buffer(vim.fn.bufname(bufnr))
      end,
    }
  ),
}

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
local servers = {
  -- clangd = {},
  clojure_lsp = {
    [":keep-parens-when-threading?"] = true
  },
  cssls = {},
  css_variables = {},
  elixirls = {},
  elp = {},
  gopls = {},
  golangci_lint_ls = {
    filetypes = { "go", "gomod" }
  },
  jsonls = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
  marksman = {},
  pyright = {},
  ruby_lsp = {},
  svelte = {},
  -- I could also use lsp-zero, which sets up linting as well as diagnostics.
  tsserver = {},
}

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
      capabilities = server_dependent_capabilities(server_name),
      filetypes = (servers[server_name] or {}).filetypes,
      handlers = lsp_handlers,
      on_attach = server_dependent_on_attach(server_name),
      settings = servers[server_name],
    })
  end,
})

-- Install gleam LSP manually because it isn't in the Mason registry.
-- (https://github.com/mason-org/mason-registry/pull/3872)
require("lspconfig").gleam.setup({
  capabilities = default_capabilities(),
  handlers = lsp_handlers,
  on_attach = function(client, bufnr)
    on_attach(client, bufnr)
  end
})

-- See UFO explanation at top.
-- As per
-- https://github.com/kevinhwang91/nvim-ufo/blob/43e39ec74cd57c45ca9d8229a796750f6083b850/README.md#minimal-configuration
ufo.setup()
