-- We listed telescope as a dependency for "neovim/nvim-lspconfig" (and before mason, in case that matters) so that it will be available here.
local builtin = require("telescope.builtin")

local on_attach = function(_, bufnr)
  local opts = function (description)
    return { buffer = bufnr, remap = false, desc = description }
  end

  vim.keymap.set("n", "gd", vim.lsp.buf.definition, opts("[g]oto [d]efinition"))
  vim.keymap.set("n", "gD", vim.lsp.buf.declaration, opts("[g]oto [D]eclaration"))
  vim.keymap.set("n", "gR", vim.lsp.buf.references, opts("[g]oto [R]eferences"))
  vim.keymap.set("n", "gr", builtin.lsp_references, opts("Telescope LSP [g]oto [R]eferences"))
  vim.keymap.set("n", "gI", vim.lsp.buf.implementation, opts("[g]oto [i]mplementation"))

  vim.keymap.set("n", "K", vim.lsp.buf.hover, opts("Hover Documentation"))
  vim.keymap.set({"n", "i"}, "<M-k>", vim.lsp.buf.signature_help, opts("Signature Documentation"))

  vim.keymap.set("n", "gtd", vim.lsp.buf.type_definition, opts("[g]oto [t]ype [D]efinition"))
  vim.keymap.set("n", "<leader>ds", builtin.lsp_document_symbols, opts("[d]ocument [s]ymbols"))
  vim.keymap.set("n", "<leader>ws", builtin.lsp_dynamic_workspace_symbols, opts("Telescope LSP [w]orkspace [S]ymbols"))

  vim.keymap.set("n", "<leader>ca", vim.lsp.buf.code_action, opts("[c]ode [a]ction"))
  vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, opts("[r]e[n]ame"))

  -- Lesser used LSP functionality
  vim.keymap.set("n", "<leader>wa", vim.lsp.buf.add_workspace_folder, opts("[w]orkspace [a]dd Folder"))
  vim.keymap.set("n", "<leader>wr", vim.lsp.buf.remove_workspace_folder, opts("[w]orkspace [r]emove Folder"))
  vim.keymap.set("n", "<leader>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, opts("[w]orkspace [l]ist Folders"))

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
    vim.lsp.buf.format()
  end, { desc = "Format current buffer with LSP" })
  vim.keymap.set("n", "<leader>f", vim.lsp.buf.format, opts("[f]ormat buffer"))

  -- Diagnostic keymaps
  vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, opts("Go to previous diagnostic message"))
  vim.keymap.set("n", "]d", vim.diagnostic.goto_next, opts("Go to next diagnostic message"))
  vim.keymap.set("n", "<D-f>", vim.diagnostic.open_float, opts("Open floating diagnostic message"))
  vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, opts("Open diagnostics in location list"))
end

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.
local servers = {
  -- clangd = {},
  clojure_lsp = {},
  gopls = {},
  pyright = {},
  rust_analyzer = {},
  -- tsserver = {},

  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
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
      on_attach = on_attach,
      settings = servers[server_name],
    })
  end,
})

