-- local nvim_metals_group = vim.api.nvim_create_augroup("nvim-metals", { clear = true })
--
-- vim.api.nvim_create_autocmd("FileType", {
--   pattern = { "scala", "sbt" },
--   callback = function()
--     -- TODO: Set up metals info in status bar.
--     -- See GETTING STARTED section in :help metals-commands
--     require("metals").initialize_or_attach({})
--   end,
--   group = nvim_metals_group,
-- })
