return {
  "nvim-lualine/lualine.nvim",
  opts = function(_, opts)
    return {
      options = {
        disabled_filetypes = {
          statusline = {
            "qf", -- quickfix and location list buffers
            "NeogitCommitView",
            "NeogitStatus",
            "TelescopePrompt",
            "undotree",

            -- LazyVim stuff
            "dashboard",
            "alpha",
            "ministarter",
            "snacks_dashboard",
          },
          winbar = {
            "qf", -- quickfix and location list buffers
            "NeogitCommitView",
            "NeogitStatus",
            "TelescopePrompt",
            "undotree",
          },
        },
        icons_enabled = true,
        theme = "auto",
        component_separators = "|",
        section_separators = "|",
      },
      -- Determines the sections of the status line for active buffers.
      sections = {
        lualine_a = { LazyVim.lualine.pretty_path() },
        lualine_b = { "location" },
        lualine_c = { "progress" },
        lualine_x = {
          Snacks.profiler.status(),
          {
            function()
              return "  " .. require("dap").status()
            end,
            cond = function()
              return package.loaded["dap"] and require("dap").status() ~= ""
            end,
            color = function()
              return { fg = Snacks.util.color("Debug") }
            end,
          },
        },
        lualine_y = { "diagnostics" },
        lualine_z = { "branch" },
      },
      -- Determines the sections of the status line for inactive buffers.
      inactive_sections = {
        lualine_a = { LazyVim.lualine.pretty_path() },
        lualine_b = { "location" },
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = { "branch" },
      },

      extensions = { "neo-tree", "lazy", "fzf" },
    }
  end,
}
