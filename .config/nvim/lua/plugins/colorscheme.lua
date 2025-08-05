return {
  -- {
  --   "projekt0n/github-nvim-theme",
  --   name = "github-theme",
  --   lazy = false, -- make sure we load this during startup if it is your main colorscheme
  --   priority = 1000, -- make sure to load this before all the other start plugins
  -- },

  -- {
  --   "rmehri01/onenord.nvim",
  --   lazy = false,
  --   priority = 1000,
  --   config = function()
  --     require("onenord").setup({
  --       borders = true,
  --       fade_nc = false,
  --       -- any other OneNord options…
  --     })
  --   end,
  -- },

  -- {
  --   "sainnhe/everforest",
  --   lazy = false,
  --   priority = 1000,
  --   config = function()
  --     -- Optionally configure and load the colorscheme
  --     -- directly inside the plugin declaration.
  --     vim.g.everforest_enable_italic = true
  --     vim.g.everforest_background = "hard"
  --     vim.cmd.colorscheme("everforest")
  --   end,
  -- },

  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
      style = "day",
      on_colors = function(colors)
        -- Make background much lighter (closer to github_light_default)
        colors.bg = "#ffffff"  -- Pure white background instead of gray
        colors.bg_dark = "#f6f8fa"  -- Very light gray for sidebars
        colors.bg_float = "#ffffff"  -- White for floating windows
        
        -- Change base text color from dark blue to black
        colors.fg = "#24292f"  -- Dark gray/black for main text
        colors.fg_dark = "#24292f"  -- Consistent text color
      end,
      on_highlights = function(highlights, colors)
        -- Make current line highlighting extremely subtle
        highlights.CursorLine = {
          bg = "#fafbfc",  -- Barely perceptible light gray
        }
        
        -- Ensure text remains black on the cursor line
        highlights.CursorLineNr = {
          fg = "#24292f",  -- Black line number on cursor line
          bg = "#fafbfc",  -- Match cursor line background
        }
        
        -- Make line numbers lighter but visible
        highlights.LineNr = {
          fg = "#656d76",  -- Medium gray for line numbers
        }
        
        -- Ensure good contrast for selection
        highlights.Visual = {
          bg = "#dbeafe",  -- Light blue selection background
        }
        
        -- Fix vim-illuminate highlighting to stand out with color alone
        highlights.IlluminatedWordText = {
          bg = "#ddeeff",  -- More visible light blue background
          -- No fg override to preserve syntax highlighting
        }
        highlights.IlluminatedWordRead = {
          bg = "#d6f4ff",  -- More distinct light blue for read references
          -- No fg override to preserve syntax highlighting
        }
        highlights.IlluminatedWordWrite = {
          bg = "#ffe6e6",  -- More distinct light red for write references
          -- No fg override to preserve syntax highlighting
        }
        
        -- Make which-key popup lighter to match the theme
        highlights.WhichKey = {
          fg = "#24292f",  -- Dark gray/black for keys
          bg = "#ffffff",  -- Pure white background
        }
        highlights.WhichKeyDesc = {
          fg = "#656d76",  -- Medium gray for descriptions
          bg = "#ffffff",  -- Pure white background
        }
        highlights.WhichKeyGroup = {
          fg = "#0366d6",  -- Blue for group names
          bg = "#ffffff",  -- Pure white background
        }
        -- WhichKeyBorder uses default border color (keeping original)
        
        -- Fix additional which-key background areas
        highlights.WhichKeySeparator = {
          fg = "#656d76",  -- Medium gray for separators/arrows
          bg = "#ffffff",  -- Pure white background
        }
        highlights.WhichKeyIcon = {
          bg = "#ffffff",  -- Pure white background behind icons
        }
        highlights.WhichKeyFloat = {
          bg = "#ffffff",  -- Pure white background for the entire popup
        }
        highlights.WhichKeyNormal = {
          fg = "#24292f",  -- Dark gray/black text
          bg = "#ffffff",  -- Pure white background
        }
      end,
    },
  },

  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "tokyonight-day",
    },
  },
}
