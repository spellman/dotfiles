return {
  {
    "saghen/blink.cmp",
    opts = function(_, opts)
      -- NOTE: A PR is queued to fix the following: https://github.com/LazyVim/LazyVim/pull/6183
      -- HACK: https://github.com/LazyVim/LazyVim/issues/6185
      -- super-tab disappeared in blink.cmp v1.4.0.
      -- The table that LazyVim’s blink extra consults to pull the preset was
      -- renamed in that release, so the lookup at
      -- lazyvim/plugins/extras/coding/blink.lua:112 now returns nil, raising
      -- `attempt to index field 'super-tab' (a nil value)`
      opts.keymap.preset = nil -- Don’t look for a preset.
      opts.keymap["<Tab>"] = {
        -- super-tab logic from Blink’s docs
        function(cmp)
          if cmp.snippet_active() then
            return cmp.accept()
          else
            return cmp.select_and_accept()
          end
        end,
        require("lazyvim.util").cmp.map({ "snippet_forward", "ai_accept" }),
        "fallback",
      }
      opts.keymap["<S-Tab>"] = { "snippet_backward", "fallback" }

      -- (optional) Stop the menu popping up while you’re jumping through a snippet.
      opts.completion = {
        trigger = { show_in_snippet = false },
      }

      opts.signature = { enabled = true }

      -- Add rounded borders to completion and signature windows
      opts.completion = opts.completion or {}
      opts.completion.menu = opts.completion.menu or {}
      opts.completion.menu.border = "rounded"
      
      opts.completion.documentation = opts.completion.documentation or {}
      opts.completion.documentation.window = opts.completion.documentation.window or {}
      opts.completion.documentation.window.border = "rounded"

      opts.signature.window = opts.signature.window or {}
      opts.signature.window.border = "rounded"

      opts.cmdline = {
        keymap = {
          -- Recommended, as the default keymap will only show and select the next item.
          ["<Tab>"] = { "show", "accept" },
        },
        completion = {
          menu = { auto_show = true },
          ghost_text = { enabled = false },
        },
      }
    end,
  },
}
