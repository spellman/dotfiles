local autopairs = require("nvim-autopairs")
local Rule = require("nvim-autopairs.rule")
local ts_conds = require("nvim-autopairs.ts-conds")

autopairs.setup({
  check_ts = true,
})

autopairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))

-- Prevent single quote from being matched in these filetypes.
for idx, rule in ipairs(autopairs.get_rules("'")) do
  rule.not_filetypes = { "clojure", "lisp", "scala", "scheme" }
end

for idx, rule in ipairs(autopairs.get_rules("(")) do
  rule.not_filetypes = { "clojure", "lisp", "scheme" }
end

for idx, rule in ipairs(autopairs.get_rules("[")) do
  rule.not_filetypes = { "clojure" }
end

for idx, rule in ipairs(autopairs.get_rules("{")) do
  rule.not_filetypes = { "clojure" }
end
