-- Enable requiring Lua files in ./after directory.
package.path = os.getenv("HOME") .. "/.config/nvim/after/?.lua;" .. package.path

require("cort")
