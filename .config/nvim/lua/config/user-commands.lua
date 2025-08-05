vim.api.nvim_create_user_command("FormatJSON", function()
  local start_line, end_line
  local mode = vim.fn.mode()

  -- Determine whether we're in a visual mode
  local is_visual = mode == "v" or mode == "V" or mode == "\22"

  -- Get the lines to process
  if is_visual then
    -- Get selected range
    start_line = vim.fn.line("'<")
    end_line = vim.fn.line("'>")
  else
    -- Entire buffer
    start_line = 1
    end_line = vim.fn.line("$")
  end

  -- Extract the target lines
  local lines = vim.api.nvim_buf_get_lines(0, start_line - 1, end_line, false)
  local json_input = table.concat(lines, "\n")

  -- Run jq safely
  local stdout = vim.fn.systemlist("jq .", json_input)
  local exit_code = vim.v.shell_error

  if exit_code == 0 then
    -- Replace lines only if jq succeeded
    local formatted = stdout
    vim.api.nvim_buf_set_lines(0, start_line - 1, end_line, false, formatted)
  else
    -- Show error, do not change buffer
    local err = table.concat(stdout, "\n")
    vim.notify("jq error:\n" .. err, vim.log.levels.ERROR)
  end
end, {
  desc = "Safely format JSON using jq (uses visual selection or full buffer)",
})
