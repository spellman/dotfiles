-- Command-line access to this Hammerspoon instance (the `hs` CLI), so
-- bindings can be exercised and debugged from a terminal.
require("hs.ipc")

local hyper = { "ctrl", "alt", "cmd" }
local shift_hyper = { "ctrl", "alt", "cmd", "shift" }

local function appPathToAppBundleID(app)
	if hs.application.infoForBundlePath(app) then
		return hs.application.infoForBundlePath(app)["CFBundleIdentifier"]
	end
end

local function launchOrFocusOrRotate(appBundleID)
	local focusedWindow = hs.window.focusedWindow()

	if focusedWindow and focusedWindow:application():bundleID() == appBundleID then
		-- If already focused.
		local appWindows = hs.application.get(appBundleID):allWindows()

		if #appWindows > 0 then
			-- It seems that this list order changes after one window get focus.
			-- Let's directly bring the last one to focus every time
			-- https://www.hammerspoon.org/docs/hs.window.html#focus
			if appBundleID == "com.apple.finder" then
				-- For the Finder app the window count is one more than the on-screen
				-- count so we subtract 1.
				appWindows[#appWindows - 1]:focus()
			else
				appWindows[#appWindows]:focus()
			end
		else
			-- This case should not occur because the app was found to be focused.
			-- Therefore, the number of app windows should be > 0.
			hs.application.launchOrFocusByBundleID(appBundleID)
		end
	else
		-- If not focused.
		hs.application.launchOrFocusByBundleID(appBundleID)
	end
end

hs.hotkey.bind(hyper, "b", function()
	-- launchOrFocusOrRotate("app.zen-browser.zen")
	-- launchOrFocusOrRotate("com.apple.Safari")
	-- launchOrFocusOrRotate("com.vivaldi.Vivaldi")
	launchOrFocusOrRotate("company.thebrowser.Browser") -- Arc browser
end)

hs.hotkey.bind(hyper, "c", function()
	launchOrFocusOrRotate("com.apple.iCal")
end)

hs.hotkey.bind(hyper, "d", function()
	launchOrFocusOrRotate("com.electron.dockerdesktop")
end)

hs.hotkey.bind(hyper, "e", function()
	local app = hs.application.get("org.gnu.Emacs")
	if app and #app:allWindows() > 0 then
		launchOrFocusOrRotate("org.gnu.Emacs")
	else
		hs.execute(
			"/opt/homebrew/bin/emacsclient --alternate-editor='' --create-frame --no-wait 2>&1",
			true)
	end
end)

hs.hotkey.bind(hyper, "m", function()
	launchOrFocusOrRotate("com.electron.realtimeboard") -- Miro
end)

hs.hotkey.bind(hyper, "p", function()
	launchOrFocusOrRotate("com.1password.1password")
end)

hs.hotkey.bind(hyper, "s", function()
	launchOrFocusOrRotate("com.tinyspeck.slackmacgap") -- Slack
end)

hs.hotkey.bind(hyper, "t", function()
	-- launchOrFocusOrRotate("com.googlecode.iterm2")
	launchOrFocusOrRotate("com.mitchellh.ghostty")
end)

hs.hotkey.bind(hyper, "v", function()
	launchOrFocusOrRotate("com.microsoft.VSCode")
end)

hs.hotkey.bind(hyper, "z", function()
	launchOrFocusOrRotate("us.zoom.xos")
end)

local function ghosttyCwd()
	local ok, output = hs.osascript.applescript(
		'tell application "Ghostty" to get working directory of focused terminal of selected tab of front window'
	)
	if ok and output and output ~= "" then
		return output
	end
	return nil
end

local function shellQuote(s)
	return "'" .. s:gsub("'", "'\\''") .. "'"
end

local function emacsProjectRoot()
	-- The Emacs frame still holds OS focus when the hotkey fires, so ask for
	-- the focused frame directly; fall back to the tracked frame, then to
	-- selected-frame. Ghostty does not expand "~", so the root must be
	-- returned as an absolute path.
	local elisp = "(let* ((f (or (seq-find #'frame-focus-state (frame-list))" ..
		" (and cws/last-focused-frame (frame-live-p cws/last-focused-frame) cws/last-focused-frame)" ..
		" (selected-frame)))" ..
		" (buf (window-buffer (frame-selected-window f))))" ..
		" (with-current-buffer buf" ..
		" (let ((p (project-current))) (when p (expand-file-name (project-root p))))))"
	local output, status = hs.execute(
		"/opt/homebrew/bin/emacsclient --eval " .. shellQuote(elisp) .. " 2>&1",
		true
	)
	if not status then
		hs.alert.show("emacsclient error: " .. (output or "no output"))
		return nil
	end
	-- The interactive login shell prints rc-file noise (zle warnings, iTerm2
	-- escape sequences, some without trailing newlines) before emacsclient's
	-- result, so take the quoted absolute path at the END of the output.
	if output then
		local path = output:match('"(/[^"\n]*)"%s*$')
		if path then
			return path
		end
	end
	return nil
end

local function openGhosttyWindow(dir)
	local ok, result, descriptor
	if dir then
		dir = dir:gsub('\\', '\\\\'):gsub('"', '\\"')
		ok, result, descriptor = hs.osascript.applescript(string.format(
			'tell application "Ghostty"\n' ..
			'  activate\n' ..
			'  new window with configuration {initial working directory:"%s"}\n' ..
			'end tell',
			dir
		))
	else
		ok, result, descriptor = hs.osascript.applescript(
			'tell application "Ghostty"\n' ..
			'  activate\n' ..
			'  new window\n' ..
			'end tell'
		)
	end
	if not ok then
		hs.alert.show("Ghostty new window failed: " .. hs.inspect(descriptor or result))
	end
end

-- Globals so the pieces can be exercised from the `hs` CLI.
cws = {
	ghosttyCwd = ghosttyCwd,
	emacsProjectRoot = emacsProjectRoot,
	openGhosttyWindow = openGhosttyWindow,
}

hs.hotkey.bind({"ctrl", "cmd"}, "e", function()
	local focusedWindow = hs.window.focusedWindow()
	local dir = nil

	if focusedWindow then
		local app = focusedWindow:application()
		if app and app:bundleID() == "com.mitchellh.ghostty" then
			dir = ghosttyCwd()
		end
	end

	-- Visiting a directory with emacsclient opens it in dired, so the new
	-- frame lands in dired at dir with no --eval needed.
	local cmd = "/opt/homebrew/bin/emacsclient --no-wait --create-frame"
	if dir then
		cmd = cmd .. " " .. shellQuote(dir)
	end
	local output, status = hs.execute(cmd .. " 2>&1", true)
	if not status then
		hs.alert.show("emacsclient error: " .. (output or "no output"))
	end
end)

hs.hotkey.bind({"ctrl", "cmd"}, "t", function()
	local focusedWindow = hs.window.focusedWindow()
	local dir = nil

	if focusedWindow then
		local app = focusedWindow:application()
		local bundleID = app and app:bundleID()

		if bundleID == "com.mitchellh.ghostty" then
			dir = ghosttyCwd()
		elseif bundleID == "org.gnu.Emacs" then
			dir = emacsProjectRoot()
		end
	end

	openGhosttyWindow(dir)
end)
