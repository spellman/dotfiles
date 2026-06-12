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
	launchOrFocusOrRotate("org.gnu.Emacs")
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

hs.hotkey.bind({"ctrl", "cmd"}, "e", function()
	local focusedWindow = hs.window.focusedWindow()
	local dir = nil

	if focusedWindow then
		local app = focusedWindow:application()
		if app and app:bundleID() == "com.mitchellh.ghostty" then
			dir = ghosttyCwd()
		end
	end

	if dir then
		dir = dir:gsub("\\", "\\\\"):gsub('"', '\\"')
		local elisp = '(dired "' .. dir .. '")'
		hs.task.new("/opt/homebrew/bin/emacsclient", nil, {"--no-wait", "--create-frame", "--eval", elisp}):start()
	else
		hs.task.new("/opt/homebrew/bin/emacsclient", nil, {"--no-wait", "--create-frame"}):start()
	end
end)
