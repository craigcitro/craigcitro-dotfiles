on run argv
  set theUrl to item 1 of argv
  -- set theUrl to "http://news.ycombinator.com/"
  set useHome to (count argv) > 1
  -- set useHome to true
  tell application "Google Chrome"
    set winList to {}
    repeat with aWindow in windows
      if (presenting of aWindow) then
	set end of winList to aWindow
      end if
    end repeat
    set theWindow to (item 1 in winList)
    if (count winList) > 1 then
      set foundHome to (index of (item 1 of winList)) < (index of (item 2 of winList))
      if (foundHome and not useHome) or (not foundHome and useHome) then
	set theWindow to (item 2 in winList)
      end if
    end if
    tell theWindow
      make new tab with properties {URL:theUrl}
    end tell
  end tell
end run
