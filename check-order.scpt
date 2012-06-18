set out to ""
tell application "Google Chrome"
  repeat with aWindow in windows
    if (presenting of aWindow) then
      if (length of out) > 0 then
	set out to out & "\n"
      end if
      set myTitle to (title of aWindow as text)
      if (length of myTitle) > 70 then
        set myTitle to (text 1 through 67 of myTitle) & "..."
      end if
      set msg to (index of aWindow as text) & ": " & myTitle
      set out to out & msg
    end if
  end repeat
end tell
out
