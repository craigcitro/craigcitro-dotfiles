function Set(t)
   local s = {}
   for a,b in pairs(t) do print(a, b) end
   for _,v in pairs(t) do s[v] = true end
   return s
end

function set_contains(t, e)
   return t[e] or false
end

-- The set of global shortcuts we don't want to swap cmd/alt. For these,
-- we really only want to include the things that are just too hardwired
-- to want to switch for alt.

global_excludes = Set{
                       -- "alt-cmd-left",
                       "cmd-tab",
                       "shift-cmd-tab",
	               "cmd-h",
                     }

-- The set of apps we want to consider swapping keys for, with some
-- notable exclusions. The exclusion means that a "cmd-w" will do the
-- normal OS Terminal behaviour. If you omit items then you would
-- have to use "alt-w" to close a terminal window.

chrome_swaps = Set{
  "cmd-v",
  "cmd-w",
  "cmd-z", "cmd-b", "cmd-f",
  "cmd-/", "cmd-5", "cmd-t",
  "shift-cmd-.", "shift-cmd-,",
}

-- Return true to swap cmd/alt, otherwise false.

-- This function is passed a table comprising the following keys:
--
--   key_str_seq        key sequence (e.g., "shift-cmd-e")
--   alt                true if the alt key was pressed
--   fn                 true if the fn key was pressed
--   control            true if the control key was pressed
--   shift              true if the shift key was pressed
--   cmd                true if the command key was pressed
--   keycode            numeric virtual keycode (e.g., 48)
--   appname            the frontmost application (e.g., Terminal)
--
-- The order of the modifier keys in key-str-eq is always:
--   shift control alt cmd fn, separated by a hyphen ("-").

function swap_keys(t)
   if set_contains(global_excludes, t.key_str_seq) then
      return false
   end
   if not (t.appname == "Google Chrome") then
      return false
   end
   if set_contains(chrome_swaps, t.key_str_seq) or
      set_contains(chrome_swaps, t.key_str_seq:gsub("alt%-", "cmd-")) then
      return true
   end
   return false
end
