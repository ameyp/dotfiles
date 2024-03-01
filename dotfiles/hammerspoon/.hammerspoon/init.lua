local gridset = function(x)
   return function()
      hs.grid.set(hs.window.focusedWindow(),
		  hs.geometry.rect(x, 0.0, 1.0, 3.0))
   end
end

local gridshow = function()
  hs.grid.HINTS = {{
    "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9", "f10"
  }, {
    "1", "2", "3", "4", "5", "6", "7", "8", "9", "0"
  }, {
    "'", ",", ".", "P", "Y", "F", "G", "C", "R", "L"
  }, {
    "A", "O", "E", "U", "I", "D", "H", "T", "N", "S"
  }, {
    ";", "Q", "J", "K", "X", "B", "M", "W", "V", "Z"
  }}
  hs.grid.show()
end

hs.grid.setMargins(hs.geometry(nil,nil,1,1))
local mash = {"cmd", "alt"}
hs.hotkey.bind(mash, "1", gridset(0.0))
hs.hotkey.bind(mash, "2", gridset(1.0))
hs.hotkey.bind(mash, "3", gridset(2.0))
hs.hotkey.bind(mash, "W", gridshow)
