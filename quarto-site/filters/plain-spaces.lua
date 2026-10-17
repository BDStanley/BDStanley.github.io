-- Pandoc's `smart` extension joins a common abbreviation ("ed.", "eds.",
-- "et al.") to the next word with a non-breaking space. The Calepin site keeps
-- an ordinary space there, so turn those back into spaces and let lines wrap at
-- the same words as on the live site (it matters in the Publications list).
local NBSP = "\u{a0}"

function Str(el)
  if not el.text:find(NBSP, 1, true) then
    return nil
  end
  local inlines = pandoc.List()
  local rest = el.text
  while true do
    local i = rest:find(NBSP, 1, true)
    if not i then break end
    if i > 1 then inlines:insert(pandoc.Str(rest:sub(1, i - 1))) end
    inlines:insert(pandoc.Space())
    rest = rest:sub(i + #NBSP)
  end
  if #rest > 0 then inlines:insert(pandoc.Str(rest)) end
  return inlines
end
