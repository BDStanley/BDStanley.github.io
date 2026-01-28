-- Lua filter to dynamically generate a "Recent" section with media from the last 6 months
-- This filter runs during Quarto rendering

-- Function to parse date from DD.MM.YYYY or DD/MM/YYYY format
function parse_date(date_str)
  if not date_str then return nil end

  -- Try format: DD.MM.YYYY
  local day, month, year = date_str:match("(%d+)%.(%d+)%.(%d%d%d%d)")
  if day and month and year then
    return os.time({year=tonumber(year), month=tonumber(month), day=tonumber(day)})
  end

  -- Try format: DD/MM/YYYY
  day, month, year = date_str:match("(%d+)/(%d+)/(%d%d%d%d)")
  if day and month and year then
    return os.time({year=tonumber(year), month=tonumber(month), day=tonumber(day)})
  end

  return nil
end

-- Calculate cutoff date (6 months ago)
function get_cutoff_date()
  local now = os.time()
  local date_table = os.date("*t", now)

  -- Subtract 6 months
  date_table.month = date_table.month - 6

  -- Handle year rollover
  while date_table.month <= 0 do
    date_table.month = date_table.month + 12
    date_table.year = date_table.year - 1
  end

  return os.time(date_table)
end

-- Check if date string is within last 6 months
function is_recent(date_str)
  local item_date = parse_date(date_str)
  if not item_date then return false end

  local cutoff = get_cutoff_date()
  return item_date >= cutoff
end

-- Extract date from list item
function extract_date(item)
  local text = pandoc.utils.stringify(item)

  -- Look for (DD.MM.YYYY) at end
  local date_str = text:match("%((%d+%.%d+%.%d%d%d%d)%)%s*$")
  if date_str then return date_str end

  -- Look for (DD/MM/YYYY) at end
  date_str = text:match("%((%d+/%d+/%d%d%d%d)%)%s*$")
  return date_str
end

-- Main filter function
function Pandoc(doc)
  local recent_items = {}
  local inside_section = false

  -- Scan through document blocks
  for i, block in ipairs(doc.blocks) do
    -- Track when we're inside a section
    if block.t == "Header" then
      inside_section = true
    end

    -- Process bullet lists
    if block.t == "BulletList" and inside_section then
      for _, item in ipairs(block.content) do
        local date_str = extract_date(item)

        if date_str and is_recent(date_str) then
          local timestamp = parse_date(date_str)
          table.insert(recent_items, {
            content = item,
            date = date_str,
            timestamp = timestamp or 0
          })
        end
      end
    end
  end

  -- If we found recent items, add a Recent section at the top
  if #recent_items > 0 then
    -- Sort by date (newest first)
    table.sort(recent_items, function(a, b)
      return a.timestamp > b.timestamp
    end)

    -- Build the Recent section
    local new_blocks = {}

    -- Add Recent header
    table.insert(new_blocks, pandoc.Header(3, "Recent"))

    -- Add explanation
    table.insert(new_blocks, pandoc.Para({
      pandoc.Emph("Media appearances from the last six months")
    }))

    -- Add the items
    local items_list = {}
    for _, item in ipairs(recent_items) do
      table.insert(items_list, item.content)
    end
    table.insert(new_blocks, pandoc.BulletList(items_list))

    -- Add separator
    table.insert(new_blocks, pandoc.HorizontalRule())

    -- Add original blocks
    for _, block in ipairs(doc.blocks) do
      table.insert(new_blocks, block)
    end

    doc.blocks = new_blocks
  end

  return doc
end
