-- Lua filter to dynamically generate a "Recent" section with publications from the last 6 months
-- This filter runs during Quarto rendering

-- Function to parse year from publication citation
function extract_year(text)
  if not text then return nil end

  -- Look for year pattern: ". YYYY. '" (period, space, 4-digit year, period, space, quote)
  -- This is the standard format in the publications
  local year = text:match("%. (%d%d%d%d)%. '")
  if year then
    return tonumber(year)
  end

  -- Alternative pattern: ", YYYY. '" (comma, space, year)
  year = text:match(", (%d%d%d%d)%. '")
  if year then
    return tonumber(year)
  end

  return nil
end

-- Calculate cutoff date (6 months ago)
-- For publications, we'll use a simpler year-based approach
function get_cutoff_year()
  local now = os.time()
  local date_table = os.date("*t", now)

  -- If we're in the first half of the year, include previous year
  -- If we're in the second half, only include current year
  if date_table.month <= 6 then
    return date_table.year - 1
  else
    return date_table.year
  end
end

-- Check if publication year is recent (within last ~6 months)
function is_recent(year)
  if not year then return false end

  local cutoff = get_cutoff_year()
  return year >= cutoff
end

-- Extract year from list item
function extract_year_from_item(item)
  local text = pandoc.utils.stringify(item)
  return extract_year(text)
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

    -- Process paragraphs (publications are in paragraphs, not bullet lists)
    if block.t == "Para" and inside_section then
      local text = pandoc.utils.stringify(block)
      local year = extract_year(text)

      if year and is_recent(year) then
        table.insert(recent_items, {
          content = block,
          year = year
        })
      end
    end
  end

  -- If we found recent items, add a Recent section at the top
  if #recent_items > 0 then
    -- Sort by year (newest first)
    table.sort(recent_items, function(a, b)
      return a.year > b.year
    end)

    -- Build the Recent section
    local new_blocks = {}

    -- Add Recent header
    table.insert(new_blocks, pandoc.Header(3, "Recent"))

    -- Add explanation
    local current_year = os.date("*t").year
    local cutoff = get_cutoff_year()
    table.insert(new_blocks, pandoc.Para({
      pandoc.Emph("Publications from " .. cutoff .. " onwards")
    }))

    -- Add the items
    for _, item in ipairs(recent_items) do
      table.insert(new_blocks, item.content)
      -- Add a blank line between items
      table.insert(new_blocks, pandoc.Para({}))
    end

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
