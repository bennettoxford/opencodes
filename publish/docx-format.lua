-- docx-format.lua
--
-- Fixes image cropping when DOCX files are opened in Google Docs.
--
-- What happens: Quarto wraps every fig- labelled figure in a single-column
-- table (via pandoc.SimpleTable in main.lua). Pandoc writes that table with
-- w:tblLayout type="fixed" in the OOXML, which Google Docs treats as a hard
-- clip boundary. On top of that, chunk-level fig-width (e.g. 10 in) sets an
-- explicit width on the image and cell, and Quarto's page width defaults to
-- 6.5 in (US Letter hardcoded), so the column ends up narrower than the image
-- on A4. Result: right side of the image gets cut off.
--
-- The fix: user Lua filters run after Quarto's internal renderers, so by the
-- time this file runs, FloatRefTarget nodes are already Tables. We find these
-- figure-wrapper tables (one body, one row, one cell, cell has an image),
-- pull out the cell contents as plain blocks, and strip the explicit image
-- dimensions. No table = no clipping. No explicit dimensions = Pandoc reads
-- the PNG size and caps it at the reference doc page width (A4 = 6.26 in).
--
-- Note: tbl.colspec is nil for tables created by from_simple_table() in
-- Pandoc 3.8 + Quarto 1.9 (some marshaling quirk). Use row cell count
-- to check for single-column tables instead.


-- Pandoc's docx writer emits one "Author"-styled paragraph per author, so a
-- multi-author manuscript gets a tall stack of names under the title. The
-- reference doc can't fix this (styles can't merge paragraphs), so instead
-- collapse the author list into a single comma-separated metadata entry,
-- which the writer then renders as one Author paragraph.
function Meta(meta)
  if FORMAT ~= "docx" then return nil end
  local authors = meta.author
  if not authors or pandoc.utils.type(authors) ~= "List" or #authors < 2 then
    return nil
  end

  local combined = pandoc.Inlines({})
  for i, author in ipairs(authors) do
    if i > 1 then
      combined:insert(pandoc.Str(","))
      combined:insert(pandoc.Space())
    end
    if pandoc.utils.type(author) == "Inlines" then
      combined:extend(author --[[@as pandoc.Inlines]])
    else
      combined:insert(pandoc.Str(pandoc.utils.stringify(author)))
    end
  end

  meta.author = pandoc.MetaInlines(combined)
  return meta
end


function Header(el)
  if FORMAT == "docx" then
    el.identifier = ""
  end
  return el
end


function Table(tbl)
  if FORMAT ~= "docx" then return nil end

  -- Figure wrapper tables have exactly one body, one row, one cell.
  if not tbl.bodies or #tbl.bodies ~= 1 then return nil end
  local body = tbl.bodies[1]
  if not body.body or #body.body ~= 1 then return nil end
  if body.head and #body.head > 0 then return nil end
  local row = body.body[1]
  if not row.cells or #row.cells ~= 1 then return nil end
  if tbl.head and #tbl.head.rows > 0 then return nil end
  if tbl.foot and #tbl.foot.rows > 0 then return nil end
  -- The figure caption lives inside the cell as a Para with embedded OOXML,
  -- not as a table-level caption, so this should always be empty.
  if tbl.caption and tbl.caption.long and #tbl.caption.long > 0 then return nil end

  local cell = row.cells[1]
  if not cell.contents then return nil end

  -- Only unwrap if the cell actually has an image; leave data tables alone.
  local cell_div = pandoc.Div(cell.contents)
  local has_image = false
  cell_div:walk({ Image = function(_) has_image = true end })
  if not has_image then return nil end

  local unwrapped = cell_div:walk({
    Image = function(img)
      img.attributes["width"]  = nil
      img.attributes["height"] = nil
      return img
    end
  }).content

  return unwrapped
end


-- Figures without a fig- label go through Pandoc's Figure node instead of
-- FloatRefTarget. Same problem, same fix.
function Figure(fig)
  if FORMAT ~= "docx" then return nil end

  local blocks = {}

  for _, block in ipairs(fig.content) do
    if block.t == "Plain" then
      block = pandoc.Para(block.content)
    end
    block = block:walk({
      Image = function(img)
        img.attributes["width"]  = nil
        img.attributes["height"] = nil
        return img
      end
    })
    table.insert(blocks, block)
  end

  local caption_inlines = pandoc.utils.blocks_to_inlines(fig.caption.long)
  if #caption_inlines > 0 then
    table.insert(blocks, pandoc.Div(
      {pandoc.Para(caption_inlines)},
      pandoc.Attr("", {}, {["custom-style"] = "Caption"})
    ))
  end

  return blocks
end
