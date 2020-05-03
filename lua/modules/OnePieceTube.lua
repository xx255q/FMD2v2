----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

-- local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/'   --> Override template variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  -- [Debug] LuaDebug.WriteLogWithHeader('GetInfo', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  MANGAINFO.Title     = x.XPathString('//div[@id="breadcrumbs"]/substring-before(substring-after(., "Start"), "|")'):gsub('^%s*(.-)%s*$', '%1')
  MANGAINFO.Summary   = x.XPathString('//table[@class="infobox"]//tr[6]//td[2]')

  local v for _,v in ipairs(x.XPathI('//table[@class="list"]//tr[./td/@onclick|./td[2]]')) do
    MANGAINFO.ChapterNames.Add(x.XPathString('string-join((td[1],td[2])," ")', v))
    if MANGAINFO.Title == "Kapitel" then
      -- remove last /1 for quick getimageurl later
      MANGAINFO.ChapterLinks.Add(x.XPathString('td[@onclick]/substring-before(substring-after(@onclick, "\'"),"\'")', v):gsub('/1$',''))
    else
      -- remove last /1 for quick getimageurl later
      MANGAINFO.ChapterLinks.Add(x.XPathString('substring-before(substring-after(@onclick, "\'"),"\'")', v):gsub('/1$',''))
    end
  end

  -- [Debug] LuaDebug.PrintMangaInfo()
  -- [Debug] LuaDebug.WriteStatistics('Chapters', MANGAINFO.ChapterLinks.Count .. '  (' .. MANGAINFO.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  return no_error
end


-- Get LINKS and NAMES from the manga list of the current website.
-- DirectoryPagination = RootURL + Manga List
function GetNameAndLink()
  local v, x = nil
  local u = MODULE.RootURL .. DirectoryPagination
  
  -- [Debug] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'URL ->  ' .. u)

  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  v = x.XPath('//div[@id="mangalist"]//a[not(@id="SpinOffOpen")]')

  for i = 1, v.Count do
    LINKS.Add(MODULE.RootURL .. x.XPathString('@href', v.Get(i)))
    NAMES.Add(x.XPathString('text()', v.Get(i)))
  end

  -- [Debug] LuaDebug.PrintMangaDirectoryEntries(u)
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL) .. '/1') then
    x=TXQuery.Create(HTTP.Document)
    -- get total page number
    TASK.PageNumber = tonumber(x.XPathString('//td[@id="tablecontrols"]/a[last()]')) or 0
    -- first page image URL
    TASK.PageLinks.Add(x.XPathString('//img[@id="p"]/@src'))
    return true
  else
    return false
  end
end

function GetImageURL()
  if HTTP.GET(AppendURLDelim(MaybeFillHost(MODULE.RootURL, URL)) .. IncStr(WORKID)) then
    TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//img[@id="p"]/@src')
    return true
  else
    return false
  end
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  local m = NewWebsiteModule()
  m.ID                       = '4c3fb549e0de4a1cbad85869d3d79ef7'
  m.Name                     = 'OnePiece-Tube'
  m.RootURL                  = 'https://onepiece-tube.com'
  m.Category                 = 'German'
  m.OnGetInfo                = 'GetInfo'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  m.OnGetImageURL            = 'GetImageURL'
end