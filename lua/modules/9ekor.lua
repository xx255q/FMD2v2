----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

-- local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/daftar-isi/'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local pages, v, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  local p = 1
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  MANGAINFO.Title     = Trim(x.XPathString('//title/substring-after(substring-before(., "Bahasa Indonesia - 9ekor"), "Baca")'))
  
  pages = tonumber(x.XPathString('//div[@class="pagination"]/a[last()]/@href[not(parent::span/@id="tie-next-page")]'):match('/page/(%d+)/'))
  if pages == nil then pages = 1 end
  while true do
    x.XPathHREFAll('//div[contains(@class, "post-listing")]//h2[@class="post-box-title"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    p = p + 1
    if p > pages then
      break
    elseif HTTP.GET(u .. 'page/' .. tostring(p) .. '/') then
      x = TXQuery.Create(HTTP.Document)
    else
      break
    end
  end
  InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', MANGAINFO.ChapterLinks.Count .. '  (' .. MANGAINFO.Title .. ')')
  
  return no_error
end


-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
  local x = nil
  local u = MODULE.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  x.XPathHREFAll('//a[contains(@class, "cvplbd")]', LINKS, NAMES)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(1)
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  x.XPathStringAll('//div[@id="all"]/img/@src', TASK.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', TASK.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  local m = NewWebsiteModule()
  m.ID                       = '09c45865698743908b84ecadc71e278c'
  m.Name                     = '9ekor'
  m.RootURL                  = 'https://9ekor.com'
  m.Category                 = 'Indonesian'
  m.OnGetInfo                = 'GetInfo'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
end
