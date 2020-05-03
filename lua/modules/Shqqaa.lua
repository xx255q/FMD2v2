----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

-- local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga/'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  MANGAINFO.Title     = x.XPathString('//ul[@class="list-group"]//li[2]/small')
  MANGAINFO.CoverLink = x.XPathString('//img[contains(@class, "img-thumbnail")]/@data-src')
  MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="list-group"]//li[5]//span'), 'مستمر', 'منتهي');
  MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "card-body")]/p')
  
  x.XPathHREFAll('//div[@class="text-center"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
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
  x.XPathHREFAll('//div[@class="card-body p-2"]/h6/a', LINKS, NAMES)
  
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
  x.XPathStringAll('//div[@class="img-manga"]/img/@src', TASK.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', TASK.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  local m = NewWebsiteModule()
  m.ID                       = '686b0e24c30443aca38ddb22ed578cb7'
  m.Name                     = 'Shqqaa'
  m.RootURL                  = 'https://www.shqqaa.com'
  m.Category                 = 'Arabic'
  m.OnGetInfo                = 'GetInfo'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
end