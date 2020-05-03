----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

-- local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/?page='


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
  MANGAINFO.Title     = x.XPathString('//h1')
  MANGAINFO.CoverLink = x.XPathString('//div[@id="cover"]//img/@data-src')
  if MANGAINFO.CoverLink == '' then MANGAINFO.CoverLink = x.XPathString('//div[@id="cover"]//img/@src') end
  MANGAINFO.Artists   = x.XPathString('//section[@id="tags"]//a[contains(@href, "artist")]/text()')
  MANGAINFO.Genres    = x.XPathStringAll('//section[@id="tags"]//a/text()')
  MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "drop-discription")]/p/text()')
   
  MANGAINFO.ChapterLinks.Add(URL)
  MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', MANGAINFO.ChapterLinks.Count .. '  (' .. MANGAINFO.Title .. ')')

  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local u = MODULE.RootURL .. DirectoryPagination .. 1
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  if MODULE.Name == 'NHentai' then
    PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).XPathString('//a[@class="last"]/@href/substring-after(.,"=")'))
  else
    PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).XPathString('//section[@class="pagination"]/li[last()]/a/@href'):match('?page=(%d+)&order='))
  end
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', PAGENUMBER)
  
  return no_error
end


-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
  local x = nil
  local u = MODULE.RootURL .. DirectoryPagination .. IncStr(URL)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  x.XPathHREFAll('//div[@id="content"]/div/div[@class="gallery"]/a', LINKS, NAMES)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(URL))
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  x.XPathStringAll('//a[@class="gallerythumb"]/@href', TASK.PageContainerLinks)
  TASK.PageNumber = TASK.PageContainerLinks.Count
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', TASK.PageContainerLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
  local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])
  
  if HTTP.GET(u) then
    TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//section[@id="image-container"]//img/@src')
    return true
  end
  
  return false
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  AddWebsiteModule('f8d26ca921af4876b7ba84bd7e06fe82', 'NHentai', 'https://nhentai.net', 'H-Sites')
  AddWebsiteModule('0052cb4aabe0443ca0c97e1eb217728a', 'HentaiHand', 'https://hentaihand.com', 'H-Sites')
end

function AddWebsiteModule(id, name, url, category)
  local m = NewWebsiteModule()
  m.ID                       = id
  m.Name                     = name
  m.RootURL                  = url
  m.Category                 = category
  m.OnGetInfo                = 'GetInfo'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  m.OnGetImageURL            = 'GetImageURL'
  m.SortedList               = True
end
