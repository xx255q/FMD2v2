----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

-- local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local AlphaList = '#abcdefghijklmnopqrstuvwxyz'
local DirectoryPagination = '/manga-list/'
local LoadAllImages = '/all-pages'


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local v, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  MANGAINFO.Title     = x.XPathString('(//h5[@class="widget-heading"])[1]')
  MANGAINFO.CoverLink = x.XPathString('//div[@class="content"]//img/@src')
  MANGAINFO.Status    = x.XPathString('//dt[contains(., "Status")]/following-sibling::dd[1]')
  MANGAINFO.Authors   = x.XPathString('//dt[contains(., "Author")]/following-sibling::dd[1]')
  MANGAINFO.Artists   = x.XPathString('//dt[contains(., "Artist")]/following-sibling::dd[1]')
  MANGAINFO.Genres    = x.XPathStringAll('//dt[contains(., "Categories")]/following-sibling::dd[1]/a')
  MANGAINFO.Summary   = Trim(x.XPathString('//div[contains(@class, "note")]'))
  
  v = x.XPath('//div[@id="chapter_list"]/ul/li/a')
  for i = 1, v.Count do
    MANGAINFO.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
    MANGAINFO.ChapterNames.Add(x.XPathString('span[1]', v.Get(i)))
  end
  InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', MANGAINFO.ChapterLinks.Count .. '  (' .. MANGAINFO.Title .. ')')
  
  return no_error
end


-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
  local x = nil
  local u = MODULE.RootURL .. DirectoryPagination .. AlphaList:sub(MODULE.CurrentDirectoryIndex + 1, MODULE.CurrentDirectoryIndex + 1)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  x.XPathHREFAll('//ul[contains(@class, "manga-list")]/li/a', LINKS, NAMES)
  UPDATELIST.CurrentDirectoryPageNumber = 1
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(MODULE.CurrentDirectoryIndex + 1)
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local s, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL) .. LoadAllImages
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  x.ParseHTML(GetBetween('var images = ', ';', x.XPathString('//script[contains(., "var images = ")]')):gsub('\\/', '/'))
  x.XPathStringAll('json(*)().URL', TASK.PageLinks)
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', TASK.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
  HTTP.Headers.Values['referer'] = MODULE.RootURL
  
  return true
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  local m = NewWebsiteModule()
  m.ID                    = 'de4684706931463da794bb45377f4c3e'
  m.Name                  = 'MangaInn'
  m.RootURL               = 'http://www.mangainn.net'
  m.Category              = 'English'
  m.TotalDirectory        = AlphaList:len()
  m.OnGetInfo             = 'GetInfo'
  m.OnGetNameAndLink      = 'GetNameAndLink'
  m.OnGetPageNumber       = 'GetPageNumber'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  return m
end