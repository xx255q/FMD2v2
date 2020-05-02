----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

-- local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/backend/ajax/searchengine.php'
local DirectoryParameters = 'contentType=manga&retrieveCategories=true&retrieveAuthors=true&perPage=18&page='


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  local c, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  MANGAINFO.URL       = x.XPathString('//meta[@property="og:URL"]/@content')
  MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content')
  MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
  MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Estado")]'), 'Activo', 'Finalizado')
  MANGAINFO.Authors   = x.XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Autor") and not(contains(.,"No Disponible"))]/substring-after(normalize-space(.),": ")')
  MANGAINFO.Artists   = x.XPathString('//*[@class="infom"]/div/div[2]//p[contains(.,"Artist") and not(contains(.,"No Disponible"))]/substring-after(normalize-space(.),": ")')
  MANGAINFO.Genres    = x.XPathString('//*[@class="panel-footer" and contains(.,"Géneros")]/string-join(.//a,", ")')
  MANGAINFO.Summary   = x.XPathString('//meta[@property="og:description"]/@content')
  
  x.XPathHREFAll('//table[contains(@class, "table")]//h4[@class="title"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
  c = GetPageChapterListPageCount(x.XPathString('//script[contains(., "php_pagination")]'))
  
  if c > 1 then
    for i = 2, c do
      if HTTP.GET(MANGAINFO.URL .. '/p/' .. i) then
        x = TXQuery.Create(HTTP.Document)
        x.XPathHREFAll('//table[contains(@class, "table")]//h4[@class="title"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
      end
    end
  end
  
  InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', MANGAINFO.ChapterLinks.Count .. '  (' .. MANGAINFO.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  local u = MODULE.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'URL ->  ' .. u .. '   (Params: [' .. DirectoryParameters .. '])')
  if not HTTP.POST(u, DirectoryParameters .. '1') then return net_problem end
  
  PAGENUMBER = math.ceil(tonumber(TXQuery.Create(HTTP.Document).XPathString('json(*).totalContents')) / 18)
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', PAGENUMBER)
  
  return no_error
end


-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
  local c, x = nil
  local u = MODULE.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'URL ->  ' .. u .. '   (Params: [' .. DirectoryParameters .. '])')
  if not HTTP.POST(u, DirectoryParameters .. IncStr(URL)) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  c = x.XPathCount('json(*).contents()')
  
  for i = 1, c do
    NAMES.Add(x.XPathString('json(*).contents(' .. i .. ').name'))
    LINKS.Add(x.XPathString('json(*).contents(' .. i .. ')/concat("/manga/",id,"/",slug)'))
  end
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(DirectoryParameters .. IncStr(URL))
  
  return no_error
end


-- Get the page count for the current chapter.
function GetPageNumber()
  local c, p, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL:gsub('/c/', '/leer/'))
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  c = tonumber(x.XPathString('//script[contains(., "konekomangareader")]'):match('\'pages\':(.-),'))
  p = x.XPathString('//script[contains(., "konekomangareader")]'):match('\'pageFormat\':\'(.-)\',')
  
  for i = 1, c do
    TASK.PageLinks.Add(p:gsub('{pnumber}', tostring(i)))
  end
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', TASK.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get count of chapter list pages.
function GetPageChapterListPageCount(s)
  local count = 1
  
  s = GetBetween('php_pagination(', ');', s)
  count = math.ceil(tonumber(s:match('.-,.-,.-,.-,(.-),.-,.-')) / tonumber(s:match('.-,.-,.-,.-,.-,(.-),.-')))
  
  return count
end


----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  local m = NewWebsiteModule()
  m.ID                       = '6138f1a985cd47c3b60a65cf6b1fe03d'
  m.Name                     = 'KuManga'
  m.RootURL                  = 'http://www.kumanga.com'
  m.Category                 = 'Spanish'
  m.OnGetInfo                = 'GetInfo'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
end