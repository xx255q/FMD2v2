----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}


----------------------------------------------------------------------------------------------------
-- Scripting Parameters
----------------------------------------------------------------------------------------------------

-- local LuaDebug   = require 'LuaDebugging'
-- LuaDebugging  = true   --> Override the global LuaDebugging variable by uncommenting this line.
-- LuaStatistics = true   --> Override the global LuaStatistics variable by uncommenting this line.


----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/list?sortType=created'
DirectoryParameters = '&offset='
DirectoryOffset     = 70


----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function _M.GetInfo()
  local rtitle = ''
  local x, v = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetInfo', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  rtitle              = x.XPathString('//h1[@class="NAMES"]/span[@class="name"]')
  MANGAINFO.Title     = x.XPathString('//h1[@class="NAMES"]/span[@class="eng-name"]')
  MANGAINFO.CoverLink = x.XPathString('//div[@class="picture-fotorama"]/img/@src')
  MANGAINFO.Authors   = x.XPathStringAll('//p[@class="elementList"]/span[contains(@class, "elem_author")]/a[@class="person-link"]/text()|//p[@class="elementList"]/span[contains(@class, "elem_screenwriter")]/a[@class="person-link"]/text()')
  MANGAINFO.Artists   = x.XPathStringAll('//p[@class="elementList"]/span[contains(@class, "elem_illustrator")]/a[@class="person-link"]/text()')
  MANGAINFO.Genres    = x.XPathStringAll('//p[@class="elementList"]/span[contains(@class, "elem_genre")]/a/text()|//p[@class="elementList"]/span[contains(@class, "elem_tag")]/a/text()')
  MANGAINFO.Summary   = x.XPathString('//div[@class="manga-description"]')
  
  if MANGAINFO.Title == '' then MANGAINFO.Title = rtitle end
  if Pos('продолжается', x.XPathString('//*[starts-with(@class,"subject-meta")]/*[starts-with(.,"Перевод:")]')) > 0 then MANGAINFO.Status = 1 else MANGAINFO.Status = 0 end
  
  v = x.XPath('//table[@class="table table-hover"]/tbody/tr/td/a')
  for i = 1, v.Count do
    MANGAINFO.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
    MANGAINFO.ChapterNames.Add(v.Get(i).ToString():gsub(rtitle, ''))
  end
  InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
  
  --[[Debug]] LuaDebug.PrintMangaInfo()
  --[[Debug]] LuaDebug.WriteStatistics('Chapters', MANGAINFO.ChapterLinks.Count .. '  (' .. MANGAINFO.Title .. ')')
  
  return no_error
end


-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
  local u = MODULE.RootURL .. DirectoryPagination
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetDirectoryPageNumber', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).XPathString('(//span[@class="pagination"])[last()]/a[@class="step"][last()]')) or 1
  
  --[[Debug]] LuaDebug.WriteStatistics('DirectoryPages', PAGENUMBER)
  
  return no_error
end


-- Get LINKS and NAMES from the manga list of the current website.
function _M.GetNameAndLink()
  local v, x = nil
  local u = MODULE.RootURL .. DirectoryPagination
  
  if URL ~= '0' then u = u .. DirectoryParameters .. (DirectoryOffset * tonumber(URL)) end
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetNameAndLink', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  x.XPathHREFAll('//div[@class="tiles row"]//div[@class="desc"]/h3/a', LINKS, NAMES)
  
  --[[Debug]] LuaDebug.PrintMangaDirectoryEntries(IncStr(URL))
  
  return no_error
end


-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local json, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)
  
  if Pos('mtr=1', URL) == 0 then u = u .. '?mtr=1' end
  
  --[[Debug]] LuaDebug.WriteLogWithHeader('GetPageNumber', 'URL ->  ' .. u)
  if not HTTP.GET(u) then return net_problem end
  
  x = TXQuery.Create(HTTP.Document)
  json = GetBetween('[[', ', 0, ', Trim(GetBetween('rm_h.init(', 'false);', x.XPathString('//script[@type="text/javascript" and contains(., "rm_h.init")]'))))
  json = json:gsub('%],%[', ';'):gsub('\'', ''):gsub('"', ''):gsub(']]', ';')
  for i in json:gmatch('(.-);') do
    i1, i2 = i:match('(.-),.-,(.-),.-,.-')
    TASK.PageLinks.Add(i1 .. i2)
  end
  
  --[[Debug]] LuaDebug.PrintChapterPageLinks()
  --[[Debug]] LuaDebug.WriteStatistics('ChapterPages', TASK.PageLinks.Count .. '  (' .. u .. ')')
  
  return no_error
end


----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
