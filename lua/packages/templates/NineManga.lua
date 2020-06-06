----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/category/index_'
DirectorySuffix     = '.html'
MangaInfoParameters = '?waring=1'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function _M.GetInfo()
  local s, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL) .. MangaInfoParameters

  if not HTTP.GET(u) then return net_problem end

  x = CreateTXQuery(HTTP.Document)
  MANGAINFO.Title     = x.XPathString('//div[@class="manga"]/div[@class="ttline"]/h1')
  MANGAINFO.CoverLink = x.XPathString('//a[@class="bookface"]/img/@src')
  MANGAINFO.Authors   = x.XPathString('//ul[@class="message"]/li[starts-with(.,"Author")]/string-join(a,", ")')
  MANGAINFO.Genres    = x.XPathString('//ul[@class="message"]/li[starts-with(.,"Genre")]/string-join(a,", ")')
  MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="message"]/li[starts-with(.,"Status")]/a[1]'));
  MANGAINFO.Summary   = Trim(x.XPathString('//p[@itemprop="description"]/substring-after(.,":")'))

  s = MANGAINFO.Title:match('^(.*) Manga$')
  if s ~= nil and s ~= '' then MANGAINFO.Title = s end

  x.XPathHREFAll('//div[@class="chapterbox"]//li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
  InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

  return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function _M.GetNameAndLink()
  local x = nil
  local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectorySuffix

  if not HTTP.GET(u) then return net_problem end

  x = CreateTXQuery(HTTP.Document)
  x.XPathHREFAll('//dl[@class="bookinfo"]//dd/a[@class="bookname"]', LINKS, NAMES)

  if tonumber((URL + 1)) < tonumber(x.XPathString('(//ul[@class="pagelist"]/li[last()-1])[1]')) then
    UPDATELIST.CurrentDirectoryPageNumber = tonumber((URL + 1)) + 1
  end

  return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)

  if not HTTP.GET(u) then return net_problem end

  x = CreateTXQuery(HTTP.Document)
  x.XPathStringAll('(//select[@id="page"])[last()]/option/@value', TASK.PageContainerLinks)
  TASK.PageNumber = TASK.PageContainerLinks.Count

  return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
  local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

  if HTTP.GET(u) then
    TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class,"manga_pic")]/@src')
    return true
  end

  return false
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M