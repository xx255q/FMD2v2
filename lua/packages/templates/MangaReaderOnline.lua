----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryParameters = '/changeMangaList?type=text'
XPathTokenStatus    = 'Status'
XPathTokenAuthors   = 'Author(s)'
XPathTokenArtists   = 'Artist(s)'
XPathTokenGenres    = 'Categories'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function _M.GetInfo()
  local v, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)

  if not HTTP.GET(u) then return net_problem end

  x = CreateTXQuery(HTTP.Document)
  MANGAINFO.Title     = x.XPathString('(//div[contains(@class, "container")]//h2)[1]')
  MANGAINFO.CoverLink = x.XPathString('//div[@class="boxed"]/img/@src')
  MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]/span'), 'Ongoing', 'Complete')
  MANGAINFO.Authors   = x.XPathStringAll('//dt[text()="' .. XPathTokenAuthors .. '"]/following-sibling::dd[1]/a')
  MANGAINFO.Artists   = x.XPathStringAll('//dt[text()="' .. XPathTokenArtists .. '"]/following-sibling::dd[1]/a')
  MANGAINFO.Genres    = x.XPathStringAll('//dt[text()="' .. XPathTokenGenres .. '"]/following-sibling::dd[1]/a')
  MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "well")]/p')

  v = x.XPath('//ul[@class="chapters"]/li/h5')
  for i = 1, v.Count do
    MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v.Get(i)))
    MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(.)', v.Get(i)))
  end
  InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

  return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function _M.GetNameAndLink()
  local v, x = nil
  local u = MODULE.RootURL .. DirectoryParameters

  if not HTTP.GET(u) then return net_problem end

  x = CreateTXQuery(HTTP.Document)
  x.XPathHREFAll('//li/a', LINKS, NAMES)

  return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
  local s, x = nil
  local u = MaybeFillHost(MODULE.RootURL, URL)

  if not HTTP.GET(u) then return net_problem end

  x = CreateTXQuery(HTTP.Document)
  x.XPathStringAll('//div[@id="all"]//img/@data-src', TASK.PageLinks)
  if TASK.PageLinks.Count == 0 then
    x.XPathStringAll('//div[@id="all"]//img/@src', TASK.PageLinks)
  end

  return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M