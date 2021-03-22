----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/category/index_'
DirectorySuffix     = '.html'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function _M.GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. 1

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="dis-inline-block para-h8"]')) or 1

  return no_error
end

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectorySuffix

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="book-info"]/a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="bookinfo-info"]/h1')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="bookinfo-pic-img"]/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="bookinfo-author"]/a/span')
	MANGAINFO.Genres    = x.XPathStringAll('(//div[@class="bookinfo-category-list"])[1]/span/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('(//div[contains(@class, "bookinfo-category-list")])[3]/a'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="bookinfo-summary"]/span')

	x.XPathHREFTitleAll('//div[@class="chapter-item-list"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)

	x.XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	if TASK.PageContainerLinks.Count == 0 then
		x.ParseHTML('[' .. GetBetween('all_imgs_url: [', '],', x.XPathString('//script[contains(., "all_imgs_url")]')) .. ']')
		x.XPathStringAll('json(*)()', TASK.PageLinks)
	end

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function _M.GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

	if HTTP.GET(u) then
		TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class, "manga_pic")]/@src')
		return true
	end

	return false
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
