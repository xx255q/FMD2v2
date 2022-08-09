----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '92ade0c9487d4667ad81c33bd7fcbd35'
	m.Name                     = 'Koushoku'
	m.RootURL                  = 'https://koushoku.org'
	m.Category                 = 'H-Sites'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetImageURL            = 'GetImageURL'
	m.MaxTaskLimit             = 1
	m.MaxConnectionLimit       = 1
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	if not HTTP.GET(MODULE.RootURL .. '/?sort=published_at') then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//nav[@class="pagination"]//li[last()]/a/@href[1]'):match('?page=(%d+)&sort='))

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. '/?page=' .. (URL + 1) .. '&sort=published_at') then return net_problem end

	CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="entries"]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//img[contains(@alt, "Thumbnail")]/@src')
	MANGAINFO.Artists   = x.XPathStringAll('//tr[./td="Artist"]//a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="tags"]/a')

	MANGAINFO.ChapterLinks.Add(URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[@class="preview"]//a/@href', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])) then
		TASK.PageLinks[WORKID] = MaybeFillHost(MODULE.RootURL, CreateTXQuery(HTTP.Document).XPathString('//div[@class="main"]//img/@src'))
		return true
	end

	return false
end
