----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/page/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="entry-wrap"]//a[contains(@href, "/artist/")]')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="entry-wrap"]//a[contains(@href, "/tag/")]')
	MANGAINFO.ChapterLinks.Add(x.XPathString('//a[contains(@title, "Read")]/@href'))
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).XPathString('//a[@class="last" and @title="Last Page"]'))

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. IncStr(URL)

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	x.XPathHREFAll('//h2[@class="entry-title"]/a', LINKS, NAMES)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local s, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	x.ParseHTML(GetBetween('var pages = ', ';', x.XPathString('//script[contains(., "var pages = ")]')):gsub('\\/', '/'))
	x.XPathStringAll('json(*)().URL', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '650591a805214f7787379d582ffbab21'
	m.Name                     = 'HentaiCafe'
	m.RootURL                  = 'https://hentai.cafe'
	m.Category                 = 'H-Sites'
	m.FavoriteAvailable        = false
	m.OnGetInfo                = 'GetInfo'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end