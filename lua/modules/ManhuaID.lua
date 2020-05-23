----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryParameters = '/lists'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//title/text()'):gsub('Bahasa Indonesia', ''):gsub('ManhuaID.com', ''):gsub('-', '')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "col-md-4")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//th[contains(., "Author(s)")]/following-sibling::td'):gsub(',', ', ')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//th[contains(., "Status")]/following-sibling::td/span'))
	MANGAINFO.Genres    = x.XPathStringAll('//span[contains(@class, "badge badge-info mr-1 mb-1")]')
	MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "mb-4")]//p[2]')
	x.XPathHREFAll('//table[contains(@class,"table-striped")]//tr/td[1]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	x.XPathHREFAll('//div[@class="col-md-12"]//div[@class="col-md"]/a', LINKS, NAMES)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	x.XPathStringAll('//div[@class="col-md-12"]/img/@src', TASK.PageLinks)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '0d5cc79d4dd442cb9cadc133c241d937'
	m.Name                     = 'ManhuaID'
	m.RootURL                  = 'https://manhuaid.com'
	m.Category                 = 'Webcomics'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end
