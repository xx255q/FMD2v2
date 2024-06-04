----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'H-Sites'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.OnGetImageURL            = 'GetImageURL'
		m.SortedList               = true
	end
	AddWebsiteModule('093e8ef9ad3542f68bbfd7aec0652773', 'NHentaiCom', 'https://nhentai.com')
	AddWebsiteModule('0052cb4aabe0443ca0c97e1eb217728a', 'HentaiHand', 'https://hentaihand.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/api/comics?page='
DirectoryParameters = '&lang=en&q=&sort=uploaded_at&order=desc&duration=all'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local u = MODULE.RootURL .. DirectoryPagination .. '1'

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).last_page')) or 1

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local data, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for data in x.XPath('json(*).data()').Get() do
		LINKS.Add('/comic/' .. x.XPathString('slug', data))
		NAMES.Add(x.XPathString('title', data))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MODULE.RootURL .. '/api/comics/' .. URL:match('/comic/(.+)')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = x.XPathString('json(*).thumb_url')
	MANGAINFO.Authors   = x.XPathStringAll('json(*).authors().name')
	MANGAINFO.Artists   = x.XPathStringAll('json(*).artists().name')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')

	MANGAINFO.ChapterLinks.Add(URL)
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MODULE.RootURL .. '/api/comics/' .. URL:match('/comic/(.+)') .. '/images'

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('json(*).images().source_url', TASK.PageLinks)

	return no_error
end