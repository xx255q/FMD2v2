----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'ac42a85566244b7e836679491ce679e6'
	m.Name                     = 'YugenMangas'
	m.RootURL                  = 'https://yugenmangas.com'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = 'https://api.yugenmangas.com'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	HTTP.MimeType = 'application/json'
	if not HTTP.POST(API_URL .. '/series/querysearch', '{"order":"asc", "type":"Comic"}') then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*)()').Get() do
		LINKS.Add('series/' .. x.XPathString('series_slug', v))
		NAMES.Add(x.XPathString('title', v))
	end

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = API_URL .. '/' .. x.XPathString('json(*).thumbnail')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	local v for v in x.XPath('json(*).chapters()').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('id', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('chapter_name', v))
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(API_URL .. '/series/chapter' .. URL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).content.images()').Get() do
		TASK.PageLinks.Add(API_URL .. '/' .. v.ToString())
	end

	return no_error
end
