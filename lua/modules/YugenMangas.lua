----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'Spanish'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('ac42a85566244b7e836679491ce679e6', 'YugenMangas', 'https://yugenmangas.lat')
	AddWebsiteModule('ac42a85566244b7e836679491ce679ey', 'Punuprojects', 'https://punuprojects.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local API_URL = 'https://api.yugenmangas.net'

function getapi(website)
	local apis  = {
		['YugenMangas']        = API_URL,
		['Punuprojects']       = 'https://api.punuprojects.com'
	}
	if apis[website] ~= nil then
		return apis[website]
	else
		return API_URL
	end
end

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(getapi(MODULE.Name) .. '/query?series_type=Comic&order=asc&perPage=100' .. '&page=' .. (URL + 1)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. x.XPathString('series_slug', v))
		NAMES.Add(x.XPathString('title', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('json(*).meta.last_page_url'):match('/?page=(%d+)')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local u = getapi(MODULE.Name) .. '/series/' .. URL:match('/series/(.-)$')
	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.CoverLink = x.XPathString('json(*).thumbnail')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	local series = '/' .. x.XPathString('json(*).series_slug') .. '/'
	local v for v in x.XPath('json(*).seasons().chapters()').Get() do
		MANGAINFO.ChapterLinks.Add('chapter' .. series .. x.XPathString('chapter_slug', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('chapter_name', v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(getapi(MODULE.Name) .. URL) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('json(*).data()').Get() do
		TASK.PageLinks.Add(v.ToString())
	end

	return no_error
end