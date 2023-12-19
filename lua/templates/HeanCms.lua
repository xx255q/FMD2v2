----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = ''

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, x = nil
	if not HTTP.GET(API_URL .. '/query?series_type=Comic&order=asc&perPage=100' .. '&page=' .. (URL + 1)) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		LINKS.Add('series/' .. x.XPathString('series_slug', v))
		NAMES.Add(x.XPathString('title', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('json(*).meta.last_page_url'):match('/?page=(%d+)')) or 1

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local thumbnail, title, series, v, x = nil
	local u = API_URL .. '/series/' .. URL:match('/series/(.-)$')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	thumbnail		= x.XPathString('json(*).thumbnail')
	if string.find(thumbnail, 'media', 1, true) == nil then
		thumbnail = API_URL .. '/' .. thumbnail
	end
	MANGAINFO.CoverLink = thumbnail
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')

	series = '/' .. x.XPathString('json(*).series_slug') .. '/'
	for v in x.XPath('json(*).seasons().chapters()').Get() do
		title = x.XPathString('chapter_title', v)

		-- Empty title if null:
		if title == '' or title == nil or title == 'null' then title = '' end
		-- Add prefix to title if it's not empty:
		if title ~= '' then title = ' - ' .. title end

		MANGAINFO.ChapterLinks.Add('chapter' .. series .. x.XPathString('chapter_slug', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('chapter_name', v) .. title)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local v, x = nil
	if not HTTP.GET(API_URL .. URL) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		TASK.PageLinks.Add(v.ToString())
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
