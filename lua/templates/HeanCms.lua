----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

API_URL = ''
DirectoryPagination = '/query?series_type=Comic&order=asc&perPage=100&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local v, x = nil
	if not HTTP.GET(API_URL .. DirectoryPagination .. (URL + 1)) then return net_problem end

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
	local chapters, i, j, thumbnail, title, seasons, series, v, x = nil
	local season_prefix = ''
	local u = API_URL .. '/series/' .. URL:match('/.-/(.-)$')

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('json(*).title')
	MANGAINFO.Authors   = x.XPathString('json(*).author')
	MANGAINFO.Genres    = x.XPathStringAll('json(*).tags().name')
	MANGAINFO.Summary   = x.XPathString('json(*).description')
	thumbnail = x.XPathString('json(*).thumbnail')
	if string.find(thumbnail, 'media', 1, true) == nil then
		thumbnail = API_URL .. '/' .. thumbnail
	end
	MANGAINFO.CoverLink = thumbnail

	series = '/' .. x.XPathString('json(*).series_slug') .. '/'
	seasons = x.XPath('json(*).seasons()')
	for i = 1, seasons.Count do
		if (seasons.Count) > 1 then season_prefix = 'S' .. x.XPathString('json(*).seasons()[' .. i .. '].index') .. ' - ' end
		chapters = x.XPath('json(*).seasons()[' .. i .. '].chapters()')
		for j = 1, chapters.Count do
			v = chapters.Get(j)

			title = x.XPathString('chapter_title', v)
			-- Empty title if null:
			if title == 'null' then title = '' end
			-- Add prefix to title if it's not empty:
			if title ~= '' then title = ' - ' .. title end

			MANGAINFO.ChapterLinks.Add('chapter' .. series .. x.XPathString('chapter_slug', v))
			MANGAINFO.ChapterNames.Add(season_prefix .. x.XPathString('chapter_name', v) .. title)
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local image, v, x = nil
	if not HTTP.GET(API_URL .. URL) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('json(*).data()').Get() do
		image = v.ToString()
		if string.find(image, 'media', 1, true) == nil then
			image = API_URL .. '/' .. image
		end
		TASK.PageLinks.Add(image)
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
