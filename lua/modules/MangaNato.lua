----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fa8bb4d1ceea4c8fa0e98c00755f95d4'
	m.Name                     = 'MangaNato'
	m.RootURL                  = 'https://www.natomanga.com'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnBeforeDownloadImage    = 'BeforeDownloadImage'
	m.SortedList               = true
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaBox'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
end

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	-- Handle potential JavaScript redirect
	local redirect = string.match(HTTP.Document.ToString(), 'window%.location%.assign%([\'"]([^\'"]+)')
	if redirect ~= nil then
		u = redirect
		if not HTTP.GET(u) then return net_problem end
	end

	MANGAINFO.URL = u
	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//ul[@class="manga-info-text"]//h1')
	MANGAINFO.AltTitles = x.XPathString('//ul[@class="manga-info-text"]//h2/substring-after(., "Alternative :")')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="manga-info-pic"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Author")]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Genre")]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info-text"]/li[contains(., "Status")]'))
	MANGAINFO.Summary   = x.XPathStringAll('//div[@id="contentBox"]/substring-after(., "your bookmark.")')

	-- Use more specific selector for chapter list (MangaBat style)
	x.XPathHREFAll('//div[@class="chapter-list"]/div[@class="row"]/span/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/'

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	TASK.PageLinks.Clear()
	TASK.PageNumber = 0
	local u = MaybeFillHost(MODULE.RootURL, URL)

	HTTP.Cookies.Values['content_server'] = 'server2'
	if not HTTP.GET(u) then return net_problem end

	-- Handle potential JavaScript redirect
	local redirect = string.match(HTTP.Document.ToString(), 'window%.location%.assign%([\'"]([^\'"]+)')
	if redirect ~= nil then
		local function splitURL(url) return url:match('(https://[^/]+)') or '' end
		local h, p = splitURL(redirect), splitURL(u)
		if not HTTP.GET(h .. p) then return net_problem end
	end

	local x = CreateTXQuery(HTTP.Document)
	-- Try multiple selectors for better compatibility
	x.XPathStringAll('//div[@class="container-chapter-reader"]/img[@title]/@src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then
		x.XPathStringAll('//div[@id="vungdoc"]/img[@title]/@src', TASK.PageLinks)
		if TASK.PageLinks.Count == 0 then
			x.XPathStringAll('//div[@class="vung_doc"]/img[@title]/@src', TASK.PageLinks)
			if TASK.PageLinks.Count == 0 then
				x.XPathStringAll('//div[@id="vungdoc"]/img[@title]/@data-src', TASK.PageLinks)
			end
		end
	end

	return no_error
end

-- Prepare the URL, http header and/or http cookies before downloading an image.
function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])

	return true
end