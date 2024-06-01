----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

local _M = {}

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

XPathTokenAuthors = 'Author(s)'
XPathTokenArtists = 'Artist(s)'
XPathTokenGenres  = 'Genre(s)'
XPathTokenStatus  = 'Status'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function _M.GetNameAndLink()
	local x = nil
	local s = 'action=madara_load_more&template=madara-core/content/content-archive&page=' .. URL .. '&vars[paged]=0&vars[post_type]=wp-manga&vars[posts_per_page]=250'
	local u = MODULE.RootURL .. '/wp-admin/admin-ajax.php'
	HTTP.MimeType = 'application/x-www-form-urlencoded'

	if not HTTP.POST(u, s) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	if x.XPath('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a').Count == 0 then return no_error end
	x.XPathHREFAll('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function _M.GetInfo()
	local id, q, status, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="post-title"]/*[self::h1 or self::h3]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@data-src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="author-content"]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[@class="artist-content"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="genres-content"]/a')
	status = x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenStatus .. '")]/following-sibling::div')
	status = status:gsub('Berjalan', 'Ongoing'):gsub('مستمرة', 'Ongoing'):gsub('Tamat', 'Completed'):gsub('مكتملة', 'Completed')
	MANGAINFO.Status    = MangaInfoStatusIfPos(status)
	MANGAINFO.Summary   = x.XPathString('//div[@class="description-summary" or @class="manga-excerpt"]//p')

	if MANGAINFO.CoverLink == '' then MANGAINFO.CoverLink = x.XPathString('//div[@class="summary_image"]//img/@src') end
	if MANGAINFO.Authors == '' then MANGAINFO.Authors = x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenAuthors .. '")]/following-sibling::div') end
	if MANGAINFO.Artists == '' then MANGAINFO.Artists = x.XPathString('//div[@class="summary-heading" and contains(., "' .. XPathTokenArtists .. '")]/following-sibling::div') end
	if MANGAINFO.Genres == '' then MANGAINFO.Genres = x.XPathStringAll('//div[@class="summary-heading" and contains(., "' .. XPathTokenGenres .. '")]/following-sibling::div/a') end

	if x.XPathString('normalize-space(.)'):find('ajax_url') then
		HTTP.Reset()
		HTTP.Headers.Values['Cache-Control'] = 'no-cache'
		HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
		if HTTP.POST(MANGAINFO.URL .. 'ajax/chapters') then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		if MANGAINFO.ChapterLinks.Count == 0 then
			HTTP.Reset()
			HTTP.Headers.Values['Cache-Control'] = 'no-cache'
			HTTP.Headers.Values['X-Requested-With'] = 'XMLHttpRequest'
			HTTP.MimeType = 'application/x-www-form-urlencoded'
			id = x.XPathString('//div[contains(@id, "manga-chapters-holder")]/@data-id')
			q = 'action=manga_get_chapters&manga=' .. id
			if HTTP.POST(MODULE.RootURL .. '/wp-admin/admin-ajax.php', q) then
				CreateTXQuery(HTTP.Document).XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			end
		end
	else
		x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[not(@href="#")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	end
	ReverseLinksAndChapters()

	return no_error
end

function ReverseLinksAndChapters()
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return true
end

-- Get the page count for the current chapter.
function _M.GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if string.find(u, 'style=list', 1, true) == nil then u = string.gsub(u, '?style=paged', '') .. '?style=list' end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[contains(@class, "page-break")]/img/@data-src', TASK.PageLinks)
	if TASK.PageLinks.Count == 0 then x.XPathStringAll('//div[contains(@class, "page-break")]/img/@src', TASK.PageLinks) end
	for i = 0, TASK.PageLinks.Count - 1 do
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub("i%d.wp.com/", "")
		i = i + 1
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module After-Initialization
----------------------------------------------------------------------------------------------------

return _M
