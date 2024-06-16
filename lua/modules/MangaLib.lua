----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'Russian'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('df365d3d22f141ad8b9224cc1413ca02', 'MangaLib', 'https://mangalib.me')
	AddWebsiteModule('892b72276f06476cb1f866998bb9e894', 'MangaLib', 'https://mangalib.org')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/manga-list?sort=created_at&dir=desc&page='
DirectoryParameters = '&site_id=1&type=manga&caution_list%5B%5D=%D0%9E%D1%82%D1%81%D1%83%D1%82%D1%81%D1%82%D0%B2%D1%83%D0%B5%D1%82&caution_list%5B%5D=16+&caution_list%5B%5D=18+'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. (URL + 1) .. DirectoryParameters

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.ParseHTML(x.XPathString('//script[contains(., "window.__DATA")]/substring-before(substring-after(., "DATA = "), "};")') .. '}')
	if x.XPath('json(*).filters.series()').Count == 0 then return no_error end
	for v in x.XPath('json(*).filters.series()').Get() do
		LINKS.Add(x.XPathString('slug', v))
		NAMES.Add(x.XPathString('name', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local chapter, name, slug, v, volume, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@itemprop="alternativeHeadline"]/@content')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="media-sidebar__cover paper"]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[contains(., "Автор")]/following-sibling::div/a')
	MANGAINFO.Artists   = x.XPathStringAll('//div[contains(., "Художник")]/following-sibling::div/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="media-tags"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[contains(., "Статус перевода")]/following-sibling::div'), 'Продолжается', 'Завершен')
	MANGAINFO.Summary   = x.XPathString('//div[@class="media-description__text"]')

	x.ParseHTML(x.XPathString('//script[contains(., "window.__DATA")]/substring-before(substring-after(., "__ = "), "};")') .. '}')
	slug = x.XPathString('json(*).manga.slug')
	for v in x.XPath('json(*).chapters.list()').Get() do
		name    = x.XPathString('chapter_name', v)
		volume  = x.XPathString('chapter_volume', v)
		chapter = x.XPathString('chapter_number', v)

		name = name ~= 'null' and name ~= '' and string.format(' - %s', name) or ''

		MANGAINFO.ChapterLinks.Add(slug .. '/v' .. x.XPathString('chapter_volume', v) .. '/c' .. x.XPathString('chapter_number', v))
		MANGAINFO.ChapterNames.Add('Том ' .. volume .. ' Глава ' .. chapter .. name)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	HTTP.Reset()
	HTTP.Headers.Values['Referer'] = MANGAINFO.URL
	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local server, url, v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	url = x.XPathString('json(//script[contains(., "window.__info")]/substring-before(substring-after(., "info = "), ";")).img.url')
	server = x.XPathString('json(//script[contains(., "window.__info")]/substring-before(substring-after(., "info = "), ";")).servers.main')
	for v in x.XPath('json(//script[contains(., "window.__pg")]/substring-before(substring-after(., " = "), ";"))()').Get() do
		TASK.PageLinks.Add(server .. url .. x.XPathString('u', v))
	end

	return no_error
end