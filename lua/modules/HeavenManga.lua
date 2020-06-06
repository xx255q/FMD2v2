-- Get info and chapter list for current manga.
function GetInfo()
	local pages, x, v = nil
	local u = AppendURLDelim(MaybeFillHost(MODULE.RootURL, URL))
	local p = 1

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="comic-info"]/div/img/@src'))
	MANGAINFO.Title     = x.XPathString('//div[@class="info"]/h1')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="info"]//div[@class="author"]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="info"]//div[@class="genre"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="info"]//div[@class="update"]/span[last()]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="comic-description"]/p')

	pages = tonumber(x.XPathString('//div[@class="pagination"]//a[contains(@class, "page-numbers")][last()]/substring-after(@href, "/page-")'))
	if pages == nil then pages = 1 end
	while true do
		for _, v in ipairs(x.XPathI('//div[contains(@class, "chapters-wrapper")]//h2[@class="chap"]/a')) do
			MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		end
		p = p + 1
		if p > pages then
			break
		elseif HTTP.GET(u .. '/page-' .. tostring(p)) then
			x = CreateTXQuery(HTTP.Document)
		else
			break
		end
	end
	InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = AppendURLDelim(MaybeFillHost(MODULE.RootURL, URL))

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathStringAll('//div[@class="chapter-content"]//img/@src', TASK.PageLinks)
	TASK.PageNumber = TASK.PageLinks.Count

	return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	local x = nil
	local u = MODULE.RootURL .. '/manga-list/page-1/'

	if not HTTP.GET(u) then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//div[@class="pagination"]/a[contains(@class, "page-numbers")])[last()]/substring-after(@href, "/page-")'))

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local u = MODULE.RootURL .. '/manga-list/page-' .. (URL + 1) .. '/'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	x.XPathHREFAll('//div[@class="comics-grid"]/div/div/h3/a', LINKS, NAMES)

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, URL, cat)
		local m = NewWebsiteModule()
		m.ID                          = id
		m.Name                        = name
		m.RootURL                     = URL
		m.Category                    = cat
		m.OnGetInfo                   = 'GetInfo'
		m.OnGetNameAndLink            = 'GetNameAndLink'
		m.OnGetPageNumber             = 'GetPageNumber'
		m.OnGetDirectoryPageNumber    = 'GetDirectoryPageNumber'
	end
	AddWebsiteModule('3b0d5c38081a4b21a39a388a3ec59197', 'HeavenManga', 'http://www.heaventoon.com', 'English')
	AddWebsiteModule('a9a8bd394d63495686794a8d427bda00', 'HolyManga', 'http://w15.holymanga.net', 'English')
end
