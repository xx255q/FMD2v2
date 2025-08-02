----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '44d1b00d78a54277b9a2af68c7980be2'
	m.Name                     = 'DragonTranslation'
	m.RootURL                  = 'https://dragontranslation.net'
	m.Category                 = 'Spanish'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.TotalDirectory           = #DirectoryPages
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPages = {'mangas', 'doujin', 'raws'}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local u = MODULE.RootURL .. '/' .. DirectoryPages[MODULE.CurrentDirectoryIndex + 1] .. '?page=' .. (URL + 1)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//a[@class="link-light lanzador"]').Get() do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('div/h2', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1

	return no_error
end

-- Get info and chapter list for the current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if URL:find('/doujin/', 1, true) then
		MANGAINFO.Title     = x.XPathString('//h1')
		MANGAINFO.CoverLink = x.XPathString('(//div[@id="chapter_imgs"]/img)[1]/@src')

		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	else
		MANGAINFO.Title     = x.XPathString('//main//h3[contains(@class, "section-main-title")]')
		MANGAINFO.AltTitles = x.XPathString('//main//div[@class="row"]//p[./strong="Alternative:"]/text()')
		MANGAINFO.CoverLink = x.XPathString('//main//div[@class="row"]//img/@src')
		MANGAINFO.Authors   = x.XPathString('//main//div[@class="row"]//p[./strong="Authors:"]/text()')
		MANGAINFO.Artists   = x.XPathString('//main//div[@class="row"]//p[./strong="Artists:"]/text()')
		MANGAINFO.Genres    = x.XPathStringAll('//main//div[@class="row"]//p[./strong="Tag(s):"]/a')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="row"]//p[./strong="Status:"]/a|//div[@class="row"]//p[./strong="Status:"]/text()'), 'ongoing|publishing', 'ended')
		MANGAINFO.Summary   = x.XPathString('//main//div[@class="row"]/div[2]/text()')

		for v in x.XPath('//ul[@class="list-group"]/a').Get() do
			local title = x.XPathString('li/text()', v):gsub('\u{00A0}', ''):gsub('(%d+)%.(%d*0)$', function(int, dec)
				dec = dec:gsub('0+$', '') return dec == '' and int or (int .. '.' .. dec) end)

			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(title)
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return false end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="chapter_imgs"]/img/@src', TASK.PageLinks)

	return true
end