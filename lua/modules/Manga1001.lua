function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                         = id
		m.Name                       = name
		m.RootURL                    = url
		m.Category                   = 'Raw'
		m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink           = 'GetNameAndLink'
		m.OnGetInfo                  = 'GetInfo'
		m.OnGetPageNumber            = 'GetPageNumber'
		m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
	end
	AddWebsiteModule('1d09f3bea8f148fa9e9215fc578fedcd', 'Manga1001', 'https://manga1001.in')
	AddWebsiteModule('67e79aff105946dc8586a220ed5763ef', 'SyoSetu', 'https://syosetu.top')
	AddWebsiteModule('cf43153ce70c4501b1deb7a0f039f3fd', 'Manga9', 'https://manga9.co')
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/list/') then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="wp-pagenavi"]/a[last()]/@href'):match('=(%d+)')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/list/?page=' .. (URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//div[@class="text-center text-white text-shadow p-2"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = Trim(SeparateLeft(x.XPathString('//h1'), "(Raw – Free)"))
		MANGAINFO.CoverLink = x.XPathString('//main[@id="primary"]/p/img/@data-src')
		MANGAINFO.Genres    = x.XPathStringAll('//main[@id="primary"]/div/a[@rel="category tag"]')
		MANGAINFO.Summary   = x.XPathString('//main[@id="primary"]/p[3]')
		x.XPathHREFAll('//table[contains(@class, "table")]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MANGAINFO.URL
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="container-chapter-reader"]//img/@data-src', TASK.PageLinks)
		return true
	else
		return false
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	return true
end
