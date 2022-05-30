function Init()
	local m = NewWebsiteModule()
	m.ID                       = '29826228ef9e4d1bad17f4c22e8d9951'
	m.Name                     = 'MangaKatana'
	m.RootURL                  = 'https://mangakatana.com'
	m.Category                 = 'English'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/manga/page/1?filter=1&include_mode=and&chapters=1&order=new') then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="uk-pagination"]/li[last()-1]/a/@href'):match('/(%d+)?')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/manga/page/' .. (URL + 1) .. '?filter=1&include_mode=and&chapters=1&order=new') then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@id="book_list"]//h3[@class="title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//h1')
		MANGAINFO.CoverLink = x.XPathString('//div[@class="cover"]/img/@src')
		MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="meta d-table"]//a[@class="author"]')
		MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="meta d-table"]//div[@class="genres"]/a')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="meta d-table"]/li[(./div="Status:")]'))
		MANGAINFO.Summary   = x.XPathString('//div[@class="summary"]/p')

		x.XPathHREFAll('//div[@class="chapter"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		x.ParseHTML(GetBetween('var ytaw=', ';function', x.XPathString('//script[contains(., "ytaw")]')))
		x.XPathStringAll('json(*)()', TASK.PageLinks)
		return true
	else
		return false
	end
end
