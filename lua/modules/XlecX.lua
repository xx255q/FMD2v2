function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'fb5ffb6bb1804573a7c76bc645fb66db'
	m.Name                       = 'XlecX'
	m.RootURL                    = 'https://xlecx.org'
	m.Category                   = 'H-Sites'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.SortedList                 = true
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="navigation"]/a[last()-1]')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/page/' .. (URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@id="dle-content"]//div[@class="th-text"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//meta[@property="og:title"]/@content')
		MANGAINFO.CoverLink  = x.XPathString('//meta[@property="og:image"]/@content')
		MANGAINFO.Authors    = x.XPathStringAll('//div[@class="full-in"]/div[contains(., "Artist")]/a')
		MANGAINFO.Genres     = x.XPathStringAll('//div[@class="full-in"]/div[contains(., "Tags")]//a')

		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "f-desc")]//a/@href', TASK.PageLinks)
		return true
	else
		return false
	end
end