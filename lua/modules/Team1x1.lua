function Init()
	local m = NewWebsiteModule()
	m.ID                         = '2889e1036e104c9081e0be59180b8354'
	m.Name                       = 'Team1x1'
	m.RootURL                    = 'https://team1x1.com'
	m.Category                   = 'Arabic'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/manga/') then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//a[@class="last"]/@href'):match('page/(%d+)/')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/manga/page/' .. (URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="info"]//a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//div[@class="col-md-9"]')
		MANGAINFO.CoverLink  = x.XPathString('//div[@class="thumb"]/img/@src')
		MANGAINFO.Genres     = x.XPathStringAll('//div[@class="genre"]/a')
		MANGAINFO.Summary    = x.XPathString('//div[@class="story"]/p')

		x.XPathHREFAll('//div[@class="col-md-12"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="translationPageall"]//img/@src', TASK.PageLinks)
		return true
	else
		return false
	end
end