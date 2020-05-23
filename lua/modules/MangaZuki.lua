local dirurl = '/manga-list'

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL..dirurl) then
		local s = TXQuery.Create(HTTP.Document).XPathString('(//ul[@class="pagination"]//a)[last()-1]/@href')
		PAGENUMBER = tonumber(s:match('=(%d+)$') or 1)
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL..dirurl..'?page='..IncStr(URL)) then
			TXQuery.Create(HTTP.Document).XPathHREFAll('//*[@class="row"]//a[@class="chart-title"]', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)

		MANGAINFO.CoverLink = x.XPathString('//meta[@itemprop="photo"]/@content')
		MANGAINFO.Title     = x.XPathString('//div[@class="container"]/div[@class="row"]/div/h2')
		MANGAINFO.Summary   = x.XPathString('//h5[text()="Summary"]/following-sibling::*')

		x.XPathHREFAll('//ul[@class="chapters"]/li/h3/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(RemoveURLDelim(MaybeFillHost(MODULE.RootURL, URL))) then
		TXQuery.Create(HTTP.Document).XPathStringAll('//div[@id="all"]/img/@data-src', TASK.PageLinks)
		return true
	else
		return false
	end
end

function Init()
	function AddWebsiteModule(id, website, rooturl, category)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = website
		m.RootURL                  = rooturl
		m.Category                 = category
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('6c116508a52448eeae4d09ff909c9d22', 'MangaZuki', 'https://mangazuki.co', 'English-Scanlation')
	AddWebsiteModule('5d1a3c9e886f4e0b83894c8894914c24', 'MangaZukiRaws', 'https://raws.mangazuki.co', 'Raw')
end
