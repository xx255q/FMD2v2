local dirurl = '/explore?search[sort]=date'

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		page = tonumber(TXQuery.Create(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. dirurl .. '&page=' .. IncStr(URL)) then
		TXQuery.Create(HTTP.Document).XPathHREFAll('//a[contains(@class,"comic-grid-name")]', LINKS, NAMES)	    
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)
		
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.URL, x.XPathString('//img[@class="cover-detail-img"]/@src'))
		MANGAINFO.Title     = x.XPathString('//h1')
		MANGAINFO.Authors   = x.XPathString('//a[@itemprop="author"]')
		MANGAINFO.Genres    = x.XPathString('string-join(//span[@itemprop="about"]//svg/a, ", ")')
		MANGAINFO.Summary   = x.XPathString('//div[@itemprop="description"]')

		x.XPathHREFAll('//div[@class="card-header" and .="Содержание"]/following-sibling::div//a[contains(@class,"text-truncate")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = TXQuery.Create(HTTP.Document)
		local s = x.XPathString('//div[@data-js-scans]/@data-js-scans')
		if s ~= '' then
			x.ParseHTML(s)
			for _, v in ipairs(x.XPathI('json(*)()("src")')) do
				TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString():gsub('^//', 'https://')))
			end
		end
		return true
	else
		return false
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fe52ff5f7cf64d98ae17052dee6ab2c2'
	m.Name                     = 'MangaHubRU'
	m.RootURL                  = 'https://mangahub.ru'
	m.Category                 = 'Russian'
	m.SortedList               = true
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end
