-- Get manga info and chapter list:
function getinfo()
	local x = nil

	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x = TXQuery.Create(HTTP.Document)
		MANGAINFO.CoverLink = x.XPathString('//figure[@class="image"][1]/img/@src')
		MANGAINFO.Artists = x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=artist:")]/text()')
		MANGAINFO.Title = '[' .. MANGAINFO.Artists .. '] ' .. x.XPathString('//h1[@class="title"]/text()') .. ' (' .. x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=publisher:")]/text()') .. ')'
		MANGAINFO.Genres = x.XPathStringAll('//table[@class="view-page-details"]//a[contains(@href, "=tag:")]/text()')
		MANGAINFO.Authors = x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=publisher:")]/text()')
		MANGAINFO.Status = 0
		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)

		return no_error
	end
	return net_problem
end

-- Get page number and page container LINKS:
function getpagenumber()
	local x = nil

	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		x = TXQuery.Create(HTTP.Document)
		x.XPathStringAll('(//div[@class="box"])[2]/div/div/a/@href', TASK.PageContainerLinks)
		TASK.PageNumber = TASK.PageContainerLinks.Count

		return true
	end
	return false
end

-- Get image urls from page containers:
function getimageurl()
	local s = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

	if HTTP.GET(s) then
		TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//img[@id="currImage"]/@src')

		return true
	end
	return false
end

-- Get last page number for manga directory:
function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).XPathString('(//ul[@class="pagination-list"])[1]/li[last()]/a/text()'))

		return no_error
	end
	return net_problem
end

-- Go through all directory pages and get NAMES and LINKS for manga entries:
function getnameandlink()
	local v, x = nil

	if HTTP.GET(MODULE.RootURL .. '/page/' .. IncStr(URL)) then
		x = TXQuery.Create(HTTP.Document)
		v = x.XPath('//div[@class="container"]/div/div/a[contains(@href, "/view/")]')
		for i = 1, v.Count do
			v1 = v.Get(i)
			LINKS.Add(v1.GetAttribute('href'))
			NAMES.Add(x.XPathString('div/header/@title', v1))
		end

		return no_error
	end
	return net_problem
end

-- Initialize module:
function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5eda5ccf87f1488f9dfa7a9a18f2bcf1'
	m.Category                 = 'H-Sites'
	m.Name                     = 'HentaiNexus'
	m.RootURL                  = 'https://hentainexus.com'
	m.OnGetInfo                = 'getinfo'
	m.OnGetPageNumber          = 'getpagenumber'
	m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
	m.OnGetNameAndLink         = 'getnameandlink'
	m.OnGetImageURL            = 'getimageurl'
end
