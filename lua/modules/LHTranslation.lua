function Init()
	local m = NewWebsiteModule()
	m.ID                    = '7fb5fbed6d3a44fe923ecc7bf929e6cb'
	m.Category              = 'English-Scanlation'
	m.Name                  = 'LHTranslation'
	m.RootURL               = 'https://lhtranslation.net'
	m.OnGetNameAndLink      = 'GetNameAndLink'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/manga-list.html?listType=allABC') then
		TXQuery.Create(HTTP.Document).XPathHREFAll('//span[@manga-slug]//a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('(//ol[@class="breadcrumb"]//span[@itemprop="name"])[last()]')
		MANGAINFO.CoverLink = x.XPathString('//img[@class="thumbnail"]/@src')
		MANGAINFO.Authors   = x.XPathStringAll('//ul[@class="manga-info"]/li[starts-with(.,"Author")]//a')
		MANGAINFO.Summary   = x.XPathString('string-join(//div[./h3="Description"]/p, "\r\n")')
		MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="manga-info"]/li[starts-with(.,"Genre")]//a')
		MANGAINFO.Status    = MangaInfoStatusIfPos((x.XPathString('//ul[@class="manga-info"]/li[starts-with(.,"Status")]//a')))
		x.XPathHREFAll('//div[@id="list-chapters"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MODULE.RootURL, URL) then
		TXQuery.Create(HTTP.Document).XPathStringAll('//img[@class="_lazy chapter-img"]/@src', TASK.PageLinks)
	else
		return false
	end
	return true
end