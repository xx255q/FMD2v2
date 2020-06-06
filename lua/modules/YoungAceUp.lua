function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL..'/comics/') then
			CreateTXQuery(HTTP.Document).XPathHREFAll('//ul[@class="manga-list__list"]/li/h4/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL=AppendURLDelim(MaybeFillHost(MODULE.RootURL,URL))
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//strong')
		MANGAINFO.CoverLink=x.XPathString('//*[@class="fancybox a-alpha"]/img/@src')
		MANGAINFO.Summary=x.XPathString('//*[@class="single-story"]/p')
		-- there is no chapter list?
		-- assuming the first chapter link in manga info is always the last chapters
			x.XPathHREFAll('//a[@class="single"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(FillHost('http://viewer.tonarinoyj.jp',URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//img[@class="js-page-image"]/@src', TASK.PageLinks)
		return true
	else
		return false
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID               = '9398095f450f4a1f9f0cc5c3ea145f0b'
	m.Category         = 'Raw'
	m.Name             = 'YoungAceUp'
	m.RootURL          = 'https://web-ace.jp/youngaceup'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
end
