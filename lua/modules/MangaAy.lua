function Init()
	local m = NewWebsiteModule()
	m.ID                  = '738b476a0fa5429bbc9de8e2ae5d00a6'
	m.Name                = 'MangaAy'
	m.RootURL             = 'https://manga-ay.com'
	m.Category            = 'Turkish'
	m.OnGetNameAndLink    = 'GetNameAndLink'
	m.OnGetInfo           = 'GetInfo'
	m.OnGetPageNumber     = 'GetPageNumber'
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/seriler') then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="table-responsive"]//td[2]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//h3[@class="panel-title"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="col-sm-3"]/img/@src'))
		MANGAINFO.Authors   = x.XPathStringAll('//th[contains(., "Yazar")]/following-sibling::td/a')
		MANGAINFO.Artists   = x.XPathStringAll('//th[contains(., "Çizer")]/following-sibling::td/a')
		MANGAINFO.Genres    = x.XPathStringAll('//th[contains(., "Türler")]/following-sibling::td//a')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//th[contains(., "Durum")]/following-sibling::td'), 'Devam Ediyor', 'Tamamlandı')
		MANGAINFO.Summary   = x.XPathString('//th[contains(., "Açıklama")]/following-sibling::td')

		x.XPathHREFAll('//div[@class="table-responsive"]//td[2]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		local body    = x.XPathString('//script[contains(.,"var dataurl")]')
		local dataurl = body:match("var dataurl = '(.-)'")
		local server  = body:match("var server = '(.-)'")
		local images  = body:match('var page_array = %[([^%]]+)')
		local i; for i in images:gmatch("'([^',]+)") do
			TASK.PageLinks.Add(MODULE.RootURL .. server .. dataurl .. '/' .. i)
		end
		return true
	else
		return false
	end
end