local domain = 'pururin.io'

function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=TXQuery.Create(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//*[@class="title"]/h1')
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//*[@class="cover-wrapper"]//v-lazy-image/@src'))
		MANGAINFO.Artists = x.XPathStringAll('//table[contains(@class,"table-gallery-info")]//tr/td[contains(.,"Artist")]/following-sibling::td//a')
		MANGAINFO.Genres = x.XPathStringAll('//table[contains(@class,"table-gallery-info")]//tr/td[contains(.,"Contents")]/following-sibling::td//a')
		MANGAINFO.ChapterLinks.Add(x.XPathString('//*[@class="gallery-action"]/a/@href'))
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local PATH = 'https://cdn.' .. domain .. '/assets/images/data'
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		local x=TXQuery.Create(HTTP.Document)
		local s = x.XPathString('//gallery-read/@gallery')
		x.ParseHTML(s)
		local ext = x.XPathString('json(*).image_extension')
		local cnt = x.XPathString('json(*).total_pages')
		local id = x.XPathString('json(*).ID')
		for i = 1, cnt do
			TASK.PageLinks.Add(string.format('%s/%s/%d.%s', PATH, id, i, ext))
		end
		return true
	else
		return false
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL..'/browse/newest?page='..IncStr(URL)) then
		local x=TXQuery.Create(HTTP.Document)
		local v=x.XPath('//*[@class="row-gallery"]/a')
		for i=1,v.Count do
			local v1 = v.Get(i)
			LINKS.Add(v1.GetAttribute('href'))
			NAMES.Add(x.XPathString('.//*[@class="title"]/text()[1]', v1))
		end
		return no_error
	else
		return net_problem
	end
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL) then
		local x = TXQuery.Create(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//ul[contains(@class,"pagination")]/li[last()-1]')) or 1
		return true
	else
		return false
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '18470a3b19034f3f91289c2b8b7d3ab3'
	m.Category='H-Sites'
	m.Name='Pururin'
	m.RootURL='https://' .. domain
	m.LastUpdated = 'March 29, 2019'
	m.SortedList=true
	m.OnGetInfo='GetInfo'
	m.OnGetPageNumber='GetPageNumber'
	m.OnGetNameAndLink='GetNameAndLink'
	m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end