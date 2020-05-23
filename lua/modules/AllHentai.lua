local dirurl = '/list?type=&sortType=DATE_CREATE'
local perpage = 60

function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=TXQuery.Create(HTTP.Document)
		local rname = x.XPathString('//meta[@itemprop="name"]/@content')
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = x.XPathString('//meta[@itemprop="alternativeHeadline"]/@content')
		end
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = rname
		end
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@itemprop="image"]/@src'))
		MANGAINFO.Authors=x.XPathStringAll('//p[@class="elementList" and contains(b, "Автор")]/a')
		MANGAINFO.Genres=x.XPathStringAll('//p[@class="elementList" and (contains(b, "Жанры") or contains(b, "Категории"))]/a')
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//p[contains(b, "Перевод")]'), 'продолжается', 'завершен')
		MANGAINFO.Summary=x.XPathString('//div[@class="mangaDescription"]/div[@itemprop="description"]')
		-- TODO: remove manga name from chapter name
		x.XPathHREFAll('//div[@class="expandable"]/table[@class="cTable"]/tbody/tr/td/a',MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function beforedownloadimage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end

function getpagenumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=TXQuery.Create(HTTP.Document)
		local s = x.XPathString('//script[contains(., "var pictures")]')
		s = GetBetween('pictures =', ';', s)
		x.ParseHTML(s)
		x.XPathStringAll('json(*)().URL', TASK.PageLinks)
	else
		return false
	end
	return true
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		local x = TXQuery.Create(HTTP.Document)
		local s = x.XPathString('//*[@class="pagination"]/a[@class="step"][last()]/@href')
		PAGENUMBER = tonumber(s:match('offset=(%d+)')) or 1
		if PAGENUMBER > 1 then
		PAGENUMBER = math.ceil(PAGENUMBER / perpage) + 1
	end
		return no_error
	else
		return net_problem
	end
end

function getnameandlink()
	local s = MODULE.RootURL .. dirurl
	if URL ~= '0' then s = s .. '&offset=' .. (tonumber(URL) * perpage) .. '&max=' .. perpage; end
	if HTTP.GET(s) then
		local x = TXQuery.Create(HTTP.Document)
		local v = x.XPath('//table[@class="cTable"]//tr/td/a[not(@class)]')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			LINKS.Add(v1.GetAttribute('href'))
			NAMES.Add(x.XPathString('./text()', v1))
		end
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '29ef3c8373f54aa2b8167d2af7b0d241'
	m.Name = 'AllHentai'
	m.RootURL = 'http://allhentai.ru'
	m.Category = 'Russian'
	m.LastUpdated='April 26, 2018'
	m.SortedList = true
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
	m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
	m.OnBeforeDownloadImage = 'beforedownloadimage'
end
