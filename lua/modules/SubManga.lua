function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=TXQuery.Create(HTTP.Document)
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = Trim(x.XPathString('//div[contains(@class, "col-md-3")]//h3/text()'))
		end
		MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "col-md-3")]//img[@class="img-responsive"]/@src')
		MANGAINFO.Authors=Trim(x.XPathString('//span[@class="list-group-item" and contains(., "Autor")]/a'))
		MANGAINFO.Artists=Trim(x.XPathString('//span[@class="list-group-item" and contains(., "Artist")]/a'))
		MANGAINFO.Genres=Trim(x.XPathStringAll('//span[@class="list-group-item" and contains(., "Categorías:")]/a'))
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//span[@class="list-group-item" and contains(., "Estado")]'))
		MANGAINFO.Summary = x.XPathStringAll('//span[@class="list-group-item" and contains(., "Resumen")]/text()', '')
		x.XPathHREFAll('//table//tr/td[1]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		HTTP.Reset()
		HTTP.Headers.Values['Referer'] = MANGAINFO.URL
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	TASK.PageContainerLinks.Clear()
	local URL = MaybeFillHost(MODULE.RootURL, URL);
	if HTTP.GET(URL) then
		local x=TXQuery.Create(HTTP.Document)
		x.XPathStringAll('//*[@id="all"]/img/@data-src', TASK.PageLinks)
		TASK.PageContainerLinks.Text = URL
	else
		return false
	end
	return true
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = TASK.PageContainerLinks.Text
	return true
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL .. '/changeMangaList?type=text') then
		TXQuery.Create(HTTP.Document).XPathHREFAll('//li/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = 'cdf8dd5f0f7b458f97e344a430e979ba'
	m.Name = 'SubManga'
	m.RootURL = 'https://submangas.net'
	m.Category = 'Spanish'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end
