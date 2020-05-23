function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=TXQuery.Create(HTTP.Document)
		MANGAINFO.CoverLink = x.XPathString('//*[@id="mangaimg"]/img/@src')
		MANGAINFO.Title = x.XPathString('//*[@id="mangaproperties"]//h2')
		MANGAINFO.Authors = x.XPathString('//*[@id="mangaproperties"]//td[contains(text(),"Author:")]/following-sibling::td')
		MANGAINFO.Artists = x.XPathString('//*[@id="mangaproperties"]//td[contains(text(),"Artist:")]/following-sibling::td')
		MANGAINFO.Genres = x.XPathStringAll('//*[@id="mangaproperties"]//td[contains(text(),"Genre:")]/following-sibling::td/a')
		MANGAINFO.Status = MangaInfoStatusIfPos((x.XPathString('//*[@id="mangaproperties"]//td[contains(text(),"Status:")]/following-sibling::td')))
		summary = x.XPathString('//*[@id="readmangasum"]/p')
		local chapters=x.XPath('//table[@id="listing"]//tr/td[1]')
		for i=1,chapters.Count do
			MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href',chapters.Get(i)))
			MANGAINFO.ChapterNames.Add(x.XPathString('.',chapters.Get(i)))
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		TASK.PageNumber=TXQuery.Create(HTTP.Document).XPath('//select[@id="pageMenu"]/option').Count
		return true
	else
		return false
	end
end

function GetImageURL()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)..'/'..tostring(WORKID+1)) then
		TASK.PageLinks[WORKID]=TXQuery.Create(HTTP.Document).XPathString('//img[@id="img"]/@src')
		return true
	else
		return false
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL..'/alphabetical') then
		TXQuery.Create(HTTP.Document).XPathHREFAll('//ul[@class="series_alpha"]/li/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	function AddWebsiteModule(id, name, URL, category)
		local m = NewWebsiteModule()
		m.ID = id
		m.Name				= name
		m.RootURL				= URL
		m.Category				= category
		m.OnGetInfo				= 'GetInfo'
		m.OnGetPageNumber		= 'GetPageNumber'
		m.OnGetImageURL			= 'GetImageURL'
		m.OnGetNameAndLink		= 'GetNameAndLink'
	end
	AddWebsiteModule('e9f4e7fe53bc4ff6921db6d67c9b9fb2', 'MangaReader', 'http://www.mangareader.net', 'English')
	AddWebsiteModule('42bd64394655449b8b39ed2c20481c15', 'MangaPanda', 'http://www.mangapanda.com', 'English')
end