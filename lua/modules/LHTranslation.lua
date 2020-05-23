function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=TXQuery.Create(HTTP.Document)
		if MANGAINFO.Title == '' then
		local v=x.XPath('//ol[@class="breadcrumb"]//li//a')
		local v1=v.Get(3)
		MANGAINFO.Title = v1.ToString()
		end
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="thumbnail"]/@src'))
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info"]/li[contains(., "Status")]//a'))
		MANGAINFO.Authors=x.XPathString('//ul[@class="manga-info"]/li[contains(., "Author")]//a')
		MANGAINFO.Genres=x.XPathStringAll('//ul[@class="manga-info"]/li[contains(., "Genre")]//a')
		MANGAINFO.Summary=x.XPathString('//h3[text()="Description"]/following-sibling::p')
		if MANGAINFO.Summary == '' then
			MANGAINFO.Summary=x.XPathString('//div[@class="detail"]/div[@class="content"]')
		end
		x.XPathHREFAll('//div[@id="tab-chapper"]//table/tbody/tr/td/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		if MANGAINFO.ChapterLinks.Count == 0 then
			x.XPathHREFAll('//div[@id="list-chapters"]//a[@class="chapter"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		for i = 0, MANGAINFO.ChapterLinks.Count-1 do
			MANGAINFO.ChapterLinks[i] = MODULE.RootURL .. '/' .. MANGAINFO.ChapterLinks[i]
		end
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(u) then
		local x=TXQuery.Create(HTTP.Document)
		x.XPathStringAll('//img[@class="_lazy chapter-img"]/@src', TASK.PageLinks)
		TASK.PageContainerLinks.Text = u
	else
		return false
	end
	return true
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL .. '/manga-list.html?listType=allABC') then
		local x = TXQuery.Create(HTTP.Document)
		local v = x.XPath('//span[@manga-slug]//a')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			NAMES.Add(Trim(SeparateLeft(v1.ToString(), '- Raw')));
			LINKS.Add(v1.GetAttribute('href'));
		end
		return no_error
	else
		return net_problem
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = TASK.PageContainerLinks.Text
	return true
end

function Init()
	local m = NewWebsiteModule()
	m.ID                    = '7fb5fbed6d3a44fe923ecc7bf929e6cb'
	m.Category              = 'English-Scanlation'
	m.Name                  = 'LHTranslation'
	m.RootURL               = 'https://lhtranslation.net'
	m.LastUpdated           = 'March 30, 2019'
	m.TotalDirectory        = 1
	m.OnGetInfo             = 'getinfo'
	m.OnGetPageNumber       = 'getpagenumber'
	m.OnGetNameAndLink      = 'getnameandlink'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end
