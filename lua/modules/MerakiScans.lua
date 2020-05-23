function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=TXQuery.Create(HTTP.Document)
		if MANGAINFO.Title == '' then
			MANGAINFO.Title=x.XPathString('//*[@id="manga_name"]')
		end
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@id="cover_img"]/@src'))
		MANGAINFO.Authors=x.XPathString('//ul[@id="detail_list"]/li[contains(., "Author")]/substring-after(., ":")')
		MANGAINFO.Artists=x.XPathString('//ul[@id="detail_list"]/li[contains(., "Artist")]/substring-after(., ":")')
		MANGAINFO.Genres=x.XPathStringAll('//ul[@id="detail_list"]/li[contains(., "Genres")]/a')
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//ul[@id="detail_list"]/li[contains(., "Status")]'))
		MANGAINFO.Summary=x.XPathString('//ul[@id="detail_list"]/span')
		local v = x.XPath('//table[@id="chapter_table"]//tr')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			MANGAINFO.ChapterLinks.Add(MaybeFillHost(MODULE.RootURL, v1.GetAttribute("data-href")))
			MANGAINFO.ChapterNames.Add(x.XPathString("./td[1]", v1))
		end
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	TASK.PageNumber = 0
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = TXQuery.Create(HTTP.Document)
		local curCh = x.XPathString('//select[@id="chapter_select"]/option[@selected]/@value')
		local s = x.XPathString('//script[contains(., "images")]')
		local slug = Trim(GetBetween("var manga_slug =", ";", s):gsub('"', ''))
	local chapter = Trim(GetBetween("var viewschapter =", ";", s):gsub('"', ''))
		s = GetBetween("var images =", ";", s)
		x.ParseHTML(s)
		local v = x.XPath('json(*)()')
		for i = 1, v.Count do
			s = string.format("/manga/%s/%s/%s/%s", slug, chapter, curCh, v.Get(i).ToString())
		s = string.gsub(s, '//', '/')
		s = string.gsub(s, '///', '/')
			TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, s))
		end
	else
		return false
	end
	return true
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL .. '/manga/') then
		local x = TXQuery.Create(HTTP.Document)
		local v = x.XPath('//div[@id="all"]/div[@id="listitem"]/a')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			LINKS.Add(v1.GetAttribute('href'))
			NAMES.Add(x.XPathString('./h1', v1))
		end
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '06436a82f5994b889e8d63f76fc5c69a'
	m.Name = 'MerakiScans'
	m.RootURL = 'https://merakiscans.com'
	m.Category = 'English-Scanlation'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
end
