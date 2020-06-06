function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//*[contains(@class,"entry-content")]/h2')
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = Trim(x.XPathString('//*[contains(@class,"entry-content")]/p[starts-with(.,"Title")]/substring-after(.,":")'))
		end
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = x.XPathString('//*[contains(@class,"entry-content")]/p[1]/strong')
		end
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = x.XPathString('//h1[@class="entry-title"]')
		end
		MANGAINFO.Authors=Trim(x.XPathString('//*[contains(@class,"entry-content")]/p[starts-with(.,"Author")]/substring-after(.,":")'))
		MANGAINFO.Genres=x.XPathString('//header[@class="entry-header"]/string-join(./p[position()>1]//a,", ")')
		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		local v = x.XPath('//*[contains(@class,"entry-pagination")]/a')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			if string.match(v1.ToString(), '^Next') == nil then
				MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'));
				MANGAINFO.ChapterNames.Add(MANGAINFO.Title .. ' - ' .. v1.ToString());
			end
		end
		if MANGAINFO.ChapterNames.Count > 1 then
			MANGAINFO.ChapterNames[0] = MANGAINFO.ChapterNames[0] .. ' - 1'
		end
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//*[contains(@class,"entry-content")]//img/@data-lazy-src', TASK.PageLinks)
		if TASK.PageLinks.Count == 0 then
			x.XPathStringAll('//div[@class="separator" and @style]//img/@data-lazy-src', TASK.PageLinks)
		end
	else
		return false
	end
	return true
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL) then
		local x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//*[contains(@class,"archive-pagination")]/ul/li[last()-1]')) or 1
		return no_error
	else
		return net_problem
	end
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL .. '/page/' .. (URL + 1) .. '/') then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//h2[@class="entry-title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '84451c35b7764bb5a5e3dd8692e84682'
	m.Name = 'MyReadingManga'
	m.RootURL = 'https://myreadingmanga.info'
	m.Category = 'H-Sites'
	m.LastUpdated='April 10, 2018'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
	m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
	m.SortedList = true
end
