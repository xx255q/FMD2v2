function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	HTTP.Cookies.Values['set'] = 'h=1'
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//h2'):gsub(' Manga$', '')
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class, "cover")]/img/@src'))
		MANGAINFO.Authors=x.XPathStringAll('//table[@class="attr"]//tr[contains(th,"Author")]/td/a')
		MANGAINFO.Artists=x.XPathStringAll('//table[@class="attr"]//tr[contains(th,"Artist")]/td/a')
		MANGAINFO.Genres=x.XPathStringAll('//table[@class="attr"]//tr[contains(th,"Genre")]/td/a')
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//table[@class="attr"]//tr[contains(th,"Status")]/td'))
		MANGAINFO.Summary=x.XPathString('//p[@class="summary"]')
		local v for v in x.XPath('//div[@id="list"]/div[contains(@class, "stream")]').Get() do
			local stream = ' [' .. x.XPathString('div[@id]//a/span', v) .. ']'
			local w for w in x.XPath('div/div/ul[@class="chapter"]/li', v).Get() do
				local link = x.XPathString('div/a/@href', w)
				local title = x.XPathString('div/a', w) .. x.XPathString('div[contains(@class, "txt")]', w)
				MANGAINFO.ChapterLinks.Add(link:gsub('/%d+$', ''))
				MANGAINFO.ChapterNames.Add(title .. stream)
			end
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=CreateTXQuery(HTTP.Document)
		x.XPathStringAll('json(//script[contains(.,"var _load_pages")]/substring-after(substring-before(.,";")," = "))()/u', TASK.PageLinks)
	else
		return false
	end
	return true
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL .. '/search?orderby=create') then
		local x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('(//div[@id="paging-bar"])[2]/ul/li[last()-2]/a/substring-after(@href, "page=")')) or 1
		return no_error
	else
		return net_problem
	end
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL .. '/search?orderby=create&page=' .. (URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//div[@class="manga-list"]//table//h2/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID = id
		m.Name = name
		m.RootURL = url
		m.Category = 'English'
		m.OnGetInfo='getinfo'
		m.OnGetPageNumber='getpagenumber'
		m.OnGetNameAndLink='getnameandlink'
		m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
		m.SortedList = true
	end
	AddWebsiteModule('09d49c7760904bc2817dbc80757de44a', 'MangaPark', 'https://mangapark.net')
	AddWebsiteModule('32b25f04931b414a803286a6f8cabd8d', 'MangaParkNet', 'https://mangapark.net')
	AddWebsiteModule('273268fd7b1a42b38e50c010a630bfad', 'MangaParkCom', 'https://mangapark.com')
end
