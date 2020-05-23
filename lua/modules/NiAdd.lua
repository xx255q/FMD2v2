function getauthors()
	if MODULE.Name == 'NiAddRU' then
		return "Авторы"
	else
		return "Aut"
	end
end

function getartists()
	if MODULE.Name == 'NiAddRU' then
		return "Исполнитель"
	elseif MODULE.Name == 'NiAddDE' then
		return "Künstler"
	else
		return "Art"
	end
end

function getgenres()
	if MODULE.Name == 'NiAddES' or MODULE.Name == 'NiAddBR' then
		return "Géneros"
	elseif MODULE.Name == 'NiAddIT' then
		return "generi"
	elseif MODULE.Name == 'NiAddRU' then
		return "Жанры"
	else
		return "Genres"
	end
end

function getdirpagenumber()
	if MODULE.Name == 'NiAddES' or MODULE.Name == 'NiAddBR' then
		return "Todos"
	elseif MODULE.Name == 'NiAddIT' then
		return "tutti"
	elseif MODULE.Name == 'NiAddRU' then
		return "все"
	elseif MODULE.Name == 'NiAddDE' then
		return "alle"
	elseif MODULE.Name == 'NiAddFR' then
		return "Tous"
	else
		return "All"
	end
end

function getinfo()
	if URL:find('/chapters') then
		MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	else
		MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL..'/chapters'):gsub('.html', '')
	end
	if HTTP.GET(MANGAINFO.URL) then
		local x=TXQuery.Create(HTTP.Document)
		if MANGAINFO.Title == '' then
			MANGAINFO.Title=x.XPathString('//h1')
		end
		MANGAINFO.CoverLink=x.XPathString('//img[@itemprop="image"]/@src')
		MANGAINFO.Authors=x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author" and contains(span, "'..getauthors()..'")]/a/span')
		MANGAINFO.Artists=x.XPathStringAll('//td[@class="bookside-general-type"]//div[@itemprop="author" and contains(span, "'..getartists()..'")]/a/span')
		MANGAINFO.Genres=x.XPathStringAll('//td[@class="bookside-general-type"]//div[contains(span, "'..getgenres()..'")]/a/span')
		local n = x.XPath('//span[@class="chp-title"]')
		local v = x.XPath('//ul[contains(@class, "chapter-list")]/a')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			MANGAINFO.ChapterNames.Add(n.Get(i).ToString())
			MANGAINFO.ChapterLinks.Add(v.Get(i).GetAttribute('href'))
		end
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=TXQuery.Create(HTTP.Document)
		x.XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', TASK.PageContainerLinks)
		TASK.PageNumber = TASK.PageContainerLinks.Count
	else
		return false
	end
	return true
end

function getnameandlink()
	local s = string.format("/category/index_%s.html?sort=name", IncStr(URL))
	if HTTP.GET(MODULE.RootURL .. s) then
		local x = TXQuery.Create(HTTP.Document)
		local p=x.XPathString('//div[@class="page-all-num"]/substring-after(.,"'..getdirpagenumber()..' ")')
		p = tonumber(p)
		if p ~= nil then
			UPDATELIST.CurrentDirectoryPageNumber = p
		end
		local v = x.XPath('//div[contains(@class, "manga-list")]//div[@class="manga-item"]//td[2]/a[1]')
		for i = 1, v.Count do
			LINKS.Add(v.Get(i).GetAttribute('href'):gsub('.html', '') .. '/chapters')
			NAMES.Add(x.XPathString('div', v.Get(i)))
		end
		return no_error
	else
		return net_problem
	end
end

function getimageurl()
	local s = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])
	if HTTP.GET(s) then
		TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//img[contains(@class,"manga_pic")]/@src')
		return true
	else
		return false
	end
end

function Init()
	function AddWebsiteModule(id, name, url, category)
		local m = NewWebsiteModule()
		m.ID               = id
		m.Name             = name
		m.RootURL          = url
		m.Category         = category
		m.OnGetInfo        = 'getinfo'
		m.OnGetPageNumber  = 'getpagenumber'
		m.OnGetNameAndLink = 'getnameandlink'
		m.OnGetImageURL    = 'getimageurl'
	end
	AddWebsiteModule('1230bae145b3452580164d00acc05e6f', 'NiAdd', 'https://www.niadd.com', 'English')
	AddWebsiteModule('482deba9267346418abf3d381712e87a', 'NiAddES', 'https://es.niadd.com', 'Spanish')
	AddWebsiteModule('6f3f9352aa6a4751bfcccc695105c571', 'NiAddIT', 'https://it.niadd.com', 'Italian')
	AddWebsiteModule('b90c9b43d9144fcbab291c19faaaf14e', 'NiAddRU', 'https://ru.niadd.com', 'Russian')
	AddWebsiteModule('9a6c199307064d85b5da627b1ea99c87', 'NiAddBR', 'https://br.niadd.com', 'Portuguese')
	AddWebsiteModule('b4443c9e561c48f0a24983aa36a950d4', 'NiAddDE', 'https://de.niadd.com', 'German')
	AddWebsiteModule('d4dd36e029784572a134f971c7970c0e', 'NiAddFR', 'https://fr.niadd.com', 'French')
end
