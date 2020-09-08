local ALPHA_LIST = '#abcdefghijklmnopqrstuvwxyz'

function Init()
	local m = NewWebsiteModule()
	m.ID                         = '23493efd110745a7beb20a6689f4ecd6'
	m.Name                       = 'ReadM'
	m.RootURL                    = 'https://readm.org'
	m.Category                   = 'English'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.TotalDirectory             = ALPHA_LIST:len()
end

function GetNameAndLink()
	local s = ''
	if MODULE.CurrentDirectoryIndex ~= 0 then
		s = ALPHA_LIST:sub(MODULE.CurrentDirectoryIndex+1,MODULE.CurrentDirectoryIndex+1)
	end
	if HTTP.GET(MODULE.RootURL .. '/manga-list/' .. s) then
		local x = CreateTXQuery(HTTP.Document)
		local v; for v in x.XPath('//div[contains(@class, "poster-xs")]/a').Get() do
			LINKS.Add(v.GetAttribute('href'))
			NAMES.Add(x.XPathString('div/h2', v))
		end
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//h1[@class="page-title"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="series-profile-thumb"]/@src'))
		MANGAINFO.Authors   = x.XPathString('//span[@id="first_episode"]/a')
		MANGAINFO.Artists   = x.XPathString('//span[@id="last_episode"]/a')
		MANGAINFO.Genres    = x.XPathStringAll('//div[@class="ui list"]/div/a')
		MANGAINFO.Summary   = x.XPathString('//div[@class="series-summary-wrapper"]/p[2]')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "series-status")]'))

		x.XPathHREFAll('//td[@id="table-episodes-title"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		local h = x.XPathString('//*[@data-homepage]/@data-homepage')
		if h == '' then h = MODULE.RootURL end
		h = h:gsub('/+$','')
		local v, s
		for v in x.XPath('//div[contains(@class,"ch-image-container")]//img').Get() do
			s = v.GetAttribute('src')
			if s:find('^/') then s = h .. s end
			TASK.PageLinks.Add(s)
		end
		return true
	else
		return false
	end
end