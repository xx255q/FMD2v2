function Init()
	local m = NewWebsiteModule()
	m.ID                         = '06436a82f5994b889e8d63f76fc5c69a'
	m.Name                       = 'MerakiScans'
	m.RootURL                    = 'https://merakiscans.com'
	m.Category                   = 'English-Scanlation'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//*[@id="manga_name"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@id="cover_img"]/@src'))
		MANGAINFO.Authors   = x.XPathString('//ul[@id="detail_list"]/li[contains(., "Author")]/substring-after(., ":")')
		MANGAINFO.Artists   = x.XPathString('//ul[@id="detail_list"]/li[contains(., "Artist")]/substring-after(., ":")')
		MANGAINFO.Genres    = x.XPathStringAll('//ul[@id="detail_list"]/li[contains(., "Genres")]/a')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@id="detail_list"]/li[contains(., "Status")]'))
		MANGAINFO.Summary   = x.XPathString('//ul[@id="detail_list"]/span')
		local v for v in x.XPath('//table[@id="chapter_table"]//tr').Get() do
			MANGAINFO.ChapterLinks.Add(MaybeFillHost(MODULE.RootURL, v.GetAttribute("data-href")))
			MANGAINFO.ChapterNames.Add(x.XPathString('./td[1]', v))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local body     = HTTP.Document.ToString()
		local comicloc = body:match('var comiclocation = "(.-)"')
		local slug     = body:match('var manga_slug = "(.-)"')
		local curCh    = body:match('var currentChapter = "(.-)"')
		local images   = body:match("var images = %[([^%]]+)")
		local i for i in images:gmatch('"([^",]+)') do
			TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, comicloc .. slug .. '/' .. curCh .. '/' .. i))
		end
		return true
	else
		return false
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/manga/') then
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('//div[@id="all"]/div[@id="listitem"]/a').Get() do
			LINKS.Add(v.GetAttribute('href'))
			NAMES.Add(x.XPathString('./h1', v))
		end
		return no_error
	else
		return net_problem
	end
end
