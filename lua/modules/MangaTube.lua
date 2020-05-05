function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'fc8d20df870e4f70853dac8141b34459'
	m.Name                       = 'MangaTube'
	m.RootURL                    = 'https://manga-tube.me'
	m.Category                   = 'German'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/series/?filter=alphabetic') then
		PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).x.XPathString('//div[@id="series_list"]/@data-series-pages')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local data = 'action=load_series_list_entries&parameter%5Bpage%5D=' .. IncStr(URL) .. '&parameter%5Bletter%5D=&parameter%5Bsortby%5D=alphabetic&parameter%5Border%5D=asc'
	if HTTP.POST(MODULE.RootURL .. '/ajax', data) then
		local x = TXQuery.Create(HTTP.Document)
		x.XPathStringAll('json(*).success().manga_title', NAMES)
		local v for _,v in ipairs(x.XPathI('json(*).success().manga_slug')) do
			LINKS.Add(MODULE.RootURL .. '/series/' .. v.ToString())
		end
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)
		
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.URL, x.XPathString('//div[contains(@class, "cover")]//img/@data-original'))
		MANGAINFO.Title     = x.XPathString('//h1[@class="series-title"]')
		MANGAINFO.Authors   = x.XPathStringAll('//ul[contains(@class, "series-details")]/li[contains(., "Autor")]/a')
		MANGAINFO.Artists   = x.XPathStringAll('//ul[contains(@class, "series-details")]/li[contains(., "Artist")]/a')
		MANGAINFO.Genres    = SeparateRight(x.XPathString('//*[@class="row"][starts-with(.,"Genre")]'),':')
		MANGAINFO.Summary   = x.XPathString('//h4[text()="Beschreibung"]/following-sibling::text()[1]')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[contains(@class, "series-details")]/li[contains(., "Status (Offiziell):")]/text()'), 'laufend', 'abgeschlossen')
		
		local v for _,v in ipairs(x.XPathI('//ul[contains(@class, "chapter-list")]/li/a[contains(@href, "read/")]')) do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('concat(b, " ", span[1])',v))
		end
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = TXQuery.Create(HTTP.Document)
		local s = x.XPathString('//script[contains(., "img_path: ")]')
		local img_path = s:match("img_path:%s*'(.-)'")
		s = s:match('pages:%s*(%[.-%])')
		if img_path and s then
			x.ParseHTML(s)
			local v for _,v in ipairs(x.XPathI('json(*)().file_name')) do
				TASK.PageLinks.Add(img_path .. v.ToString())
			end
		end
		return true
	else
		return false
	end
end
