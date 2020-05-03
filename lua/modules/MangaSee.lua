function Init()
	local m = NewWebsiteModule()
	m.ID                         = '3db42782cfc441e3a3498afa91f70a80'
	m.Name                       = 'MangaSee'
	m.RootURL                    = 'https://mangaseeonline.us'
	m.Category                   = 'English'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/directory/') then
		TXQuery.Create(HTTP.Document).x.XPathHREFAll('//*[@id="content"]//a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)
		
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.URL, x.XPathString('//meta[@property="og:image"]/@content'))
		MANGAINFO.Title     = x.XPathString('//*[@class="row"]//h1')
		MANGAINFO.Authors   = SeparateRight(x.XPathString('//*[@class="row"][starts-with(.,"Author")]'),':')
		MANGAINFO.Artists   = SeparateRight(x.XPathString('//*[@class="row"][starts-with(.,"Artist")]'),':')
		MANGAINFO.Genres    = SeparateRight(x.XPathString('//*[@class="row"][starts-with(.,"Genre")]'),':')
		MANGAINFO.Summary   = Trim(SeparateRight(x.XPathString('//*[@class="row"][starts-with(.,"Description")]'),':'))
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//*[@class="row"][starts-with(.,"Status")]'))
		
		local s,v for _,v in ipairs(x.XPathI('//div[@class="list chapter-list"]//a')) do
			s = v.GetAttribute('href')
			if s:find('%-page%-1') then
			  s = s:gsub('%-page%-1','')
			end
			MANGAINFO.ChapterLinks.Add(s)
			MANGAINFO.ChapterNames.Add(x.XPathString('span[@class="chapterLabel"]',v))
		end
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		TXQuery.Create(HTTP.Document).XPathStringAll('//*[contains(@class,"image-container")]//img/@src', TASK.PageLinks)
		return true
	else
		return false
	end
end
