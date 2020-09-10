function Init()
	local m = NewWebsiteModule()
	m.ID                         = '41782b8acd6a4f17af5bcc13c18e6b40'
	m.Name                       = 'MangaForFree'
	m.RootURL                    = 'http://mangaforfree.com'
	m.Category                   = 'English'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/manga/') then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[contains(@class, "page-nav")]/a[@class="last"]'))
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/manga/page/' .. (URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//h3[contains(@class, "entry-title")]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//h1[@class="entry-title"]')
		MANGAINFO.CoverLink  = x.XPathString('//div[@class="td-post-featured-image"]//a/@href')
		MANGAINFO.Genres     = x.XPathString('//div[@class="td-post-content"]/p[2]/span/span')
		if MANGAINFO.Genres  == '' then MANGAINFO.Genres = x.XPathString('//div[@class="td-post-content"]/p[2]/span[2]') end
		MANGAINFO.Status     = MangaInfoStatusIfPos(x.XPathString('//div[@class="td-post-content"]/p[1]/span/strong/span[2]'))
		if MANGAINFO.Status  == '' then MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="td-post-content"]/p[1]/span/span[2]')) end
		if MANGAINFO.Status  == '' then MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="td-post-content"]/p[1]/span/span')) end
		MANGAINFO.Summary    = x.XPathString('//div[@class="td-post-content"]/p[3]/span[2]')
		if MANGAINFO.Summary == '' then MANGAINFO.Summary = x.XPathString('//div[@class="td-post-content"]/p[3]/span') end

		x.XPathHREFAll('//tbody[@class="row-hover"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		if MANGAINFO.ChapterLinks.Count < 1 then
			x.XPathHREFAll('//div[@class="td-post-content"]/p[3]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		if MANGAINFO.ChapterLinks.Count < 1 then
			x.XPathHREFAll('//div[@class="td-post-content"]/p[5]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		if MANGAINFO.ChapterLinks.Count < 1 then
			x.XPathHREFAll('//tr/td/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		if MANGAINFO.ChapterLinks.Count < 1 then
			if HTTP.GET(x.XPathString('//a[contains(., "List")]/@href')) then
				CreateTXQuery(HTTP.Document).XPathHREFAll('//tbody[@class="row-hover"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
				MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
				return no_error
			else
				return net_problem
			end
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="td-post-content"]//a/@href', TASK.PageLinks)
		return true
	else
		return false
	end
end