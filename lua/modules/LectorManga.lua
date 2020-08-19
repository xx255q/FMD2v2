function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL..'/library') then
			CreateTXQuery(HTTP.Document).XPathHREFTitleAll('//a[contains(@href,"/gotobook/")]', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL):gsub('/+$', '') .. '/'
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//h1/text()')
		MANGAINFO.CoverLink = x.XPathString('//img[@class="img-fluid"]/@src')
		MANGAINFO.Summary   = x.XPathString('//div/p')
		MANGAINFO.Genres    = x.XPathStringAll('//a[contains(@href, "/library?genders")]')

		local function parseChapters(s)
			local v, y
			local ctitle, stitle, title, link
			for v in x.XPath('//div[@id="chapters'..s..'"]/div[@class="row"]').Get() do
				ctitle = x.XPathString('./div/h4[contains(@class,"text-truncate")]', v)
				for y in x.XPath('./following-sibling::ul[1]/li', v).Get() do
					link = x.XPathString('.//a[contains(@href, "/viewer/")]/@href', y)
					stitle = x.XPathString('./div/div[contains(@class,"text-truncate")]/span', y)
					title = ctitle
					if stitle ~= '' then title = title .. ' [' .. stitle .. ']' end
					MANGAINFO.ChapterLinks.Add(link)
					MANGAINFO.ChapterNames.Add(title)
				end
			end
		end
		parseChapters('')
		parseChapters('-collapsed')
		
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local url = HTTP.LastURL
		if url:find('/paginated') then
			url = url:gsub('/paginated.*$', '/cascade')
			HTTP.Reset()
			HTTP.Headers.Add('Referer: '..HTTP.LastURL)
			if not HTTP.GET(url) then return false end
		end
		CreateTXQuery(HTTP.Document).XPathStringAll('//img[contains(@class,"viewer-image")]/@data-src', TASK.PageLinks)
		return true
	else
		return false
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID               = 'd68542df8f5b47bbabcab226957598de'
	m.Category         = 'Spanish'
	m.Name             = 'LectorManga'
	m.RootURL          = 'https://lectormanga.com'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetPageNumber  = 'GetPageNumber'
end
