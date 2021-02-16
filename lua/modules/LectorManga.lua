function GetInfo()
	if URL:find('/gotobook/',1,true) then
		HTTP.Headers.Values['Referer'] = MODULE.RootURL .. '/library'
	end
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL):gsub('/+$', '') .. '/'
	if not HTTP.GET(MANGAINFO.URL) then return net_problem end
	MANGAINFO.URL=HTTP.LastURL
	x = CreateTXQuery(HTTP.Document)
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
				link = x.XPathString('.//a[contains(@href, "/view_uploads/")]/@href', y)
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
end

function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return false end
	local url = HTTP.LastURL
	if url:find('/paginated') then
		url = url:gsub('/paginated.*$', '/cascade')
		HTTP.Reset()
		HTTP.Headers.Add('Referer: '..HTTP.LastURL)
		if not HTTP.GET(url) then return false end
	end
	CreateTXQuery(HTTP.Document).XPathStringAll('//img[contains(@class,"viewer-image")]/@data-src', TASK.PageLinks)
	return true
end

function GetDirectoryPageNumber()
	-- guessing the last directory page
	PAGENUMBER = 990
	return no_error
end

function GetNameAndLink()
	local url = MODULE.RootURL .. '/library?order_item=creation&order_dir=desc'
	if URL ~= '0' then url = url .. '&page=' .. (URL + 1) end
	if not HTTP.GET(url) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	x.XPathHREFTitleAll('//a[contains(@href,"/gotobook/")]', LINKS, NAMES)
	local p = x.XPathString('//a[@rel="next"]/@href/substring-after(.,"&page=")')
	if p ~= '' then
		p = tonumber(p) or 0
		-- update the last directory page if found
		if p > UPDATELIST.CurrentDirectoryPageNumber then
			UPDATELIST.CurrentDirectoryPageNumber = p
		end
	end
	return no_error
end

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'd68542df8f5b47bbabcab226957598de'
	m.Category                 = 'Spanish'
	m.Name                     = 'LectorManga'
	m.RootURL                  = 'https://lectormanga.com'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.SortedList               = True
end
