function Init()
	local m = NewWebsiteModule()
	m.ID                         = '1fb102cc750a4448b28eb605d508bec2'
	m.Name                       = 'HentaiMimi'
	m.RootURL                    = 'https://hentaimimi.com'
	m.Category                   = 'H-Sites'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.MaxTaskLimit               = 1
	m.MaxConnectionLimit         = 2
	m.SortedList                 = true
end

function GetNameAndLink()
	local dirurl = MODULE.RootURL
	if not HTTP.GET(dirurl) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	local next_url
	while true do
		x.XPathHREFAll('//div[contains(., "All")]/following-sibling::div//h5//a', LINKS, NAMES)
		next_url = x.XPathString('//ul[contains(@class, "pagination")]/li[contains(@class, "active")]/following-sibling::li[1]/a/@href')
		if HTTP.Terminated then break end
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('page=(%d+)') or ''))
		if HTTP.GET(MODULE.RootURL .. next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end
	return no_error
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.CoverLink  = x.XPathString('//meta[@name="og:image"]/@content')
		MANGAINFO.Authors    = x.XPathString('//p[contains(., "Publisher")]/following-sibling::p')
		MANGAINFO.Artists    = x.XPathStringAll('//p[contains(., "Artist")]/following-sibling::p/a')
		MANGAINFO.Title      = '[' .. MANGAINFO.Artists .. '] ' .. x.XPathString('//meta[@name="title"]/@content') .. ' (' .. MANGAINFO.Authors .. ')'
		MANGAINFO.Genres     = x.XPathStringAll('//p[contains(., "Tags")]/following-sibling::p/a')
		MANGAINFO.Summary    = x.XPathString('//meta[@name="description"]/@content')

		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		x.ParseHTML('[' .. GetBetween('var previewImages = [', ']', x.XPathString('//script[contains(., "var previewImages")]')) .. ']')
		local v for v in x.XPath('json(*)()').Get() do
			TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
		end
		return true
	else
		return false
	end
end
