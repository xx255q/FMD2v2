function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'fa2359317a72416a958a16682050acb0'
	m.Name                       = 'Manga1000'
	m.RootURL                    = 'https://manga1000.com'
	m.Category                   = 'Raw'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetNameAndLink()
	local dirurl = MODULE.RootURL .. '/manga/newmanga'
	if not HTTP.GET(dirurl) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	local next_url
	while true do
		x.XPathHREFAll('//h3[@class="entry-title"]/a',LINKS,NAMES)
		next_url = x.XPathString('//a[contains(@class, "next page-numbers")]/@href')
		if HTTP.Terminated then break end
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('page/(%d+)/') or ''))
		if HTTP.GET(next_url) then
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

		MANGAINFO.CoverLink = x.XPathString('//div[@class="wp-block-image"]//img/@src')
		MANGAINFO.Title     = Trim(SeparateLeft(x.XPathString('//h1[@class="entry-title"]'), "(Raw – Free)"))
		MANGAINFO.Genres    = x.XPathStringAll('//div[@class="entry-content"]/p[1]/a')
		MANGAINFO.Summary   = x.XPathString('//div[@class="entry-content"]/p[2]')

		x.XPathHREFAll('//table[contains(@class, "table")]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('//figure[@class="wp-block-image"]/img').Get() do
			local src = v.GetAttribute('src')
			if v.GetAttribute('data-src') ~= '' then
				src = v.GetAttribute('data-src')
			end
		TASK.PageLinks.Add(src)
		end
		return true
	else
		return false
	end
end