----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local cat = 'Spanish-Scanlation'
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = cat
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('41294a121062494489sdqt601c542ef0', 'TempleScanEsp', 'https://templescanesp.net')

	cat = 'Spanish'
	AddWebsiteModule('41294a121062494489rwra601c542efg', 'ManwhasOnline', 'https://manwhasonline.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = {
	['41294a121062494489sdqt601c542ef0'] = '/comics', -- TempleScanEsp
	['41294a121062494489rwra601c542efg'] = '/doujin' -- ManwhasOnline
}

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local x = nil
	local next_url = ''
	local u = MODULE.RootURL .. DirectoryPagination[MODULE.ID]

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	while true do
		x.XPathHREFAll('//a[@class="rounded-md"]', LINKS, NAMES)
		next_url = x.XPathString('//section[@class="flex flex-col gap-6"]/div[3]/a[.="Next"]/@href')
		if HTTP.Terminated then break end
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('page=(%d+)') or ''))
		if HTTP.GET(next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end
	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "max-w-80 w-full")]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//div/div[contains(., "Autor")]/following-sibling::div/text()')
	MANGAINFO.Genres    = x.XPathStringAll('//div/div[contains(., "Generos") or contains(., "Genders")]/following-sibling::div/a')
	MANGAINFO.Summary   = x.XPathString('//section[@id="section-sinopsis"]/p')

	if MODULE.ID == '41294a121062494489rwra601c542efg' then -- ManwhasOnline
		for v in x.XPath('//*[@id="section-list-cap"]//a[1]').Get() do
			if x.XPathString('div[@id="name"]', v) ~= '' then
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('div[@id="name"]', v))
			end
		end
	else
		for v in x.XPath('//*[@id="section-list-cap"]//a[2]').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('.//div[@id="name"]', v))
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//section//img/@src', TASK.PageLinks)
	for i = 0, TASK.PageLinks.Count - 1 do 
		TASK.PageLinks[i] = TASK.PageLinks[i]:gsub("cdn.statically.io/img/", "")
		i = i + 1
	end

	return no_error
end
