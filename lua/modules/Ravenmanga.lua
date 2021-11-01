----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '3a3f61e584334c26923cfcb160a5f50b'
	m.Name                     = 'Ravenmanga'
	m.RootURL                  = 'https://ravenmanga.xyz'
	m.Category                 = 'Spanish'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comic'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	local dirurl = MODULE.RootURL .. DirectoryPagination
	if not HTTP.GET(dirurl) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	local next_url
	while true do
		x.XPathHREFAll('//a[contains(@class, "link_title")]', LINKS, NAMES)
		next_url = x.XPathString('//ul[@class="pagination"]/li[last()]/a/@href')
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
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div/span[@class="info-span"]')
	MANGAINFO.CoverLink = x.XPathString('//img[@class="image_manga"]/@src')
	MANGAINFO.Genres    = x.XPathStringAll('//div[contains(., "Generos")]/following-sibling::div/span')
	MANGAINFO.Summary   = x.XPathString('//div[contains(@class, "text-muted")]')

	x.XPathHREFAll('//a[contains(@class, "cap-link")]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[contains(@class, "container-image")]//img/@src', TASK.PageLinks)

	return no_error
end
