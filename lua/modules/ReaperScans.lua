----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb042c961d06479582edb2fa582e3a41'
	m.Name                     = 'ReaperScans'
	m.RootURL                  = 'https://reaperscans.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/comics'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. '?page=' .. (URL + 1)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	if x.XPathString('//div[contains(@class, "p-3")]/a[2]') == '' then return no_error end
	x.XPathHREFAll('//div[contains(@class, "p-3")]/a[2]', LINKS, NAMES)
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[contains(@class, "container")]//h1')
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "overflow-hidden")]/img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@aria-label="card"]//div[./dt="Source Status"]/dd'))
	MANGAINFO.Summary   = x.XPathString('//div[@aria-label="card"]//p')

	local p = 1
	local pages = tonumber(x.XPathString('//span[contains(@class, "z-0")]/span[last()-1]/button')) or 1
	while true do
		local v for v in x.XPath('//div[contains(@class, "mt-6")]//ul[@role="list"]//a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('(.//p)[1]', v))
		end
		p = p + 1
		if p > pages then
			break
		elseif HTTP.GET(MaybeFillHost(MODULE.RootURL, URL .. '?page=' .. tostring(p))) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	CreateTXQuery(HTTP.Document).XPathStringAll('//img[contains(@class, "max-w-full")]/@src', TASK.PageLinks)

	return no_error
end
