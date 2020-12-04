----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'    --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h1[@class="manga-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="show-boxed"]/img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('(//span[@class="spec-item"])[1]'))
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="manga-cat"]/a')
	MANGAINFO.Summary   = x.XPathString('//div[@class="manga-desc"]/p')

	x.XPathHREFAll('//h3[@class="chapter-title"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5ad6ebfe79f04b3b954829180052cb9e'
	m.Name                     = 'SamuraiScan'
	m.RootURL                  = 'https://samuraiscan.com'
	m.Category                 = 'Spanish-Scanlation'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end