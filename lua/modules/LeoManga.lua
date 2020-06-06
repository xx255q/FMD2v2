----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
XPathTokenStatus    = 'Estado'
-- XPathTokenAuthors   = 'Author(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
XPathTokenGenres    = 'Categorías:'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//section[contains(@class, "container")]//h3)[1]'):gsub('%(Manga%)', '')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="list-group"]//img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('normalize-space(//div[@class="content-wrapper"]//span[contains(b, "' .. XPathTokenStatus .. '")])'):gsub('Estado: ', ''), 'Ongoing', 'Complete')
	MANGAINFO.Genres    = x.XPathStringAll('//b[text()="' .. XPathTokenGenres .. '"]/following-sibling::a')
	MANGAINFO.Summary   = x.XPathString('normalize-space(//div[@class="content-wrapper"]//span[contains(b, "Resumen")])'):gsub('Resumen Resumen: ', '')

	x.XPathHREFAll('//table[contains(@class, "table")]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

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
	m.ID               = '7d2551a1f9b8495da52ea9dffd531c1b'
	m.Name             = 'LeoManga'
	m.RootURL          = 'https://leomanga.me'
	m.Category         = 'Spanish'
	m.OnGetInfo        = 'GetInfo'
	m.OnGetNameAndLink = 'GetNameAndLink'
	m.OnGetPageNumber  = 'GetPageNumber'
end