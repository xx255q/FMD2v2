----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
XPathTokenStatus    = 'Statut'
XPathTokenAuthors   = 'Auteur(s)'
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
XPathTokenGenres    = 'Catégories'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]/span'), 'En cours', 'Complete')
	MANGAINFO.Title     = x.XPathString('(//div[contains(@class, "container")]//h2)[1]/substring-after(., "Manga ")')
	MANGAINFO.Artists   = x.XPathStringAll('//dt[text()="' .. XPathTokenArtists .. '"]/following-sibling::dd[1]')

	v = x.XPath('//ul[@class="chapters888"]/li/h5')
	for i = 1, v.Count do
		MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v.Get(i)))
		MANGAINFO.ChapterNames.Add(x.XPathString('.', v.Get(i)))
	end
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
	m.ID                       = '4dc19ea882d046c2a7ac385757e0a514'
	m.Name                     = 'ScanFR'
	m.RootURL                  = 'https://www.scan-fr.co'
	m.Category                 = 'French'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end
