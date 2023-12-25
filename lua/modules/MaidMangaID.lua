----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '7a74b2abda1d4b329ee1d1fa58866c03'
	m.Name                     = 'MaidMangaID'
	m.RootURL                  = 'https://www.maid.my.id'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
DirectoryPagination = '/manga-list/'
-- XPathTokenAuthors   = 'Author'
-- XPathTokenArtists   = 'Artist'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	CreateTXQuery(HTTP.Document).XPathHREFAll('//div[@class="mangalist-blc"]//a', LINKS, NAMES)

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//div[@class="series-title"]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="series-thumb"]/img/@src')
	MANGAINFO.Authors   = x.XPathString('//li[contains(b, "Author")]/span')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="series-genres"]/a')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "status")]'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="series-synops"]/string-join(.//text(), " ")')

	for v in x.XPath('//ul[@class="series-chapterlist"]//a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('span[1]/text()', v))
	end		
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="reader-area"]//img/@src', TASK.PageLinks)

	return no_error
end