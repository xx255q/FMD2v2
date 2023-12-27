----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fb34a56c83f54b19b57a9a92070fe899'
	m.Name                     = 'FlameComics'
	m.RootURL                  = 'https://flamecomics.com'
	m.Category                 = 'English-Scanlation'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaThemesia'
DirectoryPagination = '/series/list-mode/'
-- XPathTokenAuthors   = 'Author'
-- XPathTokenArtists   = 'Artist'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get links and names from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	x = CreateTXQuery(HTTP.Document)
	for v in x.XPath('//*[@id="chapterlist"]//li/a').Get() do
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		MANGAINFO.ChapterNames.Add(x.XPathString('div/div/span[@class="chapternum"]',v))
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	Template.GetPageNumber()

	return no_error
end