----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '770b018081a74cb1ae983233d318ff87'
	m.Name                     = 'ComicFx'
	m.RootURL                  = 'https://comicfx.net'
	m.Category                 = 'Indonesian'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
-- XPathTokenStatus    = 'Status'       --> Override template variable by uncommenting this line.
-- XPathTokenAuthors   = 'Author(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenArtists   = 'Artist(s)'    --> Override template variable by uncommenting this line.
-- XPathTokenGenres    = 'Categories'   --> Override template variable by uncommenting this line.

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
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//div[contains(@class, "container")]//h2)[1]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="thumb"]/img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//b[text()="Status:"]/following-sibling::i'))
	MANGAINFO.Authors   = x.XPathStringAll('//b[text()="Author:"]/following-sibling::a')
	MANGAINFO.Artists   = x.XPathStringAll('//b[text()="Artists:"]/following-sibling::a')
	MANGAINFO.Genres    = x.XPathStringAll('//b[text()="Genres:"]/following-sibling::a')
	MANGAINFO.Summary   = x.XPathString('//div[@class="sinopsis"]/p')

	x.XPathHREFTitleAll('//div[@class="chaplist"]/ul/li/div/span[1]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local body = HTTP.Document.ToString()
	local pages = body:match('var pages = (%[.-%]);')
	if pages then
		local json = require "utils.json"
		local crypto = require "fmd.crypto"
		local pages = json.decode(pages)
		local i, v; for i, v in ipairs(pages) do
			TASK.PageLinks.Add(crypto.DecodeURL(crypto.DecodeBase64(v.page_image:gsub('https://img.comicfx.net/img/.-/', ''):gsub('.jpg', ''))):gsub('i%d.wp.com/', ''):gsub('cdn.statically.io/img/', ''))
		end
	end

	return no_error
end
