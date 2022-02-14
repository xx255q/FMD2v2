----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.MangaReaderOnline'
-- DirectoryParameters = '/'            --> Override template variable by uncommenting this line.
-- XPathTokenStatus    = 'Status'       --> Override template variable by uncommenting this line.
XPathTokenAuthors   = 'Autor'
XPathTokenArtists   = 'Artista'
XPathTokenGenres    = 'Género'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('(//div[contains(@class, "container")]//h2)[1]')
	MANGAINFO.CoverLink = x.XPathString('//div[@class="boxed"]/img/@src')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//dt[text()="' .. XPathTokenStatus .. '"]/following-sibling::dd[1]/span'), 'Ongoing', 'Complete')
	MANGAINFO.Authors   = x.XPathStringAll('//dt[text()="' .. XPathTokenAuthors .. '"]/following-sibling::dd[1]/a')
	MANGAINFO.Artists   = x.XPathStringAll('//dt[text()="' .. XPathTokenArtists .. '"]/following-sibling::dd[1]/a')
	MANGAINFO.Genres    = x.XPathStringAll('//dt[text()="' .. XPathTokenGenres .. '" or text()="Demográfico"]/following-sibling::dd[1]/a')
	MANGAINFO.Summary   = x.XPathString('//div[@class="well"]/p[2]')

	local v; for v in x.XPath('//ul[@class="chapters"]/li/h5/eee').Get() do
		MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v))
		MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(.)', v))
	end
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
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end
	local body = HTTP.Document.ToString()
	local pages = body:match('var pages = (%[.-%]);')
	if pages then
		local json = require "utils.json"
		local crypto = require "fmd.crypto"
		local baseuri = (body:gsub("[\r\n\t]", ""):match("aaaaaarray%.push%('(.-)'") or ''):gsub('/+$','') .. '/'
		local pages = json.decode(pages)
		local i, v; for i, v in ipairs(pages) do
			if v.external == '0' then
				TASK.PageLinks.Add(baseuri .. v.page_image)
			else
				TASK.PageLinks.Add(crypto.DecodeURL(crypto.DecodeBase64(v.page_image:gsub('^https://', ''))))
			end
		end
	end
	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = '66e4d6861b634c99aad1953099b8d2ac'
	m.Name                     = 'MangAs'
	m.RootURL                  = 'https://mangas.in'
	m.Category                 = 'Spanish'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end
