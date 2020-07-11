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
	Template.GetInfo()

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
		local baseuri = (body:match("array%.push%('(.-)'") or ''):gsub('/+$','') .. '/'
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