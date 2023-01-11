----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.ReadMangaRU'
-- DirectoryPagination       = '/'            --> Override template variable by uncommenting this line.
-- DirectoryOffset           = 0              --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	Template.GetInfo()

	return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	Template.GetDirectoryPageNumber()

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	Template.GetNameAndLink()

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local json, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if string.find(URL, 'mtr=1', 1, true) == nil then u = u .. '?mtr=1' end

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	json = GetBetween('[[', ', 0, ', Trim(GetBetween('rm_h.init(', 'false);', x.XPathString('//script[@type="text/javascript" and contains(., "rm_h.init")]'))))
	json = json:gsub('%],%[', ';'):gsub('\'', ''):gsub('"', ''):gsub(']]', ';')
	for i in json:gmatch('(.-);') do
		i1, i2 = i:match('(.-),.-,(.-),.-,.-')
		TASK.PageLinks.Add(i1 .. i2:gsub('?t.-$', ''))
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                       = 'fe152f2481a84befbfa01d7f4312edb9'
	m.Name                     = 'ReadMangaRU'
	m.RootURL                  = 'https://readmanga.live'
	m.Category                 = 'Russian'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetPageNumber          = 'GetPageNumber'
end
