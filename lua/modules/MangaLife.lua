----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local DirectoryPagination = '/directory/'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local v, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content'):gsub(' | MangaLife', '')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="mlabel" and contains(., "Status")]/following-sibling::a[1]'))
	MANGAINFO.Authors   = x.XPathString('//span[@class="mlabel" and contains(., "Author")]/following-sibling::a[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@class="mlabel" and contains(., "Genre")]/following-sibling::a')
	MANGAINFO.Summary   = x.XPathString('//span[@class="mlabel" and contains(., "Description")]/following-sibling::div[1]')

	local chapter_uri = x.XPathString('//div[@ng-if]/a/@href'):gsub('%{.*$', '')
	local chapters = x.XPathString('//script[contains(.,"vm.Chapters =")]'):match('(%[.-%])')
	x.ParseHTML(chapters)
	for i, c in ipairs(x.XPathI('json(*)().ChapterName')) do
	MANGAINFO.ChapterNames.Add('Chapter ' .. tostring(i) .. ' ' .. c.ToString())
	MANGAINFO.ChapterLinks.Add(chapter_uri .. '-chapter-' .. tostring(i) .. '.html')
	end

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local x, v = nil
	local u = MODULE.RootURL .. DirectoryPagination

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)

	json = GetBetween('vm.FullDirectory = ', '}]};', x.XPathString('//script[contains(., "vm.FullDirectory = ")]')) .. '}]}'
	json = json:gsub('\\"', ''):gsub('\\u2019', '\''):gsub('&#', '')
	x.ParseHTML(json)
	print(x.XPathString('json(*).Directory()'))
	v = x.XPath('json(*).Directory()')

	for i = 1, v.Count do
	NAMES.Add(x.XPathString('s', v.Get(i)))
		LINKS.Add(MODULE.RootURL .. '/manga/' .. x.XPathString('i', v.Get(i)))
	end

	x.XPathHREFAll('//ul[contains(@class, "manga-list")]/li/a', LINKS, NAMES)

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local chpnum, partnum, dir, host, name, pages, s, x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)
	name = GetBetween('vm.IndexName = ', ';', x.XPathString('//script[contains(., "vm.IndexName = ")]')):gsub('"', '')
	host = GetBetween('vm.CurPathName = ', ';', x.XPathString('//script[contains(., "vm.CurPathName = ")]')):gsub('"', '')
	x.ParseHTML(GetBetween('vm.CurChapter = ', ';', x.XPathString('//script[contains(., "vm.CurChapter = ")]')))
	pages = tonumber(x.XPathString('json(*).Page'))
	chpnum, partnum = x.XPathString('json(*).Chapter'):match('%d(%d%d%d%d)(%d)')
	dir = x.XPathString('json(*).Directory')

	if partnum ~= '0' then chpnum = chpnum .. '.' .. partnum end
	if dir ~= '' then chpnum = dir .. '/' .. chpnum end

	for i = 1, pages do
		TASK.PageLinks.Add(host .. '/manga/' .. name .. '/' .. chpnum .. '-' .. string.format('%03d', i) .. '.png')
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	local m = NewWebsiteModule()
	m.ID                    = '4c1e8bcc433d4ebca8e6b4d86ce100cf'
	m.Name                  = 'MangaLife'
	m.RootURL               = 'https://manga4life.com'
	m.Category              = 'English'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetNameAndLink      = 'GetNameAndLink'
	m.OnGetPageNumber       = 'GetPageNumber'
end