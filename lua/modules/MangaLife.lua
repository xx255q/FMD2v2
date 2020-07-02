----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//meta[@property="og:title"]/@content'):gsub(' | %w+$', '')
	MANGAINFO.CoverLink = x.XPathString('//meta[@property="og:image"]/@content')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="mlabel" and contains(., "Status")]/following-sibling::a[1]'))
	MANGAINFO.Authors   = x.XPathString('//span[@class="mlabel" and contains(., "Author")]/following-sibling::a[1]')
	MANGAINFO.Genres    = x.XPathStringAll('//span[@class="mlabel" and contains(., "Genre")]/following-sibling::a')
	MANGAINFO.Summary   = x.XPathString('//span[@class="mlabel" and contains(., "Description")]/following-sibling::div[1]')

	local chapter_uri = x.XPathString('//a[@ng-repeat]/@href'):gsub('{{.+$', '')
	local body = HTTP.Document.ToString()
	local vm_Chapters = body:match('vm%.Chapters%s*=%s*(%[.-%]);')
	local vm_PageOne = body:match('vm%.PageOne%s*=%s*"(.-)"')

	-- direct translation from "function MainFunction($http){..."
	local vm_ChapterDisplay = function(e)
		local t = tonumber(e:sub(2, -2))
		local n = e:sub(-1)
		if '0' ~= n then t = t .. "." .. n end
		return t
	end

	local vm_ChapterURLEncode = function(e)
		local Index = ""
		local t = e:sub(1, 1)
		if 1 ~= t then Index = "-index-" .. t end
		local n = tonumber(e:sub(2, -2))
		local m = ""
		local a = e:sub(-1)
		if '0' ~= a then m = "." .. a end
		return "-chapter-" .. n .. m .. Index .. vm_PageOne .. ".html"
	end

	x.ParseHTML(vm_Chapters)
	local chapter, chapterType, chapterName
	local v; for v in x.XPath("json(*)()").Get() do
		chapter = x.XPathString("Chapter", v)
		chapterType = x.XPathString("Type", v)
		chapterName = x.XPathString("ChapterName", v)
		if chapterType == "" then chapterType = "Chapter" end
		MANGAINFO.ChapterLinks.Add(chapter_uri .. vm_ChapterURLEncode(chapter))
		MANGAINFO.ChapterNames.Add(chapterType .. " " .. vm_ChapterDisplay(chapter) .. " " .. chapterName)
	end
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local x, v = nil
	local u = MODULE.RootURL .. '/directory/'

	if not HTTP.GET(u) then return net_problem end

	x = CreateTXQuery(HTTP.Document)

	json = GetBetween('vm.FullDirectory = ', '}]};', x.XPathString('//script[contains(., "vm.FullDirectory = ")]')) .. '}]}'
	json = json:gsub('\\"', ''):gsub('\\u2019', '\''):gsub('&#', '')
	x.ParseHTML(json)
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
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                    = id
		m.Name                  = name
		m.RootURL               = url
		m.Category              = 'English'
		m.OnGetInfo             = 'GetInfo'
		m.OnGetNameAndLink      = 'GetNameAndLink'
		m.OnGetPageNumber       = 'GetPageNumber'
	end

	AddWebsiteModule('4c1e8bcc433d4ebca8e6b4d86ce100cf', 'MangaLife', 'https://manga4life.com')
	AddWebsiteModule('3db42782cfc441e3a3498afa91f70a80', 'MangaSee', 'https://mangasee123.com')
end