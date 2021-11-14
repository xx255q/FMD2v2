----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = 'English'
		m.OnGetInfo                = 'GetInfo'
		m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetPageNumber          = 'GetPageNumber'
	end
	AddWebsiteModule('5257a0c426b94accb6dcee3101308314', 'Batoto', 'https://bato.to')
	AddWebsiteModule('41e43d6fa1434937afad3bc04a1e8603', 'Batotoo', 'https://batotoo.com')
	AddWebsiteModule('53347251db9d4d5eb92ef8bc6101e5f7', 'Battwo', 'https://battwo.com')
	AddWebsiteModule('cf8702f7f5d24bd2a1b9b9904beb246b', 'Mangatoto', 'https://mangatoto.com')
end

----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/browse?sort=title.az&page='

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	MANGAINFO.Title     = x.XPathString('//h3[contains(@class, "item-title")]/a') .. GetLanguageCodeSuffix(x.XPathString('//h3[contains(@class, "item-title")]/parent::*/em/@data-lang'))
	MANGAINFO.CoverLink = x.XPathString('//div[contains(@class, "attr-cover")]/img/@src')
	MANGAINFO.Authors   = x.XPathStringAll('//div[@class="attr-item" and (./b="Authors:")]/span/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="attr-item" and (./b="Genres:")]/span/span')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@class="attr-item" and (./b="Release status:")]/span'))
	MANGAINFO.Summary   = x.XPathString('//div[@class="limit-html"]')

	x.XPathHREFAll('//div[contains(@class, "episode-list")]/div[@class="main"]/div/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
	MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

	return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. "1") then return net_problem end

	PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//ul[contains(@class, "pagination")])[2]/li[last()-1]')) or 1

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	if not HTTP.GET(MODULE.RootURL .. DirectoryPagination .. (URL + 1)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local v for v in x.XPath('//div[@id="series-list"]/div/div').Get() do
		LINKS.Add(x.XPathString('a/@href', v))
		NAMES.Add(x.XPathString('a', v) .. GetLanguageCodeSuffix(x.XPathString('em/@data-lang', v)))
	end

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then return net_problem end
	
	local x = CreateTXQuery(HTTP.Document)
	local script = x.XPathString('//script[contains(.,"const batojs")]');
	local server = require("fmd.duktape").ExecJS(script .. [[

var CryptoJS = require("utils/crypto-js.min.js");
JSON.parse(CryptoJS.AES.decrypt(server, batojs).toString(CryptoJS.enc.Utf8));

]])
	if server:find('^//') then server = 'https:' .. server end
	local images = script:match('const images = %[([^%]]+)')
	local i for i in images:gmatch('"([^",]+)') do
		TASK.PageLinks.Add(server .. i)
	end

	return no_error
end

----------------------------------------------------------------------------------------------------
-- Helper Functions
----------------------------------------------------------------------------------------------------

-- Get the language suffix by given flag.
function GetLanguageCodeSuffix(s)
	local suffix = ' [EN]'

	if s and (s ~= '') then
		if s and (s ~= 'en') then suffix = ' [' .. string.upper(s) .. ']' end
	end

	return suffix
end
