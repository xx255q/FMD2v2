local dirurl = '/browse/?genre=All&results=%s&filter=New'

function Init()
	local m = NewWebsiteModule()
	m.ID                         = '8b08552360e14892a2b715dab6957bfe'
	m.Name                       = 'MangaRaw'
	m.RootURL                    = 'https://www.manga-raw.club'
	m.Category                   = 'Raw'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['includeraw'] = 'Show [RAW] chapters',
			['includeen'] = 'Show [EN] chapters'
		},
		['id_ID'] = {
			['includeraw'] = 'Tampilkan bab [RAW]',
			['includeen'] = 'Tampilkan bab [EN]'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('luaincludeen', lang:get('includeen'), true)
	m.AddOptionCheckBox('luaincluderaw', lang:get('includeraw'), true)
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. dirurl:format('1')) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//div[@class="catalog"]//li[@class="paginator__item"]')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. dirurl:format((URL + 1))) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//h3[@class="card__title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//h1[@class="section__title"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="card__cover"]/img/@src'))
		MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="card__meta"]/li[contains(.,"Tags")]/a')
		MANGAINFO.Status    = x.XPathString('//ul[@class="card__meta"]//span[contains(.,"Status")]/parent::*')
		MANGAINFO.Summary   = x.XPathString('//p[@class="comments__text"]')

	if MODULE.GetOption('luaincludeen') then
		local v; for v in x.XPath('//div[@id="tab-1"]//a').Get() do
			MANGAINFO.ChapterNames.Add(x.XPathString('replace(., "-eng-li", " [EN]")', v))
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		end
	end
	if MODULE.GetOption('luaincluderaw') then
		local v; for v in x.XPath('//div[@id="tab-3"]//a').Get() do
			MANGAINFO.ChapterNames.Add(x.XPathString('text()', v) .. ' [RAW]')
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
		end
	end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="tab-1"]//img/@src', TASK.PageLinks)
		return true
	else
		return false
	end
end