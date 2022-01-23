function Init()
	local m = NewWebsiteModule()
	m.ID                         = '1a7b98800a114a3da5f48de91f45a880'
	m.Name                       = 'ReadComicOnline'
	m.RootURL                    = 'https://readcomiconline.li'
	m.Category                   = 'English'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.SortedList                 = true

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['datasaver'] = 'Data saver'
		},
		['id_ID'] = {
			['datasaver'] = 'Penghemat data'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('datasaver', lang:get('datasaver'), false)
end

function GetDirectoryPageNumber()
	local url = MODULE.RootURL .. '/ComicList/Newest'
	if HTTP.GET(url) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pager"]/li[last()]/a/@href'):match('=(%d+)$')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local url = MODULE.RootURL .. '/ComicList/Newest'
	if URL ~= '0' then
		url = url .. '?page=' .. (URL + 1)
	end
	if HTTP.GET(url) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//table[@class="listing"]/tbody/tr/td[1]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//a[@class="bigChar"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@id="rightside"]//img/@src'))
		MANGAINFO.Authors   = x.XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Author") or starts-with(., "Writer")]/parent::*/a')
		MANGAINFO.Artists   = x.XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Artist")]/parent::*/a')
		MANGAINFO.Summary   = x.XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Summary:")]//following-sibling::p[1]')
		MANGAINFO.Genres    = x.XPathStringAll('//div[@class="barContent"]//span[starts-with(., "Genre")]/parent::*/a')
		MANGAINFO.Status    = MangaInfoStatusIfPos((x.XPathString('//div[@class="barContent"]/div/p[starts-with(.,"Status:")]')))
		x.XPathHREFAll('//table[@class="listing"]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local u = MaybeFillHost(MODULE.RootURL, URL .. '&quality=hq')
	if MODULE.GetOption('datasaver') then
		u = MaybeFillHost(MODULE.RootURL, URL .. '&quality=lq')
	end
	sleep(3000)
	if HTTP.GET(u) then
		local body = HTTP.Document.ToString()
		local s = body:match('var%s+lstImages%s+.-;(.-)%s+var%s')
		local i; for i in s:gmatch('%("(.-)"%)') do
			TASK.PageLinks.Add(i)
		end
		return true
	else
		return false
	end
end
