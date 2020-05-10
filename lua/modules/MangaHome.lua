function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'ef797713f63b4032a23bd049019cd350'
	m.Name                       = 'MangaHome'
	m.RootURL                    = 'http://www.mangahome.com'
	m.Category                   = 'English'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnGetImageURL              = 'GetImageURL'
end

local dirurl = '/directory'

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		PAGENUMBER = TXQuery.Create(HTTP.Document).x.XPath('//select/option').Count
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local s = MODULE.RootURL .. dirurl
	if URL ~= '0' then s = s .. '/' .. IncStr(URL) .. '.html' end
	if HTTP.GET(s) then
		TXQuery.Create(HTTP.Document).XPathHREFAll('//div[@class="cover-info"]/p[@class="title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)

		MANGAINFO.CoverLink = x.XPathString('//img[@class="detail-cover"]/@src')
		MANGAINFO.Title     = x.XPathString('//div[@class="manga-detail"]/h1')
		local v, s for _, v in ipairs(x.XPathI('//div[@class="manga-detail"]//p')) do
			s = v.ToString()
			if s:find('Author%(s%):') then MANGAINFO.Authors = s:match('.-:(.*)$')
			elseif s:find('Artist%(s%):') then MANGAINFO.Artists = s:match('.-:(.*)$')
			elseif s:find('Genre%(s%):') then MANGAINFO.Genres = s:match('.-:(.*)$')
			elseif s:find('Status%(s%):') then MangaInfoStatusIfPos(s)
			end
		end
		MANGAINFO.Summary   = x.XPathString('//p[@id="show"]/text()')

		local a = x.XPath('//ul[@class="detail-chlist"]/li/a/@href')
		local v = x.XPath('//ul[@class="detail-chlist"]/li/a/span[1]')
		local t = x.XPath('//ul[@class="detail-chlist"]/li/span[@class="vol"]')
		if (a.Count > 0) and (a.Count == v.Count) and (a.Count == t.Count) then
			local i
			for i = 1, a.Count do
				MANGAINFO.ChapterLinks.Add(a.Get(i).ToString())
				MANGAINFO.ChapterNames.Add(v.Get(i).ToString() .. ' ' .. t.Get(i).ToString())
			end
		end
		InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		TASK.PageNumber = TXQuery.Create(HTTP.Document).XPath('//div[@class="mangaread-pagenav"]/select/option').Count
		return true
	else
		return false
	end
end

function GetImageURL()
	local s = MaybeFillHost(MODULE.RootURL, URL):gsub('/*$', '')
	if WORKID > 0 then s = s .. '/' .. IncStr(WORKID) .. '.html' end
	if HTTP.GET(s) then
		TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//section[@id="viewer"]//img/@src')
		return true
	end
		return false
end
