local API_URL = 'https://img.softkomik.online/api'
local CDN_URL = 'https://cdn.statically.io/img/softkomik.online'

function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'df01551e1739407a98669e37318842b0'
	m.Name                       = 'SoftKomik'
	m.RootURL                    = 'https://softkomik.site'
	m.Category                   = 'Indonesian'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if HTTP.GET(API_URL .. '/komik-list?page=' .. '1') then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('json(*).DataByName.last_page')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(API_URL .. '/komik-list?page=' .. (URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('json(*).DataByName.data()').Get() do
			LINKS.Add(API_URL .. '/lihat-komik/' .. x.XPathString('title_slug', v))
			NAMES.Add(x.XPathString('title', v))
		end
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	local slug = URL:match('/(.-)$')
	if HTTP.GET(API_URL .. '/lihat-komik/' .. slug) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('json(*).DataKomik.title')
		MANGAINFO.CoverLink  = CDN_URL .. '/' .. x.XPathString('json(*).DataKomik.gambar')
		MANGAINFO.Authors    = x.XPathString('json(*).DataKomik.author')
		MANGAINFO.Genres     = x.XPathStringAll('json(*).dataGenre().nama_genre')
		MANGAINFO.Status     = MangaInfoStatusIfPos(x.XPathString('json(*).DataKomik.status'))
		MANGAINFO.Summary    = x.XPathString('json(*).DataKomik.sinopsis')

		local v for v in x.XPath('json(*).DataChapter()').Get() do
			MANGAINFO.ChapterLinks.Add(slug .. '&' .. x.XPathString('chapter', v))
			MANGAINFO.ChapterNames.Add(x.XPathString('chapter', v))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(API_URL .. '/baca-chapter' .. URL) then
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('json(*).DataGambar()').Get() do
			TASK.PageLinks.Add(CDN_URL .. '/' .. x.XPathString('url_gambar', v))
		end
		return true
	else
		return false
	end
end
