local domain = 'gmanga.me'
local mediaUrl = 'https://media.' .. domain .. '/uploads'

function getinfo()
	function urlencode(str)
		if (str) then
			str = string.gsub(str, "\n", "\r\n")
			str = string.gsub(str, "([^%w ])",
				 function (c) return string.format ("%%%02X", string.byte(c)) end)
			str = string.gsub(str, " ", "_")
			str = string.gsub(str, '%.', '')
		end
		return str
	end

	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		local s = x.XPathString('//script[@type="application/json" and @class]')
		x.ParseHTML(s)
		local pageurl = x.XPathString('json(*).globals.pageUrl')
		local id = x.XPathString('json(*).mangaDataAction.mangaData.ID')
		local cover = x.XPathString('json(*).mangaDataAction.mangaData.cover')
		MANGAINFO.CoverLink = mediaUrl .. '/manga/cover/' .. id .. '/' .. cover
		if MANGAINFO.Title == '' then
			MANGAINFO.Title=x.XPathString('json(*).mangaDataAction.mangaData.title')
		end
		MANGAINFO.Authors=x.XPathStringAll('json(*).mangaDataAction.mangaData.authors().name')
		MANGAINFO.Artists=x.XPathStringAll('json(*).mangaDataAction.mangaData.artists().name')
		MANGAINFO.Genres=x.XPathStringAll('json(*).mangaDataAction.mangaData.categories().name')
		MANGAINFO.Status=MangaInfoStatusIfPos(x.XPathString('json(*).mangaDataAction.mangaData.status'),'publish','finish')
		MANGAINFO.Summary=x.XPathString('json(*).mangaDataAction.mangaData.summary')
		if HTTP.XHR(MaybeFillHost(MODULE.RootURL, '/api/mangas/' .. id)) then
			x = CreateTXQuery(HTTP.Document)
			local v = x.XPath('json(*).mangaReleases()')
			local t = {}
			local data = {}
			for i = 1, v.Count do
				local v1 = v.Get(i)
				local ch = tonumber(x.XPathString('chapter', v1))
				local team = x.XPathString('team_name', v1)
				local key = string.format('%08.2f %s', ch, team)

				table.insert(t, key)
				data[key] = {
					name = string.format('%s - %s [%s]', tostring(ch), x.XPathString('title', v1), team),
					link = MaybeFillHost(MODULE.RootURL, pageurl .. '/' .. tostring(ch) .. '/' .. urlencode(team))
				}
			end
			table.sort(t)
			for _, k in ipairs(t) do
				MANGAINFO.ChapterLinks.Add(data[k].link)
				MANGAINFO.ChapterNames.Add(data[k].name)
			end
			return no_error
		else
			return net_problem
		end
	else
		return net_problem
	end
end

function getpagenumber()
	local js = require 'utils.jsunpack'
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		local x = CreateTXQuery(HTTP.Document);
		local s = x.XPathString('//script[@type="application/json" and @class]')
		x.ParseHTML(s)
	local mediakey = x.XPathString('json(*).globals.mediaKey')
		local pages = js.splitstr(x.XPathString('json(*).readerDataAction.readerData.release.hq_pages'), '\r\n')
		for _, k in ipairs(pages) do
			TASK.PageLinks.Add(mediaUrl .. '/releases/' .. k .. '?ak=' .. mediakey)
		end
		return true
	else
		return false
	end
	return true
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL..'/mangas') then
		local x = CreateTXQuery(HTTP.Document);
		local s = HTMLDecode(x.XPathString('//*[@data-store-name="mangasIndexStore"]/@data-props'))
		x.ParseHTML(s)
		local v = x.XPath('json(*).mangas()')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			NAMES.Add(x.XPathString('title', v1))
			LINKS.Add(MaybeFillHost(MODULE.RootURL, '/mangas/' .. x.XPathString('id', v1) .. '/' .. x.XPathString('slug', v1)))
		end
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID               = '0e9af4ffd93545989facf8d5090e5a8c'
	m.Category         = 'Arabic'
	m.Name             = 'GManga'
	m.RootURL          = 'https://' .. domain
	m.OnGetInfo        = 'getinfo'
	m.OnGetPageNumber  = 'getpagenumber'
	m.OnGetNameAndLink = 'getnameandlink'
end
