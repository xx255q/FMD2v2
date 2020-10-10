json = require "utils.json"
local perpage = 30
local apiurl = 'https://api.mghubcdn.com/graphql'
local cdnurl = 'https://img.mghubcdn.com/file/imghub/'

varX = setmetatable({
	['MangaHubIO'] = 'm01',
	['MangaReaderSite'] = 'mr01',
	['MangaFoxFun'] = 'mf01',
	['MangaKakalotFun'] = 'mn01',
	['MangaHereFun'] = 'mh01',
}, {
	__index = function()
		return 'm01'
	end
})

function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//div[@id="mangadetail"]//h1/text()')
		MANGAINFO.CoverLink = x.XPathString('//div[@id="mangadetail"]//img/@src')
		MANGAINFO.Authors   = x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Author")]/following-sibling::span')
		MANGAINFO.Artists   = x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Artist")]/following-sibling::span')
		MANGAINFO.Genres    = x.XPathStringAll('//div[@id="mangadetail"]//div/p/a')
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Status")]/following-sibling::span'))
		MANGAINFO.Summary   = x.XPathString('//div[contains(@id, "noanim-content-tab-pane")]/div/p')
		local v for v in x.XPath('//div[contains(@id, "noanim-content-tab-pane")]/ul/li/a').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
			MANGAINFO.ChapterNames.Add(x.XPathString('span', v))
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	local chapter = URL:match('/chapter%-(.+)$'):gsub('/$', '')
	local slug = URL:match('/chapter/(.+)/')
	local q = '{"query":"{chapter(x:'..varX[MODULE.Name]..',slug:\\"'..slug..'\\",number:'..chapter..'){id,title,mangaID,number,slug,date,pages,manga{id,title,slug,mainSlug,isWebtoon,isYaoi}}}"}'
	HTTP.MimeType = 'application/json'
	if HTTP.POST(apiurl, q) then
		local i for i in json.decode(HTTP.Document.ToString()).data.chapter.pages:gmatch('":"([^"]+)') do
			print(TASK.PageLinks.Add(cdnurl .. i))
		end
	else
		return false
	end
	return true
end

function getnameandlink()
	local offset = perpage * tonumber(URL)
	local q = '{"query":"{search(x:'..varX[MODULE.Name]..',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:'..tostring(offset)..'){rows{id,title, slug},count}}"}'
	HTTP.MimeType = 'application/json'
	if HTTP.POST(apiurl, q) then
		local data = json.decode(HTTP.Document.ToString())
		local i, v for i, v in ipairs(data.data.search.rows) do
			LINKS.Add('/manga/'..v.slug)
			NAMES.Add(v.title)
		end
		return no_error
	else
		return net_problem
	end
end

function getdirectorypagenumber()
	local q = '{"query":"{search(x:'..varX[MODULE.Name]..',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:0){rows{id,title, slug},count}}"}'
	HTTP.MimeType = 'application/json'
	if HTTP.POST(apiurl, q) then
		local data = json.decode(HTTP.Document.ToString())
		PAGENUMBER = math.floor(data.data.search.count / perpage)
		return no_error
	else
		return net_problem
	end
end

function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID = id
		m.Name = name
		m.RootURL = url
		m.Category = 'English'
		m.OnGetInfo='getinfo'
		m.OnGetPageNumber='getpagenumber'
		m.OnGetNameAndLink='getnameandlink'
		m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
	end
	AddWebsiteModule('a4f873c854b248769284896607dfb4dd', 'MangaHubIO', 'https://mangahub.io')
	AddWebsiteModule('e470dc46f8eb4ac0aa1c9e401969c1f3', 'MangaReaderSite', 'https://mangareader.site')
	AddWebsiteModule('70a69ea951fa4d78920a35f0d5bcb2d5', 'MangaFoxFun', 'https://mangafox.fun')
	AddWebsiteModule('69bf594c54c54a938da633e26291cd3e', 'MangaKakalotFun', 'https://mangakakalot.fun')
	AddWebsiteModule('5000c9841e5c4cc69bdbca334ee5f440', 'MangaHereFun', 'https://mangahere.onl')
end
