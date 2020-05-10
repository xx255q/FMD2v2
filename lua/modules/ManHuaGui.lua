local js = require 'utils.jsunpack'
local lz = require 'utils.lzstring'

function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  HTTP.Cookies.Values['isAdult']=' 1'
  if HTTP.GET(MANGAINFO.URL) then
    x=TXQuery.Create(HTTP.Document)

	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL,x.XPathString('//p[@class="hcover"]/img/@src'))
	MANGAINFO.Title     = x.XPathString('//div[@class="book-title"]/h1')
	MANGAINFO.Authors   = SeparateRight(x.XPathString('//ul[@class="detail-list cf"]/li[2]/span[2]'), '：')
	MANGAINFO.Genres    = SeparateRight(x.XPathString('//ul[@class="detail-list cf"]/li[2]/span[1]'), '：')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="detail-list cf"]/li[@class="status"]'), '连载中')
	MANGAINFO.Summary   = x.XPathString('//div[@id="intro-all"]')

	if x.XPath('//*[@id="checkAdult"]').Count ~= 0 then
		local s = x.XPathString('//*[contains(@class,"chapter")]/input/@value')
		if s~='' then x.ParseHTML(lz.decompressFromBase64(s)) end
	end
	x.XPathHREFTitleAll('//*[contains(@id,"chapter-list")]/ul/li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  local servers = {
    'http://i.hamreus.com',
    'http://us.hamreus.com',
    'http://dx.hamreus.com',
    'http://eu.hamreus.com',
    'http://lt.hamreus.com',
  }

  math.randomseed(os.time())
  math.random(); math.random(); math.random();

  if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
    x=TXQuery.Create(HTTP.Document)
    local s = x.XPathString('//script[contains(., "p,a,c,k")]')
    s = SeparateRight(s, "}('")
    local text = SeparateLeft(s, "',");
    local a = tonumber(GetBetween("',", ",", s))
    s = SeparateRight(s, "',")
    local c = tonumber(GetBetween(",", ",'", s))
    local w = js.splitstr(lz.decompressFromBase64(GetBetween(",'", "'", s)), '|')
    s = js.unpack36(text, a, c, w)
    s = s:gsub('^var%s+.+=%s*{', '{'):gsub('||{};$', ''):gsub('"status":,', '')
    s = GetBetween("SMH.imgData(", ").preInit();", s)
    x.ParseHTML(s)
    local cid = x.XPathString('json(*).cid')
    local md5 = x.XPathString('json(*).sl.md5')
    local PATH = x.XPathString('json(*).PATH')
    local srv = servers[math.random(#servers)]
	local v for _, v in ipairs(x.XPathI('json(*).files()')) do
		TASK.PageLinks.Add(srv .. PATH .. v.ToString() .. '?cid=' .. cid .. '&md5=' .. md5)
	end
    return true
  else
    return false
  end
end

function BeforeDownloadImage()
  HTTP.Headers.Values['Referer'] = MODULE.RootURL
  return true
end

function getdirectorypagenumber()
  if HTTP.GET(MODULE.RootURL .. '/list/') then
    x = TXQuery.Create(HTTP.Document)
    local s = x.XPathString('//div[contains(@id, "AspNetPager")]/a[last()]/@href')
    PAGENUMBER = tonumber(s:match('%d+')) or 1
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if HTTP.GET(MODULE.RootURL..'/list/index_p'..IncStr(URL)..'.html') then
    TXQuery.Create(HTTP.Document).XPathHREFAll('//ul[@id="contList"]/li/p/a',LINKS,NAMES)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID = '39e7f3efe2fd43c6ad0abc68b054cfc7'
  m.Category='Raw'
  m.Name='ManHuaGui'
  m.RootURL='https://www.manhuagui.com'
  m.LastUpdated='February 21, 2018'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetDirectoryPageNumber='getdirectorypagenumber'
  m.OnGetNameAndLink='getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end
