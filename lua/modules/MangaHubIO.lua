local perpage = 30
local apiurl = 'https://api.mghubcdn.com/graphql'
local cdnurl = 'https://img.mghubcdn.com/file/imghub/'

function getx()
  if MODULE.Name == 'MangaReaderSite' then
    return "mr01"
  elseif MODULE.Name == 'MangaFoxFun' then
    return "mf01"
  elseif MODULE.Name == 'MangaKakalotFun' then
    return "mn01"
  elseif MODULE.Name == 'MangaHereFun' then
    return "mh01"
  else
    return "m01"
  end
end

function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title=x.XPathString('//div[@id="mangadetail"]//h1/text()')
    MANGAINFO.CoverLink=x.XPathString('//div[@id="mangadetail"]//img/@src')
    MANGAINFO.Authors=x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Author")]/following-sibling::span')
    MANGAINFO.Artists=x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Artist")]/following-sibling::span')
    MANGAINFO.Genres=x.XPathStringAll('//div[@id="mangadetail"]//div/p/a')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@id="mangadetail"]//div/span[contains(., "Status")]/following-sibling::span'))
    MANGAINFO.Summary=x.XPathString('//div[contains(@id, "noanim-content-tab-pane")]/div/p')
    v=x.XPath('//div[contains(@id, "noanim-content-tab-pane")]/ul/li/a')
    for i=1,v.Count do
      v1=v.Get(i)
      MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'))
      MANGAINFO.ChapterNames.Add(x.XPathString('span', v1))
    end
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  local chapter = URL:match('/chapter%-(.+)$'):gsub('/$', '')
  local slug = URL:match('/chapter/(.+)/')
  local q = '{"query":"{chapter(x:'..getx()..',slug:\\"'..slug..'\\",number:'..chapter..'){id,title,mangaID,number,slug,date,pages,manga{id,title,slug,mainSlug,isWebtoon,isYaoi}}}"}'
  HTTP.MimeType = 'application/json'
  if HTTP.POST(apiurl, q) then
    x=TXQuery.Create(HTTP.Document)
    v=x.XPath('json(json(*).data.chapter.pages)/*')
    for i = 1, v.Count do
      v1=v.Get(i)
      TASK.PageLinks.Add(cdnurl .. v1.ToString())
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  local offset = perpage * tonumber(URL)
  local q = '{"query":"{search(x:'..getx()..',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:'..tostring(offset)..'){rows{id,title, slug},count}}"}'
  HTTP.MimeType = 'application/json'
  if HTTP.POST(apiurl, q) then
    x = TXQuery.Create(HTTP.Document)
    x.XPathStringAll('json(*).data.search.rows()/concat("'..MODULE.RootURL..'/manga/", slug)', LINKS)
    x.XPathStringAll('json(*).data.search.rows().title', NAMES)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  local q = '{"query":"{search(x:'..getx()..',q:\\"\\",genre:\\"all\\",mod:ALPHABET,count:true,offset:0){rows{id,title, slug},count}}"}'
  HTTP.MimeType = 'application/json'
  if HTTP.POST(apiurl, q) then
    x = TXQuery.Create(HTTP.Document)
    local total = tonumber(x.XPathString('json(*).data.search.Count'))
    if total == nil then total = 1 end
    PAGENUMBER = math.floor(total / perpage)
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(id, name, url)
  local m = NewWebsiteModule()
  m.ID = id
  m.Name = name
  m.RootURL = URL
  m.Category = 'English'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetNameAndLink='getnameandlink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end 

function Init()
  AddWebsiteModule('a4f873c854b248769284896607dfb4dd', 'MangaHubIO', 'https://mangahub.io')
  AddWebsiteModule('e470dc46f8eb4ac0aa1c9e401969c1f3', 'MangaReaderSite', 'https://mangareader.site')
  AddWebsiteModule('70a69ea951fa4d78920a35f0d5bcb2d5', 'MangaFoxFun', 'https://mangafox.fun')
  AddWebsiteModule('69bf594c54c54a938da633e26291cd3e', 'MangaKakalotFun', 'https://mangakakalot.fun')
  AddWebsiteModule('5000c9841e5c4cc69bdbca334ee5f440', 'MangaHereFun', 'https://mangahere.onl')
end
