function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title=x.XPathString('//h1/span[@class="name"]')
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@itemprop="image"]/@src'))
    MANGAINFO.Genres=x.XPathStringAll('//ul[@class="tagList"]/li/a')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "status_tag")]'), 'выходит', 'издано')
    MANGAINFO.Summary=x.XPathString('//div[@itemprop="description"]')
    x.XPathHREFAll('//ul[@class="chlist"]/li/h4/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  TASK.PageNumber=0
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
    local x=TXQuery.Create(HTTP.Document)
    local s = x.XPathString('//script[contains(., "Reader.init")]')
    s = GetBetween('.init(', ');', s)
    x.ParseHTML(s)
    local dir = x.XPathString('json(*).dir')
    local v = x.XPath('json(*).images()()')
    for i = 1,v.Count,3 do
      local v1=v.Get(i)
      TASK.PageLinks.Add(dir .. v1.ToString())
    end
  else
    return false
  end
  return true
end

local dirurl = '/manga/?order_by=name&page='
function getnameandlink()
  if HTTP.GET(MODULE.RootURL .. dirurl .. IncStr(URL)) then
    local x = TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('//div[@class="animeList"]/ol/li/div/h3/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if HTTP.GET(MODULE.RootURL .. dirurl .. '1') then
    x = TXQuery.Create(HTTP.Document)
    PAGENUMBER = tonumber(x.XPathString('//div[@class="PageNav"]/nav/a[last()-1]')) or 1
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID = '41e78386ff3447e7a283b6ce55950f0f'
  m.Name = 'DesuMe'
  m.RootURL = 'https://desu.me'
  m.Category = 'Russian'
  m.LastUpdated='March 3, 2018'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetNameAndLink='getnameandlink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
end