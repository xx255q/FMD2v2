function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title=x.XPathString('//meta[@name="title"]/@content')
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//*[@class="bt_thumb"]/a/img/@src'))
    MANGAINFO.Authors=x.XPathString('//meta[@name="author"]/@content')
    MANGAINFO.Summary=x.XPathString('//meta[@name="description"]/@content')
    local v = x.XPath('//table[@class="web_list"]/tbody//tr/td[@class="content__title"]')
    for i = 1, v.Count do
      local v1 = v.Get(i)
      MANGAINFO.ChapterLinks.Add(v1.GetAttribute('data-role'))
      MANGAINFO.ChapterNames.Add(v1.ToString())
    end
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
    local x=TXQuery.Create(HTTP.Document)
    local s = x.XPathString('//script[contains(., "toon_img")]')
    x.ParseHTML(DecodeBase64(GetBetween("toon_img = '", "';", s)))
    local v = x.XPath('//img/@src')
    for i = 1, v.Count do
      TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.Get(i).ToString()))
    end
    TASK.PageContainerLinks.Values[0] = HTTP.Cookies.Text
  else
    return false
  end
  return true
end

local dirurls = {
  '/%EC%9B%B9%ED%88%B0'
}

function getnameandlink()
  local lurl = dirurls[MODULE.CurrentDirectoryIndex+1]
  if HTTP.GET(MODULE.RootURL .. lurl) then
    local x = TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('//ul[contains(@class, "homelist")]/li/div//a[@id="title"]', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function beforedownloadimage()
  HTTP.Reset()
  HTTP.Cookies.Text = TASK.PageContainerLinks.Values[0]
  HTTP.Headers.Values['Referer'] = MODULE.RootURL
  return true
end

function Init()
  local m = NewWebsiteModule()
  m.ID = '2a7d69a1e1d24f90851b4e2598cffdcd'
  m.Category = 'Raw'
  m.Name = 'Toonkor'
  m.RootURL = 'https://tkor.club'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetNameAndLink='getnameandlink'
  m.OnBeforeDownloadImage='beforedownloadimage'
  m.TotalDirectory = #dirurls
end
