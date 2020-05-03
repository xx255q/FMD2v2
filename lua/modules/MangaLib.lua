function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title=x.XPathString('//meta[@itemprop="alternativeHeadline"]/@content')
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="manga__cover"]/@src'))
    MANGAINFO.Authors=x.XPathString('//div[@class="info-list__row"][starts-with(.,"Автор")]')
    MANGAINFO.Authors=string.gsub(MANGAINFO.Authors, 'Автор', '')
    MANGAINFO.Authors=string.gsub(MANGAINFO.Authors, '  ', '')
    MANGAINFO.Artists=x.XPathString('//div[@class="info-list__row"][starts-with(.,"Художник")]')
    MANGAINFO.Artists=string.gsub(MANGAINFO.Artists, 'Художник', '')
    MANGAINFO.Artists=string.gsub(MANGAINFO.Artists, '  ', '')
    MANGAINFO.Genres=x.XPathString('//div[@class="info-list__row"][starts-with(.,"Жанры")]')
    MANGAINFO.Genres=string.gsub(MANGAINFO.Genres, 'Жанры', '')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="info-list__row"][starts-with(.,"Перевод")]'), 'продолжается', 'завершен')
    MANGAINFO.Summary=x.XPathStringAll('//div[contains(@class, "info-desc__content")]/text()', '')
    x.XPathHREFAll('//div[@class="chapters-list"]/div/div[@class="chapter-item__name"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageNumber=0
  TASK.PageLinks.Clear()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
    local x=TXQuery.Create(HTTP.Document)
    local imgbaseurl=buildimageurl(TXQuery.Create(string.match(x.XPathString('//script[contains(., "window.__info")]/text()'), 'window.__info =(.*);')))
    local p=TXQuery.Create(DecodeBase64(StreamToString(HTTP.Document):match('<span class="pp"><!%-%-(.-)%-%-></span>'):gsub('^%s*(.-)%s*$', '%1')))
    local v=p.XPath('json(*)()')
    for i=1,v.Count do
      local v1=v.Get(i)
      TASK.PageLinks.Add(imgbaseurl..x.XPathString('u', v1))
    end
    return true
  else
    return false
  end
end

function buildimageurl(json)
  local result = string.gsub(MODULE.RootURL, 'https://', '')
  if json.XPathString('json(*).imgServer') == 'primary' then
    result = 'img1.'..result
  else
    result = 'img2.'..result
  end
  result = 'https://' .. result .. json.XPathString('json(*).imgUrl')
  
  return result
end

function getnameandlink()
  if tonumber(URL) <= 0 then URL = 200 end
  if HTTP.GET(MODULE.RootURL .. '/filterlist?page='..IncStr(URL)..'&cat=&alpha=&sortBy=name&asc=true&author=&artist=') then
    local x = TXQuery.Create(HTTP.Document)
    local page = x.XPathString('//div[@class="paginator paginator_full paginator_border-top"]//ul[@class="pagination"]/li[last()]/a/substring-after(@href, "?page=")')
    if x.XPathString('//div/p[contains(., "Ничего не найдено")]') == '' then
      local v = x.XPath('//*[@class="manga-list-item"]/a[@class="manga-list-item__content"]')
      for i=1,v.Count do
        local v1=v.Get(i)
        LINKS.Add(v1.GetAttribute('href'))
        NAMES.Add(v1.GetAttribute('title'))
      end
      UPDATELIST.CurrentDirectoryPageNumber = page
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID = 'df365d3d22f141ad8b9224cc1413ca02'
  m.Name = 'MangaLib'
  m.RootURL = 'https://mangalib.me'
  m.Category = 'Russian'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetNameAndLink='getnameandlink'
end
