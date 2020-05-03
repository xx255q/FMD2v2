function GetRedirectUrl(document)
  local x = TXQuery.Create(document)
  local s = x.XPathString('//script[contains(., "window.location.assign")]')
  if (s ~= '') and (s ~= nil) then
    return GetBetween('("', '")', s)
  end
  return ''
end

function getinfo()
  local u = MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(u) then
    local s = GetRedirectUrl(HTTP.Document)
    if (s ~= '') and (s ~= nil) then
      u = s
      if not HTTP.GET(u) then return false; end
    end
    MANGAINFO.URL=u
    local x=TXQuery.Create(HTTP.Document)
    if MODULE.Name == 'MangaKakalot' or MODULE.Name == 'MangaKakalots' then
      MANGAINFO.Title=x.XPathString('//ul[@class="manga-info-text"]/li/h1')
      MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="manga-info-pic"]/img/@src'))
      MANGAINFO.Authors=x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Author")]/a')
      MANGAINFO.Genres=x.XPathStringAll('//ul[@class="manga-info-text"]/li[contains(., "Genre")]/a')
      MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info-text"]/li[contains(., "Status")]'))
      MANGAINFO.Summary=x.XPathStringAll('//div[@id="noidungm"]/text()', '')
      x.XPathHREFAll('//div[@class="chapter-list"]/div[@class="row"]/span/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
      InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    else
      MANGAINFO.Title=x.XPathString('//div[@class="story-info-right"]/h1')
      MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//span[@class="info-image"]/img/@src'))
      MANGAINFO.Authors=x.XPathStringAll('//td[contains(., "Author(s)")]/following-sibling::td/a')
      MANGAINFO.Genres=x.XPathStringAll('//td[contains(., "Genres")]/following-sibling::td/a')
      MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//td[contains(., "Status")]/following-sibling::td'))
      MANGAINFO.Summary=x.XPathStringAll('//div[@class="panel-story-info-description"]/text()', '')
      x.XPathHREFAll('//ul[@class="row-content-chapter"]/li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
      InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    end
    if (Pos('email', MANGAINFO.Title) > 0) and (Pos('protected', MANGAINFO.Title) > 0) then
      MANGAINFO.Title = Trim(x.XPathString('//title/substring-after(substring-before(., "Manga Online"), "Read")'))
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  function spliturl(u)
    local pos = 0
    for i = 1, 3 do
      local p = string.find(u, '/', pos+1, true)
      if p == nil then break; end
      pos = p
    end
    return string.sub(u, 1, pos-1), string.sub(u, pos)
  end
  TASK.PageLinks.Clear()
  TASK.PageNumber=0
  local u = MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(u) then
    local s = GetRedirectUrl(HTTP.Document)
    if (s ~= '') and (s ~= nil) then
      local host, _ = spliturl(s)
      local _, PATH = spliturl(u)
      u = host .. PATH
      if not HTTP.GET(u) then return false; end
    end
    local x=TXQuery.Create(HTTP.Document)
    x.XPathStringAll('//div[@id="vungdoc"]/img/@src', TASK.PageLinks)
    if TASK.PageLinks.Count == 0 then
      x.XPathStringAll('//div[@class="vung_doc"]/img/@src', TASK.PageLinks)
    end
    if TASK.PageLinks.Count == 0 then
      x.XPathStringAll('//div[@class="container-chapter-reader"]/img/@src', TASK.PageLinks)
    end
    if TASK.PageLinks.Count == 0 then
      x.XPathStringAll('//div[@id="vungdoc"]/img/@data-src', TASK.PageLinks)
    end
    return true
  else
    return false
  end
end

local dirurl = '/manga_list?type=newest&category=all&state=all&page='
local dirs = '/genre-all/'

function getnameandlink()
  if MODULE.Name == 'MangaKakalot' or MODULE.Name == 'MangaKakalots' then
    if HTTP.GET(MODULE.RootURL .. dirurl .. IncStr(URL)) then
      local x = TXQuery.Create(HTTP.Document)
      x.XPathHREFAll('//div[@class="truyen-list"]/div[@class="list-truyen-item-wrap"]/h3/a', LINKS, NAMES)
      return no_error
    else
      return net_problem
    end
  else
    if HTTP.GET(MODULE.RootURL .. dirs .. IncStr(URL)) then
      local x = TXQuery.Create(HTTP.Document)
      x.XPathHREFAll('//div[@class="panel-content-genres"]//div[@class="genres-item-info"]/h3/a', LINKS, NAMES)
      return no_error
    else
      return net_problem
    end
  end
end

function getdirectorypagenumber()
  if MODULE.Name == 'MangaKakalot' or MODULE.Name == 'MangaKakalots' then
    if HTTP.GET(MODULE.RootURL .. dirurl .. '1') then
      PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).XPathString('//a[contains(@class, "page_last")]/@href'):match('page=(%d+)'))
      return no_error
    else
      return net_problem
    end
  else
    if HTTP.GET(MODULE.RootURL .. dirs .. '1') then
      PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).XPathString('//a[contains(@class, "page-last")]/@href'):match('.-//.-/.-/(%d+)'))
      return no_error
    else
      return net_problem
    end
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
  AddWebsiteModule('74674292e13c496699b8c5e4efd4b583', 'MangaKakalot', 'https://mangakakalot.com')
  AddWebsiteModule('fa8bb4d1ceea4c8fa0e98c00755f95d4', 'MangaNelo', 'https://manganelo.com')
  AddWebsiteModule('ed4175a390e74aedbe4b4f622f3767c6', 'MangaKakalots', 'https://mangakakalots.com')
end
