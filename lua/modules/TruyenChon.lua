local dirurl = {
  ['TruyenChon'] = '/the-loai?status=-1&sort=15&page=%s',
  ['NetTruyen'] = '/tim-truyen?status=-1&sort=15&page=%s',
  ['MangaNT'] = '/genres?status=-1&sort=15&page=%s'
}

function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then
      MANGAINFO.Title = x.XPathString('//h1[@class="title-detail"]')
    end
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class, "col-image")]/img/@src'))
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//li[contains(@class, "status")]/p[2]'), 'Đang tiến hành', 'Hoàn thành')
    if MANGAINFO.Status == '' then
      MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//p[contains(., "status")]/following-sibling::p'))
    end
    MANGAINFO.Authors=x.XPathString('//li[contains(@class, "author")]/p[2]')
    if MANGAINFO.Authors == '' then
      MANGAINFO.Authors=x.XPathStringAll('//p[contains(., "Author(s)")]/following-sibling::p/a')
    end
    MANGAINFO.Artists=x.XPathString('//h4[starts-with(./label,"Artista")]/substring-after(.,":")')
    MANGAINFO.Genres=x.XPathStringAll('//li[contains(@class, "kind")]/p[2]/a')
    MANGAINFO.Summary=x.XPathString('//div[@class="detail-content"]/p')
    x.XPathHREFAll('//div[@class="list-chapter"]//ul/li/div[contains(@class, "chapter")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
    local x=TXQuery.Create(HTTP.Document)
    x.XPathStringAll('//div[@class="page-chapter"]/img/@data-original', TASK.PageLinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if HTTP.GET(MODULE.RootURL .. dirurl[MODULE.Name]:format(IncStr(URL))) then
    local x = TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('//div[@class="item"]//h3/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if HTTP.GET(MODULE.RootURL .. dirurl[MODULE.Name]:format('1')) then
    local x = TXQuery.Create(HTTP.Document)
    local s = x.XPathString('//ul[@class="pagination"]/li[last()]/a/@href')
    PAGENUMBER = tonumber(s:match('&page=(%d+)')) or 1
    return no_error
  else
    return net_problem
  end
end

function Init()
  function AddWebsiteModule(id, name, url, category)
    local m = NewWebsiteModule()
    m.ID                       = id
    m.Name                     = name
    m.RootURL                  = url
    m.Category                 = category
    m.SortedList               = true
    m.OnGetInfo                = 'getinfo'
    m.OnGetPageNumber          = 'getpagenumber'
    m.OnGetNameAndLink         = 'getnameandlink'
    m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  end
  local cat = 'Vietnamese'
  AddWebsiteModule('ef7f922bd45f4f9d9c559a55f987004d', 'TruyenChon', 'http://truyenchon.com', cat)
  AddWebsiteModule('567780dbaa3149e7ad698f11ce68ea9b', 'NetTruyen', 'http://www.nettruyen.com', cat)

  cat = 'English'
  AddWebsiteModule('d2f24dec90e841b1aab4bea145ffb638', 'MangaNT', 'https://mangant.com', cat)
end
