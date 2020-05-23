function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title=x.XPathString('//h1/text()')
    MANGAINFO.CoverLink=x.XPathString('//main//img/@src')
    MANGAINFO.Artists=x.XPathStringAll('//main//table//tr[td="Artist:"]/td/a')
    MANGAINFO.Authors=x.XPathStringAll('//main//table//tr[td="Author:"]/td/a')
    MANGAINFO.Genres=x.XPathStringAll('//main//table//tr[td="Tags:"]/td/a')
    MANGAINFO.Summary=x.XPathString('//main//pre')
    x.XPathHREFAll('//main//ul/li/span/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL..'/manifest.json')) then
    local x=TXQuery.Create(HTTP.Document)
    x.XPathStringAll('json(*).readingOrder().href',TASK.PageLinks)
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if HTTP.GET(MODULE.RootURL..'/directory/') then
    PAGENUMBER=tonumber(TXQuery.Create(HTTP.Document).XPathString('//div[@class="pagination"]//a[contains(., "last")]/@href'):match('/(%d+)/'))
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  if HTTP.GET(MODULE.RootURL..'/directory/'..IncStr(URL)) then
    TXQuery.Create(HTTP.Document).XPathHREFAll('//div[@class="mdc-card__media-title"]/a',LINKS,NAMES)
    return no_error
  else
    return net_problem
  end
end

function Init()
  function AddWebsiteModule(id, name, URL, category)
    local m = NewWebsiteModule()
    m.ID = id
    m.Name = name
    m.RootURL = URL
    m.Category = category
    m.OnGetInfo = 'getinfo'
    m.OnGetPageNumber = 'getpagenumber'
    m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
    m.OnGetNameAndLink = 'getnameandlink'
  end
  local cat = 'English-Scanlation'
  AddWebsiteModule('380e42a9e93e488abcd74cf47b2cf148', 'LetItGoScans', 'https://reader.letitgo.scans.today', cat)
  AddWebsiteModule('14ac824309034a6495fc4f91873aeb30', 'ProjectTime', 'https://read.ptscans.com', cat)
end
