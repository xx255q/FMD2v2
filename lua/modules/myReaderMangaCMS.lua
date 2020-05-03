Modules = {}

function Modules.myReaderMangaCMS()
  local myReaderMangaCMS = {}
  
  function myReaderMangaCMS:new()
    local obj = {}
    setmetatable(obj, self)
    self.__index = self
    return obj
  end
  
  function myReaderMangaCMS:getinfo()
    MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
    if HTTP.GET(MANGAINFO.URL) then
      local x = TXQuery.Create(HTTP.Document)
      MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="boxed"]/img/@src'))
      if MANGAINFO.Title == '' then 
        MANGAINFO.Title = x.XPathString('//*[(self::h2 or self::h1) and contains(@class,"widget-title")]')
        if MANGAINFO.Title == '' then
          MANGAINFO.Title = MANGAINFO.URL:match('/([^/]+)$')
        end
      end
      MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//dt[.=("Status","Estado","Statut")]/following-sibling::dd[1]'))
      MANGAINFO.Authors = x.XPathStringAll('//dt[.=("Author(s)","Yazar & Çizer:","Autor(es)","Auteur(s)")]/following-sibling::dd[1]/string-join(*,", ")')
      MANGAINFO.Artists = x.XPathStringAll('//dt[.=("Artist(s)","Artiste(s)")]/following-sibling::dd[1]/string-join(*,", ")')
      MANGAINFO.Genres = x.XPathStringAll('//dt[.=("Categories","Kategoriler:","Categorías","Catégories")]/following-sibling::dd[1]/string-join(*,", ")')
      MANGAINFO.Summary = x.XPathString('//div[@class="well"]/p')
      local v = x.XPath('//ul[@class="chapters"]/li/*[self::h5 or self::h3]')
      for i = 1, v.Count do
        local v2 = v.Get(i)
        MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href', v2))
        MANGAINFO.ChapterNames.Add(x.XPathString('normalize-space(.)', v2))
      end
      InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
      return no_error
    end
    return net_problem
  end
  
  function myReaderMangaCMS:getpagenumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    if HTTP.GET(u) then
      local x = TXQuery.Create(HTTP.Document)
      x.XPathStringAll('//div[@id="all"]/img/@data-src', TASK.PageLinks)
      if TASK.PageLinks.Count == 0 then
        x.XPathStringAll('//div[@id="all"]/img/@src', TASK.PageLinks)
      end
      TASK.PageContainerLinks.Text = u
      return true
    end
    return false
  end
  
  function myReaderMangaCMS:getnameandlink()
    if HTTP.GET(MODULE.RootURL .. self.getdirurl()) then
      local x = TXQuery.Create(HTTP.Document)
      x.XPathHREFAll('//li/a', LINKS, NAMES)
      return no_error
    end
    return net_problem
  end
  
  function myReaderMangaCMS:getdirurl()
    return '/changeMangaList?type=text'
  end
  
  function myReaderMangaCMS:beforedownloadimage()
    HTTP.Reset()
    HTTP.Headers.Values['Referer'] = TASK.PageContainerLinks.Text
    return true
  end
  
  return myReaderMangaCMS
end

-------------------------------------------------------------------------------

function createInstance()
  local m = Modules[MODULE.Name]
  if m ~= nil then
    return m():new()
  else
    return Modules.myReaderMangaCMS():new()
  end
end

-------------------------------------------------------------------------------

function getinfo()
  return createInstance():getinfo()
end

function getpagenumber()
  return createInstance():getpagenumber()
end

function getnameandlink()
  return createInstance():getnameandlink()
end

function beforedownloadimage()
  return createInstance():beforedownloadimage()
end

function AddWebsiteModule(id, name, url, cat)
  local m = NewWebsiteModule()
  m.ID = id
  m.Category = cat
  m.Name = name
  m.RootURL = url
  m.OnGetNameAndLink = 'getnameandlink'
  m.OnGetInfo = 'getinfo'
  m.OnGetPageNumber = 'getpagenumber'
  m.OnBeforeDownloadImage='beforedownloadimage'
end

function Init()
  local c='Raw'
  AddWebsiteModule('70fbb8b02bd34fdab64975f93a8162d2', 'MangaRawOnline', 'http://mangaraw.online', c)
  AddWebsiteModule('c5c49466990f43e3b099719a13a2f887', 'RawMangaSite', 'https://rawmanga.site', c)

  c='Spanish-Scanlation'
  AddWebsiteModule('33a495fe529a4f01923545740d94f5d4', 'SOSScanlation', 'https://sosscanlation.com', c)
  AddWebsiteModule('5ad6ebfe79f04b3b954829180052cb9e', 'SamuraiScan', 'https://samuraiscan.com', c)
end
