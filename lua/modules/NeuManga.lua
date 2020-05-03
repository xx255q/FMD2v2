function GetInfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then
      MANGAINFO.Title=x.XPathString('//h1[@itemprop="headline"]')
      MANGAINFO.Title=MANGAINFO.Title:gsub('^Baca Manga','')
      MANGAINFO.Title=MANGAINFO.Title:gsub('Bahasa Indonesia$','')
      MANGAINFO.Title=MANGAINFO.Title:gsub('Manga','')
	    MANGAINFO.Title=MANGAINFO.Title:gsub('^Manga','')
	    MANGAINFO.Title=MANGAINFO.Title:gsub('Manga$','')
    end
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="imagemg"]/@src'))
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//span[@class="hentry"]/span[contains(., "Status")]/a'))
    MANGAINFO.Authors = x.XPathStringAll('//span[@class="hentry"]/span[contains(., "Author")]/a')
    MANGAINFO.Artists = x.XPathStringAll('//span[@class="hentry"]/span[contains(., "Artist")]/a')
    MANGAINFO.Genres = x.XPathStringAll('//span[@class="hentry"]/span[contains(., "Genre")]/a')
    MANGAINFO.Summary = x.XPathStringAll('//div[contains(@class,"summary")]/text()', '')
    local v=x.XPath('//div[@id="scans"]//div[@class="item-content"]/a')
    for i=1, v.Count do
        local v1=v.Get(i)
        MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(x.XPathString('h3', v1))
    end
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)    
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  TASK.PageLinks.Clear()
  TASK.PageNumber = 0
  HTTP.Cookies.Values['age_confirmed'] = '1'
  local u = AppendURLDelim(MaybeFillHost(MODULE.RootURL,URL)) .. '_/1'
  if HTTP.GET(u) then
    local x=TXQuery.Create(HTTP.Document)
    x.XPathStringAll('//img[@class="imagechap"]/@data-src', TASK.PageLinks)
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if HTTP.GET(MODULE.RootURL..'/manga') then
    x=TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('//div[@class="alplist"]/li/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID = '43b1be881e5c4661b984b84ba8f208a6'
  m.Category='Indonesian'
  m.Name='NeuManga'
  m.RootURL='https://neumanga.tv'
  m.OnGetInfo='GetInfo'
  m.OnGetPageNumber='GetPageNumber'
  m.OnGetNameAndLink='GetNameAndLink'
end 
