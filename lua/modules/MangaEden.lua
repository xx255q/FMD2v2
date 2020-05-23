local diren = '/en/en-directory/?order=-0'
local dirit = '/en/it-directory/?order=-0'

function GetInfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then
      MANGAINFO.Title = x.XPathString('//*[@class="manga-title"]')
    end
    local coverlink = x.XPathString('//div[@class="mangaImage2"]//img/@src')
    if coverlink ~= '' then
      if coverlink:find('^//') then coverlink = 'https:' .. coverlink; end
      MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, coverlink)
    end
    MANGAINFO.Authors=x.XPathStringAll('//*[@class="rightBox"]/a[contains(@href,"/?author=")]')
    MANGAINFO.Artists=x.XPathStringAll('//*[@class="rightBox"]/a[contains(@href,"/?artist=")]')
    MANGAINFO.Genres=x.XPathStringAll('//*[@class="rightBox"]/a[contains(@href,"/?categories")]')
    MANGAINFO.Summary=x.XPathString('//*[@id="mangaDescription"]')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//*[@class="rightBox"]'), 'Ongoing', 'Completed')
    x.XPathHREFAll('//table//tr/td/a[@class="chapterLink"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    for i = 0, MANGAINFO.ChapterNames.Count-1 do
      MANGAINFO.ChapterNames[i] = MANGAINFO.ChapterNames[i]:gsub("Chapter", " ")
    end
    InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
    x=TXQuery.Create(HTTP.Document)
    local s = x.XPathString('//script[contains(., "var pages")]')
    s = GetBetween('=', ';', s)
    x.ParseHTML(s)
    x.XPathStringAll('json(*)().fs', TASK.PageLinks)
    for i = 0, TASK.PageLinks.Count-1 do
      if string.find(TASK.PageLinks[i], '^//') then
        TASK.PageLinks[i] = 'https:' .. TASK.PageLinks[i]
      end;
    end
    return true
  else
    return false
  end
end

function GetDirUrl(website)
  if MODULE.Name == 'MangaEden_IT' or MODULE.Name == 'PervEden_IT' then
    return dirit
  else
    return diren
  end
end

function GetDirectoryPageNumber()
  if HTTP.GET(AppendURLDelim(MODULE.RootURL)..GetDirUrl(MODULE.Name)) then
    x = TXQuery.Create(HTTP.Document)
    PAGENUMBER = tonumber(x.XPathString('//*[@class="pagination pagination_bottom"]/a[last()-1]')) or 1
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if HTTP.GET(MODULE.RootURL..GetDirUrl(MODULE.Name).."&page="..IncStr(URL)) then
    x=TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('//table[@id="mangaList"]//tr/td[1]/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function Init()
  function AddWebsiteModule(id, website, rooturl, category)
    local m = NewWebsiteModule()
    m.ID=id
    m.Category=category
    m.Name=website
    m.RootURL=rooturl
    m.LastUpdated='May 17, 2019'
    m.OnGetInfo='GetInfo'
    m.OnGetPageNumber='GetPageNumber'
    m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
    m.OnGetNameAndLink='GetNameAndLink'
  end
  AddWebsiteModule('d040b868cc844649aee94abf758267a1', 'MangaEden', 'http://www.mangaeden.com', 'English')
  AddWebsiteModule('8aafeb82bfda4aeda5c1b6cd8e96016a', 'MangaEden_IT', 'http://www.mangaeden.com', 'Italian')
  AddWebsiteModule('c7b212ad0856455396d53a67fc4eb180', 'PervEden', 'http://www.perveden.com', 'H-Sites')
  AddWebsiteModule('11351449c7a2468db9e7ac6528573698', 'PervEden_IT', 'http://www.perveden.com', 'H-Sites')
end
