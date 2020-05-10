function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then
      MANGAINFO.Title = Trim(x.XPathString('//h1[contains(@class, "element-title")]/text()'))
    end
    MANGAINFO.CoverLink = x.XPathString('//img[contains(@class,"book-thumbnail")]/@src')
    MANGAINFO.Genres=x.XPathStringAll('//a[contains(@class, "badge")]')
    MANGAINFO.Authors=Trim(x.XPathString('//span[@class="list-group-item" and contains(., "Autor")]/a'))
    MANGAINFO.Artists=Trim(x.XPathString('//span[@class="list-group-item" and contains(., "Artist")]/a'))
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "book-status")]'), 'public', 'final')
    MANGAINFO.Summary = x.XPathStringAll('//*[@class="element-description"]/text()', '')
    local v = x.XPath('//div[contains(@class, "chapters")]/ul//li')
    for i = 1, v.Count do
      local v1 = v.Get(i)
      local name = x.XPathString('h4', v1)
      local w = x.XPath('div//ul/li/div', v1)
      for j = 1, w.Count do
        local w1 = w.Get(j)
        local scan = '[' .. x.XPathString('div[1]', w1) .. ']'
        MANGAINFO.ChapterLinks.Add(x.XPathString('div[contains(@class, "text-right")]/a/@href', w1))
        MANGAINFO.ChapterNames.Add(name .. ' ' .. scan)
      end
    end
    if MANGAINFO.ChapterLinks.Count == 0 then
      local w = x.XPath('//ul[contains(@class, "chapter-list")]/li/div')
      for j = 1, w.Count do
        local w1 = w.Get(j)
        local scan = '[' .. x.XPathString('div[1]', w1) .. ']'
        MANGAINFO.ChapterLinks.Add(x.XPathString('div[contains(@class, "text-right")]/a/@href', w1))
        MANGAINFO.ChapterNames.Add(MANGAINFO.Title .. ' ' .. scan)
      end
    end
    InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  HTTP.Headers.Values['Referer'] = MODULE.RootURL:gsub('http://', 'https://')
  if not HTTP.GET(MaybeFillHost(MODULE.RootURL, URL):gsub('http://', 'https://')) then return false; end
  local x = TXQuery.Create(HTTP.Document)
  local u = x.XPathString('//meta[@property="og:URL"]/@content')
  if string.match(u, 'cascade') then
    u = string.gsub(u, '/cascade', '/paginated')
    print(u)
    if not HTTP.GET(MaybeFillHost(MODULE.RootURL, u):gsub('http://', 'https://')) then return false; end
    x = TXQuery.Create(HTTP.Document)
  end
  TASK.PageNumber = tonumber(x.XPathString('(//select[@id="viewer-pages-select"])[1]/option[last()]/text()'))
  for i = 1, TASK.PageNumber do
    TASK.PageContainerLinks.Add(u..'/'..i)
  end
  return true
end

function getimageurl()
  local s = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID]):gsub('http://', 'https://')
  HTTP.Headers.Values['Referer'] = MODULE.RootURL:gsub('http://', 'https://')
  if HTTP.GET(s) then
    TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//img[@class="viewer-image"]/@src')

    return true
  end
  return false
end

function getnameandlink()
  local s = '/library?order_item=alphabetically&order_dir=asc&filter_by=title&page='..IncStr(URL)
  if HTTP.GET(MODULE.RootURL .. s) then
    local x = TXQuery.Create(HTTP.Document)
    local v = x.XPath('//*[@data-identifier]/a')
    local hasTitles = false
    for i = 1, v.Count do
      local v1 = v.Get(i)
      LINKS.Add(v1.GetAttribute('href'))
      NAMES.Add(x.XPathString('div/div[@class="thumbnail-title"]', v1))
      hasTitles = true
    end
    if hasTitles then
      UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1
    end
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID = '9185eb6c49324a849c7d7925a41ef3a3'
  m.Name = 'Tumangaonline'
  m.RootURL = 'http://tmofans.com' -- Don't set to https because TMO set http for Cloudflare!
  m.Category = 'Spanish'
  m.MaxTaskLimit = 1
  m.MaxConnectionLimit = 1
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetNameAndLink='getnameandlink'
  m.OnGetImageURL='getimageurl'
end
