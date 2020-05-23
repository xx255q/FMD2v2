function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title = Trim(SeparateLeft(x.XPathString('//div[@class="container"]//li[3]//span'), '- Raw'))
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="thumbnail"]/@src'))
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info"]/li[contains(., "Status")]//a'))
    MANGAINFO.Authors=x.XPathString('//ul[@class="manga-info"]/li[contains(., "Author")]//a')
    MANGAINFO.Genres=x.XPathStringAll('//ul[@class="manga-info"]/li[contains(., "Genre")]//a')
    MANGAINFO.Summary=x.XPathString('//h3[text()="Description"]/following-sibling::p')
    if MANGAINFO.Summary == '' then
      MANGAINFO.Summary=x.XPathString('//div[@class="detail"]/div[@class="content"]')
    end
    x.XPathHREFAll('//div[@id="tab-chapper"]//table/tbody/tr/td/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    if MANGAINFO.ChapterLinks.Count == 0 then
      x.XPathHREFAll('//div[@id="list-chapters"]//a[@class="chapter"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    end
    for i = 0, MANGAINFO.ChapterLinks.Count-1 do
      MANGAINFO.ChapterLinks[i] = MODULE.RootURL .. '/' .. MANGAINFO.ChapterLinks[i]
    end
    InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  local u = MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(u) then
    local x=TXQuery.Create(HTTP.Document)
    if MODULE.Name == 'Manhwa18' then
      local v = x.XPath('//img[contains(@class, "chapter-img")]/@src')
      for i = 1, v.Count do
        local s = v.Get(i).ToString()
        s = s:gsub('app/', 'https://manhwa18.com/app/'):gsub('https://manhwa18.net/https://manhwa18.com', 'https://manhwa18.net')
        if string.find(s, ".iff") == nil then
          TASK.PageLinks.Add(s)
        end
      end
    elseif MODULE.Name == 'Lhscans' then
      x.XPathStringAll('//img[contains(@class, "chapter-img")]/@data-src', TASK.PageLinks)
    else
      x.XPathStringAll('//img[contains(@class, "chapter-img")]/@src', TASK.PageLinks)
    end
    if MODULE.Name == 'Lhscans' or MODULE.Name == 'MangaHato' then TASK.PageContainerLinks.Text = u end
  else
    return false
  end
  return true
end

function getnameandlink()
  if HTTP.GET(MODULE.RootURL .. '/manga-list.html?listType=allABC') then
    local x = TXQuery.Create(HTTP.Document)
    local v = x.XPath('//span[@manga-slug]//a')
    for i = 1, v.Count do
      local v1 = v.Get(i)
      NAMES.Add(Trim(SeparateLeft(v1.ToString(), '- Raw')))
      LINKS.Add(v1.GetAttribute('href'))
    end
    return no_error
  else
    return net_problem
  end
end

function BeforeDownloadImage()
  if MODULE.Name == 'Lhscans' or MODULE.Name == 'MangaHato' then HTTP.Headers.Values['Referer'] = TASK.PageContainerLinks.Text end
  return true
end

function Init()
  function AddWebsiteModule(id, name, url, cat)
    local m = NewWebsiteModule()
    m.ID                    = id
    m.Category              = cat
    m.Name                  = name
    m.RootURL               = url
    m.TotalDirectory        = 1
    m.OnGetInfo             = 'getinfo'
    m.OnGetPageNumber       = 'getpagenumber'
    m.OnGetNameAndLink      = 'getnameandlink'
    m.OnBeforeDownloadImage = 'BeforeDownloadImage'
  end
  local cat = 'Raw'
  AddWebsiteModule('9e96846a035646988e1b2eb0f356d795', 'Lhscans', 'https://loveheaven.net', cat)
  AddWebsiteModule('4c089029492f43c98d9f27a23403247b', 'HanaScan', 'https://hanascan.com', cat)
  AddWebsiteModule('010777f53bf2414fad039b9567c8a9ce', 'MangaHato', 'https://mangahato.com', cat)
  AddWebsiteModule('794187d0e92e4933bf63812438d69017', 'Manhwa18', 'https://manhwa18.com', cat)

  cat = 'English'
  AddWebsiteModule('80427d9a7b354f04a8f432b345f0f640', 'MangaWeek', 'https://mangaweek.com', cat)
  AddWebsiteModule('570e716a029e45cabccc2b660ed81425', 'ManhwaScan', 'https://manhwascan.com', cat)
  AddWebsiteModule('694ff34a6ae4469fbdaecf8d3aebb6eb', 'ManhuaScan', 'https://manhuascan.com', cat)
  AddWebsiteModule('3b7ab0c7342f4783910f7842ea05630b', 'EcchiScan', 'https://ecchiscan.com', cat)
end
