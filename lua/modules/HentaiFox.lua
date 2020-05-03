function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then
      MANGAINFO.Title = x.XPathString('//div[@class="info"]/h1')
    end
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="cover"]//img/@src'))
    if MODULE.Name == 'HentaiFox' then
      MANGAINFO.Artists=x.XPathStringAll('//*[@class="artists"]/li/a')
      MANGAINFO.Genres=x.XPathStringAll('//*[@class="tags"]//li/a')
    else
      MANGAINFO.Artists=x.XPathStringAll('//div[@class="tags" and contains(h3, "Artist")]/div/a/span/text()')
      MANGAINFO.Genres=x.XPathStringAll('//div[@class="tags" and (contains(h3, "Tags") or contains(h3, "Category"))]/div/a/span/text()')
    end
    MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
    MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
    local x=TXQuery.Create(HTTP.Document)
    if MODULE.Name == 'HentaiFox' then
      local galleryId = x.XPathString('//a[@class="g_button"]/@href'):match('/.-/(%d+)/')
      TASK.PageNumber = tonumber(x.XPathString('//span[@class="i_text pages" and contains(., "Pages")]/substring-after(.,": ")'))
      for i = 1, TASK.PageNumber do
        TASK.PageContainerLinks.Add(MaybeFillHost(MODULE.RootURL, '/g/' .. galleryId .. '/' .. i))
      end
    else
      local v = x.XPath('//*[@class="gallery"]//img/@data-src')
      for i = 1, v.Count do
        local s = v.Get(i).ToString();
        s = s:gsub('^//', 'https://'):gsub('(/%d+)[tT]%.', '%1.')
        TASK.PageLinks.Add(s)
      end
    end
  else
    return false
  end
  return true
end

function getdirectorypagenumber()
  if HTTP.GET(MODULE.RootURL) then
    local x = TXQuery.Create(HTTP.Document)
    if MODULE.Name == 'HentaiFox' then
      PAGENUMBER = tonumber(x.XPathString('(//*[@class="pagination"]/li/a)[last()-1]')) or 1
    else
      PAGENUMBER = tonumber(x.XPathString('//*[@class="pagination"]/a[last()-1]')) or 1
    end
    return no_error
  else
    return net_problem
  end
end

function getimageurl()
  local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])
  if HTTP.GET(u) then
    TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//div[@class="full_image"]//img/@src')
    return true
  end
  return false
end

function getnameandlink()
  if HTTP.GET(MODULE.RootURL .. '/pag/' .. IncStr(URL) .. '/') then
    local x = TXQuery.Create(HTTP.Document)
    if MODULE.Name == 'HentaiFox' then
      x.XPathHREFAll('//*[@class="lc_galleries"]//*[@class="caption"]//a', LINKS, NAMES)
    else
      x.XPathHREFAll('//*[@class="preview_item"]/*[@class="caption"]/a', LINKS, NAMES)
    end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(id, name, URL)
  local m = NewWebsiteModule()
  m.ID                       = id
  m.Name                     = name
  m.RootURL                  = URL
  m.Category                 = 'H-Sites'
  m.SortedList               = true
  m.OnGetInfo                = 'getinfo'
  m.OnGetPageNumber          = 'getpagenumber'
  m.OnGetNameAndLink         = 'getnameandlink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnGetImageURL            = 'getimageurl'
end

function Init()
  AddWebsiteModule('58a2dec76ebf43a5a9e7dc9b453e52e9', 'HentaiFox', 'https://hentaifox.com')
  AddWebsiteModule('845f86020f8b4744824f5eaffc1f278f', 'AsmHentai', 'https://asmhentai.com')
end
