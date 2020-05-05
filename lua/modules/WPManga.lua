function getinfo()
  local s = ''
  MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then  
    x = TXQuery.Create(HTTP.Document)
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[starts-with(@class,"cvr")]/@src'))
    MANGAINFO.Title = x.XPathString('//*[@itemprop="itemreviewed"]')
    MANGAINFO.Authors = x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Author")]/substring-after(normalize-space(.)," ")')
    MANGAINFO.Artists = x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Artist")]/substring-after(normalize-space(.)," ")')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Status")]/substring-after(normalize-space(.)," ")'))
    MANGAINFO.Summary = x.XPathString('//div[@class="det"]/p[1]')
    if (MODULE.Name == 'ReadHentaiManga') then
      MANGAINFO.Genres = x.XPathString('string-join(//*[contains(@class,"mng_det")]//*[self::p or self::li]//a,", ")')
    else
      MANGAINFO.Genres = x.XPathString('//*[contains(@class,"mng_det")]//*[self::p or self::li][starts-with(.,"Category")]/string-join((./*[position()>1]),", ")')
    end
    if MODULE.Name == 'MangaOnlineToday' then
      MANGAINFO.Summary = x.XPathString('//div[contains(@class,"mng_det")]/p[1]')
      x.XPathHREFAll('//ul[@class="chp_lst"]/li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    else
      while true do      
        v = x.XPath('//a[@class="lst"]')
        for i = 1, v.Count do
          v2 = v.Get(i)
          MANGAINFO.ChapterLinks.Add(v2.GetAttribute('href'))
          s = v2.GetAttribute('title')
          if s == '' then
            s = x.XPathString('*[@class="val"]', v2)
          end
          if s == '' then
            s = x.XPathString('text()[1]', v2)
          end
          MANGAINFO.ChapterNames.Add(s)
        end
        if HTTP.Terminated then break end
        s = Trim(x.XPathString('//*[@class="pgg"]//*[./a[@class="sel"]]/following-sibling::*[./a]/a/@href'))
        if s == '' then break end
        if HTTP.GET(MaybeFillHost(MODULE.RootURL, s)) then
          x.ParseHTML(HTTP.Document)
        else
          break
        end
      end
      InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  local s = ''
  local allnum = false
  HTTP.Cookies.Values['viewer'] = '1'
  if HTTP.GET(AppendURLDelim(MaybeFillHost(MODULE.RootURL, URL)) .. '1') then      
    -- multi page
    x = TXQuery.Create(HTTP.Document)
    s = x.XPathString('//script[contains(.,"imglist")]/substring-after(substring-before(.,"]"),"[")')
    if s ~= '' then
      s = '[' .. s .. ']'
    else
      s = x.XPathString('//script[contains(.,"img_lst")]/substring-after(substring-before(.,"\')"),"(\'")')
      if s ~= '' then
        s = DecodeURL(s)
      end
    end
    x.ParseHTML(s)
    x.XPathStringAll('json(*)()("URL")', TASK.PageLinks)
    if TASK.PageLinks.Count == 0 then
      x.XPathStringAll('json(*)()', TASK.PageLinks)
    end
    
    -- single page
    if TASK.PageLinks.Count == 0 then
      x.ParseHTML(HTTP.Document)
      TASK.PageNumber = x.XPath('(//select[@class="cbo_wpm_pag"])[1]/option').Count
      if TASK.PageNumber == 0 then
        TASK.PageNumber = x.XPath('(//select[@name="page"])[1]/option').Count
      end
      if TASK.PageNumber == 0 then
        v = x.XPath('//select')
        for i = 1, v.Count do
          allnum = true
          v2 = x.XPath('option', v.Get(i))
          for i = 1, v2.Count do
            if tointeger(v2.ToString()) == -1 then
              allnum = false
              break
            end
          end
          if allnum then
            TASK.PageNumber = x.XPath('option', v).Count
            break
          end
        end
      end      
    end
    return true
  else
    return false
  end   
end

function getimageurl()
  local s = ''
  if HTTP.GET(AppendURLDelim(MaybeFillHost(MODULE.RootURL, URL)) .. IncStr(WORKID) .. '/') then
    x = TXQuery.Create(HTTP.Document)        
    if MODULE.Name == 'ReadHentaiManga' then
      s = HTMLDecode(x.XPathString('//img[@id="main_img"]/@src'))
    else
      s = x.XPathString('//*[contains(@class,"mng_rdr")]//img/@src')
    end
    if s == '' then
      s = x.XPathString('//*[@id="reader"]//img[@id="picture"]/@src')
    end
    TASK.PageLinks[WORKID] = s
    return true
  else
    return false
  end
end

function getdirurl(website)
  local result = ''
  if (website == 'MangaSpy') or (website == 'MangaIce') then
    result = 'manga_list'
  elseif (website == 'ReadHentaiManga') then
    result = 'hentai-manga-list'
  else
    result = 'manga-list'
  end
  return '/' .. result .. '/all/any/last-added/'
end

function getdirectorypagenumber()
  if HTTP.GET(AppendURLDelim(MODULE.RootURL) .. getdirurl(MODULE.Name)) then
    x = TXQuery.Create(HTTP.Document)
    PAGENUMBER = tonumber(x.XPathString('//ul[@class="pgg"]/li[last()]/a/@href'):match('/(%d+)/')) or 1
    return true
  else
    return false
  end
end

function getnameandlink()
  local w = {
    ['MangaSpy'] = true,
    ['MangaIce'] = true,
    ['MangaDeep'] = true,
    ['Manga99'] = true
  }
  if HTTP.GET(AppendURLDelim(MODULE.RootURL) .. getdirurl(MODULE.Name) .. IncStr(URL) .. '/') then
    x = TXQuery.Create(HTTP.Document)
    if w[MODULE.Name] then
      x.XPathHREFAll('//*[contains(@id,"content")]//*[@class="det"]/a', LINKS, NAMES)
    elseif MODULE.Name == 'MangaOnlineToday' then
      x.XPathHREFAll('//*[contains(@id,"content")]//div[@class="box"]/ul/li/a', LINKS, NAMES)
    else
      x.XPathHREFTitleAll('//*[contains(@id,"content")]//a[./img]', LINKS, NAMES);
    end
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(id, name, url, category)
  local m = NewWebsiteModule()
  m.ID                       = id
  m.Name                     = name
  m.RootURL                  = url
  m.Category                 = category
  m.SortedList               = true
  m.OnGetInfo                = 'getinfo'
  m.OnGetPageNumber          = 'getpagenumber'
  m.OnGetImageURL            = 'getimageurl'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnGetNameAndLink         = 'getnameandlink'
end

function Init()
  AddWebsiteModule('90067f671b7645de866ab83465fd22f9', '3asq', 'http://www.3asq.info', 'Arabic-Scanlation')
  AddWebsiteModule('d7fb3441b0ad4fb9b01df3491ee43b31', 'ReadHentaiManga', 'http://readhentaimanga.com', 'H-Sites')
end
