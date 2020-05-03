function GetInfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
  HTTP.Cookies.Values['set'] = 'h=1'
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then
      MANGAINFO.Title=x.XPathString('//h3')
    end
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('css("img.img-fluid")/@src'))
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('css("table.table-borderless")//tr[th="Status"]/td'))
    MANGAINFO.Authors = x.XPathString('css("table.table-borderless")//tr[th="Author(s)"]/td')
    MANGAINFO.Artists = x.XPathString('css("table.table-borderless")//tr[th="Artist(s)"]/td')
    MANGAINFO.Genres = x.XPathStringAll('css("table.table-borderless")//tr[th="Genre(s)"]/td')
    MANGAINFO.Genres = MANGAINFO.Genres:gsub('%s+/%s+', ', ')
    MANGAINFO.Summary = x.XPathString('//h4[.="Summary"]/following-sibling::p')
    local v=x.XPath('css("div.card-header")')
    for i=1, v.Count do
      local v1=v.Get(i)
      local src = x.XPathString('./div/a', v1)
      local w = x.XPath('./following-sibling::div/ul/li//a', v1)
      for j = 1, w.Count do
        local w1 = w.Get(j)
        MANGAINFO.ChapterLinks.Add(w1.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(string.format('%s (%s)', w1.ToString(), src))
      end
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
  if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
    local x=TXQuery.Create(HTTP.Document)
    x.XPathStringAll('json(//script[contains(.,"var _load_pages")]/substring-after(substring-before(.,";")," = "))()/u', TASK.PageLinks)
    return true
  else
    return false
  end
end

function GetNameAndLink()
  if HTTP.GET(MODULE.RootURL..'/browse?order_by=create&page='..IncStr(URL)) then
    x=TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('css("#browse h6")/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function getdirectorypagenumber()
  if HTTP.GET(MODULE.RootURL .. '/browse?order_by=create') then
    local x = TXQuery.Create(HTTP.Document)
    PAGENUMBER = tonumber(x.XPathString('css("#paging nav.d-none ul.pagination")/li[last()-1]/a')) or 1
    return true
  else
    return false
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID = '2784b193e1794dcdac1f68eb59e9ad0c'
  m.Category='English'
  m.Name='MangaParkOrg'
  m.RootURL='https://mangapark.org'
  m.LastUpdated = 'April 09, 2019'
  m.SortedList = true
  m.OnGetInfo='GetInfo'
  m.OnGetPageNumber='GetPageNumber'
  m.OnGetNameAndLink='GetNameAndLink'
  m.OnGetDirectoryPageNumber='getdirectorypagenumber'
end
