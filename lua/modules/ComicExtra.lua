function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title=x.XPathString('//h1[1]/span')
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="movie-l-img"]/img/@src'))
    MANGAINFO.Authors=x.XPathString('//dt[contains(., "Author")]/following-sibling::dd')
    MANGAINFO.Genres=x.XPathStringAll('//dt[contains(., "Genre")]/following-sibling::dd/a')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//dt[contains(., "Status")]/following-sibling::dd'))
    MANGAINFO.Summary=x.XPathString('//div[@id="film-content"]')

    local spages = x.XPathString('//div[@class="general-nav"]/a[last()]/@href')
		  spages = string.sub(spages, string.len(spages) - 1, string.len(spages))
		  spages = spages:gsub('/', '')
		  if spages == '' or spages == 'N/A' then
			 spages = 1
		  end
	local pages = tonumber(spages)
	local p = 1
    while p <= pages do
      if p >= 1 then
        if HTTP.GET(MANGAINFO.URL..'/'..p) then
			x=TXQuery.Create(HTTP.Document)
        else
          break
        end
      end

      if p == pages then
        local spg = x.XPathString('//div[@class="general-nav"]/a[last()]/@href')
		      spg = string.sub(spg, string.len(spg) - 1, string.len(spg))
		      spg = spg:gsub('/', '')
			  if spg == '' or spg == 'N/A' then
				 spg = 1
			  end
		local pg = tonumber(spg)
		if pg ~= '' then pages = pg end
      end
      local v=x.XPath('//tbody[@id="list"]/tr/td/a')
      for i=1,v.Count do
        local v1=v.Get(i)
        MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'))
        MANGAINFO.ChapterNames.Add(v1.ToString())
      end
      p = p + 1
    end

	InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  TASK.PageNumber = 0
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL .. '/full')) then
    local x=TXQuery.Create(HTTP.Document)
    x.XPathStringAll('//img[@class="chapter_img"]/@src', TASK.PageLinks)
  else
    return false
  end
  return true
end

function getnameandlink()
  if HTTP.GET(MODULE.RootURL .. '/comic-list') then
    local x = TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('//div[@class="serie-box"]/ul/li/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID = '041471dc634f4b639097ae0fede4bf7f'
  m.Name = 'ComicExtra'
  m.RootURL = 'http://www.comicextra.com'
  m.Category = 'English'
  m.LastUpdated='May 7, 2019'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetNameAndLink='getnameandlink'
end
