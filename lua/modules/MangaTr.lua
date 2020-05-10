local mangatrdirurl = '/manga-list.html?listType=allABC'
local puzzmosdirurl = '/directory?type=text'

function getinfo()
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
	  local imgurl = x.XPathString('//img[@class="thumbnail"]/@src')

	  MANGAINFO.CoverLink = MODULE.RootURL..'/'..imgurl
	  MANGAINFO.Title = x.XPathString('//title')
	  if MANGAINFO.Title > '' then
		  if (Pos('Manga - Oku', MANGAINFO.Title) > 0) then
			  MANGAINFO.Title = Trim(x.XPathString('//title/substring-after(substring-before(., " Manga - Oku "), "Read")'))
		  end
		  if (Pos('Mangasını Oku', MANGAINFO.Title) > 0) then
			  MANGAINFO.Title = Trim(x.XPathString('//title/substring-after(substring-before(., " Mangasını Oku "), "Read")'))
		  end
	  end

	  if MANGAINFO.Title == '' then
	    MANGAINFO.Title = x.XPathString('//h1')
	  end

	  if (Pos('Yazar', x.XPathString('//table[1]/tbody/tr[1]/td[1]')) > 0) then
      MANGAINFO.Authors = x.XPathString('//table[1]/tbody/tr[2]/td[1]')
		  MANGAINFO.Genres = Trim(x.XPathString('//table[1]/tbody/tr[2]/td[3]'))
    else
		  MANGAINFO.Authors = x.XPathString('//table[2]/tbody/tr[2]/td[1]')
		  MANGAINFO.Genres = Trim(x.XPathString('//table[2]/tbody/tr[2]/td[3]'))
	  end

	  MANGAINFO.Summary = x.XPathString('//*[@class="well"]/p')

    local info = x.XPathString('//*[@slug]/@slug')
	  local pages = 2
	  local p = 1
    while p <= pages do
      if p >= 1 then
        HTTP.Reset()
		    HTTP.Headers.Values['Cache-Control'] = 'no-cache'
		    HTTP.Headers.Values['content-type'] = 'application/x-www-form-urlencoded; charset=UTF-8'
		    HTTP.Headers.Add('X-Requested-With: XMLHttpRequest')
		    if HTTP.POST(MODULE.RootURL .. '/cek/fetch_pages_manga.php?manga_cek='..info, 'page='..p) then
			    x=TXQuery.Create(HTTP.Document)
        else
          break
        end
      end

      if p == pages then
        local pg = x.XPathString('//*[@class="last"]/a/@data-page')
        if pg ~= '' then pages = tonumber(pg) end
      end
      local v=x.XPath('//tr/td[1]/a')
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

function getnameandlink()
  local URL = MODULE.RootURL
  if MODULE.Name == 'Manga-Tr' then
    URL = URL..mangatrdirurl
  end

  if MODULE.Name == 'Puzzmos' then
    URL = URL..puzzmosdirurl
  end

  if HTTP.GET(URL) then
	  local x=TXQuery.Create(HTTP.Document)
	  local v=x.XPath('//tr/td[1]/a')
		if v.Count == 0 then
		   v=x.XPath('//*[@data-toggle="mangapop"]/b/a')
		end
    for i=1,v.Count do
      local v1=v.Get(i)
		  if v1.ToString() > '' or v1.ToString() > 'N/A' then
		    LINKS.Add(v1.GetAttribute('href'))
		    NAMES.Add(v1.ToString())
		  end
    end
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
    local x=TXQuery.Create(HTTP.Document)
    TASK.PageNumber = x.XPathCount('//div[@class="chapter-content"]/select[2]/option/@value')
  else
    return false
  end
  return true
end

function getimageurl()
  local s = URL:gsub('.html', '')..'-page-'..(WORKID+1)..'.html'
  if HTTP.GET(MaybeFillHost(MODULE.RootURL,s)) then
    local x=TXQuery.Create(HTTP.Document)
	local imgurl = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="chapter-content"]//img[@class="chapter-img"]/@src'))
    if imgurl > '' or imgurl > 'N/A' then
		TASK.PageLinks[WORKID] = imgurl
	else
	    TASK.PageLinks[WORKID] = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="chapter-content"]//img/@src'))
	end
  	return true
  end
  return false
end

function AddWebsiteModule(id, site, url)
  local m=NewWebsiteModule()
  m.ID=id
  m.Category='Turkish'
  m.Name=site
  m.RootURL=URL
  m.LastUpdated = 'May 06, 2019'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber='getpagenumber'
  m.OnGetNameAndLink='getnameandlink'
  m.OnGetImageURL='getimageurl'
end

function Init()
  AddWebsiteModule('bcf6bf0a1b5d4cffa6cc6743e29ee5f2', 'Manga-Tr', 'https://manga-tr.com')
  AddWebsiteModule('85bcc0c0b773495d984cedb6670f78bb', 'Puzzmos', 'http://puzzmos.com')
end