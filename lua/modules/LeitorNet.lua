function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    x=TXQuery.Create(HTTP.Document)
    MANGAINFO.Title=x.XPathString('//*[@id="series-data"]//*[@class="series-title"]/h1')
    MANGAINFO.CoverLink=x.XPathString('//*[@id="series-data"]//img[@class="cover"]/@src')
    MANGAINFO.Authors=x.XPathString('//*[@id="series-data"]//*[@class="series-author"]/normalize-space(text())')
    MANGAINFO.Genres=x.XPathStringAll('//*[@id="series-data"]//ul[contains(@class, "tags")]/li/a')
    if x.XPathString('//*[@id="series-data"]//*[@class="complete-series"]') == '' then
      MANGAINFO.Status = '1'
    else
      MANGAINFO.Status = '0'
    end
    MANGAINFO.Summary=x.XPathString('//*[@id="series-data"]//*[@class="series-desc"]')

    local id = x.XPathString('//ul[@data-id-serie]/@data-id-serie')
    local page = 1
    while true do
      if HTTP.XHR(MODULE.RootURL..'/series/chapters_list.json?page='..tostring(page)..'&id_serie='..id) then
        if HTTP.Terminated then break end
        x=TXQuery.Create(HTTP.Document)
        if x.XPathString('json(*).chapters'):lower() == 'false' then break end
        v=x.XPath('json(*).chapters()')
        for i=1,v.Count do
          v1=v.Get(i)
          w=x.XPath('./releases/*', v1)
          for j=1,w.Count do
            w1=w.Get(j)
			local s = x.XPathString('./concat(number, " - ", chapter_name)', v1)
			local sc = x.XPathString('./scanlators[1]/name', w1)
			if sc ~= '' then
			  s = s .. ' [' .. sc .. ']'
			end
			MANGAINFO.ChapterNames.Add(s)
			MANGAINFO.ChapterLinks.Add(x.XPathString('./link', w1))
          end
        end
        page=page+1
      else
        break
      end
    end
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function getpagenumber()
  TASK.PageLinks.Clear()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
    x=TXQuery.Create(HTTP.Document)
	s=x.XPathString('//script[contains(@src, "token=")]/@src')
	local token = s:match('%&token=(%w+)&?')
	local id = s:match('&id_release=(%w+)&?')
	if HTTP.GET(MODULE.RootURL .. string.format('/leitor/pages/%s.json?key=%s', id, token)) then
	  x=TXQuery.Create(HTTP.Document)
	  x.XPathStringAll('json(*).images()', TASK.PageLinks)
    TASK.PageLinks[0] = MaybeFillHost(MODULE.RootURL, TASK.PageLinks[0])
	else
	  return false
	end
  else
    return false
  end
  return true
end

function getnameandlink()
  if HTTP.GET(MODULE.RootURL..'/lista-de-mangas/ordenar-por-nome/todos?page=' .. IncStr(URL)) then
    local x=TXQuery.Create(HTTP.Document)
    local p=x.XPathString('//ul[contains(@class,"content-pagination")]/li[last()-1]/a')
    p = tonumber(p)
    if p ~= nil then
      UPDATELIST.CurrentDirectoryPageNumber = p
    end
    x.XPathHREFTitleAll('//ul[@class="seriesList"]/li/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function Init()
  local m = NewWebsiteModule()
  m.ID               = 'f3be3b2ff8134d1ea5bcc880b735b107'
  m.Category         = 'Portuguese'
  m.Name             = 'LeitorNet'
  m.RootURL          = 'https://leitor.net'
  m.OnGetInfo        = 'getinfo'
  m.OnGetPageNumber  = 'getpagenumber'
  m.OnGetNameAndLink = 'getnameandlink'
end