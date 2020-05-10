function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  	if HTTP.GET(MANGAINFO.URL) then
    	x=TXQuery.Create(HTTP.Document)
    	MANGAINFO.Title     = x.XPathString('//title'):gsub('미리보기', '')
    	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//meta[@property="og:image"]/@content'))
    	MANGAINFO.Authors   = x.XPathString('//meta[@property="og:author"]/@content')
    	MANGAINFO.Summary   = x.XPathString('//meta[@property="og:description"]/@content')
    	local v = x.XPath('//ul[contains(@class, "list-body")]//li')
	    for i = 1, v.Count do
	      local v1 = v.Get(i)
	      local name = v1.GetAttribute('href')
	      MANGAINFO.ChapterNames.Add(Trim(x.XPathString('.//div[contains(@class, "wr-subject")]/a', v1)))
	      MANGAINFO.ChapterLinks.Add(x.XPathString('.//div[contains(@class, "wr-subject")]//a/@href', v1));
	    end
    	InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    	return no_error
  	else
    	return net_problem
  end
end

function getpagenumber()
	TASK.PageNumber=0
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		local x = TXQuery.Create(HTTP.Document)
		x.XPathStringAll('//div[contains(@class, "view-content")]//img/@data-original', TASK.PageLinks)
		return true
	else
		return false
	end
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL..'/webtoon/p'.. IncStr(URL)..'?toon=일반웹툰') then
		local x=TXQuery.Create(HTTP.Document)
		  local v = x.XPath('//*[contains(@id, "webtoon-list-all")]//li')
		  for i = 1, v.Count do
	      local v1 = v.Get(i)
		    NAMES.Add(Trim(x.XPathString('.//*[contains(@class, "title")]', v1)));
	      	LINKS.Add(x.XPathString('.//*[contains(@class, "trans-bg-black")]/a/@href', v1));
	    end
	    p = tonumber(100) --limit 100 FMD freze if up to 500
	    if p ~= nil then
	      UPDATELIST.CurrentDirectoryPageNumber = p
	    end
		return no_error
	else
	    return net_problem
	end
end

function Init()
	local m=NewWebsiteModule()
	m.ID='8b8a11bb9b0e4cd8b30c8577c762d19c'
  	m.Category='Webcomics'
  	m.Name='NewToki'
  	m.RootURL='https://newtoki34.com'
  	m.OnGetInfo='getinfo'
  	m.OnGetPageNumber='getpagenumber'
  	m.OnGetNameAndLink='getnameandlink'
end