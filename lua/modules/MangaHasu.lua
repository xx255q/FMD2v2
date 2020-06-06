function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//div[@class="info-title"]/h1')
		MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[contains(@class, "info-img")]/img/@src'))
		MANGAINFO.Authors=x.XPathStringAll('//div[contains(b, "Author")]/span/a')
		MANGAINFO.Artists=x.XPathStringAll('//div[contains(b, "Artist")]/span/a')
		MANGAINFO.Genres=x.XPathStringAll('//div[contains(b, "Genre")]/span/a')
		MANGAINFO.Status=MangaInfoStatusIfPos(x.XPathString('//div[contains(b, "Status")]/span/a'))
		MANGAINFO.Summary=x.XPathString('//h3[contains(.,"Summary")]/following-sibling::*')
		x.XPathHREFAll('//div[@class="list-chapter"]/table//td[@class="name"]/a',MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function beforedownloadimage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL
	return true
end

function getpagenumber()
	TASK.PageLinks.Clear()
	TASK.PageNumber = 0
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x=CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//div[@class="img"]/img/@src', TASK.PageLinks)
	else
		return false
	end
	return true
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL .. '/directory.html?page=' .. (URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//ul[@class="list_manga"]/li//a[@class="name-manga"]', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL .. '/directory.html') then
		x = CreateTXQuery(HTTP.Document)
		PAGENUMBER = tonumber(x.XPathString('//div[@class="pagination-ct"]/a[last()]/substring-after(@href,"=")')) or 1
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = 'e30f500d4eb0413e858dc071616228f1'
	m.Name = 'MangaHasu'
	m.RootURL = 'http://mangahasu.se'
	m.Category = 'English'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
	m.OnBeforeDownloadImage='beforedownloadimage'
	m.OnGetDirectoryPageNumber='getdirectorypagenumber'
end