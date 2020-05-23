function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=TXQuery.Create(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//h1')
		MANGAINFO.CoverLink=x.XPathString('//*[@class="detail_info clearfix"]/img/@src')
		MANGAINFO.Authors=x.XPathString('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Author(s):")]/substring-after(.,":")')
		MANGAINFO.Artists=x.XPathString('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Artist(s):")]/substring-after(.,":")')
		MANGAINFO.Genres=x.XPathString('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Genre(s):")]/substring-after(.,":")')
		MANGAINFO.Status=MangaInfoStatusIfPos(x.XPathString('//*[@class="detail_info clearfix"]/ul/li[starts-with(.,"Status(s):")]'))
		MANGAINFO.Summary=x.XPathString('//*[@class="detail_info clearfix"]/ul/li/span[@id="show"]/normalize-space(text())')
		v=x.XPath('//ul[@class="chapter_list"]/li')
		for i=1,v.Count do
			v2=v.Get(i)
			MANGAINFO.ChapterLinks.Add(x.XPathString('a/@href',v2))
			MANGAINFO.ChapterNames.Add(x.XPathString('string-join((a/text(),span[not(@class)])," ")',v2))
		end
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		TASK.PageNumber=TXQuery.Create(HTTP.Document).XPathCount('(//select[not(@id)])[1]/option[not(contains(@value,"featured.html"))]')
		return true
	else
		return false
	end
	return true
end

function getimageurl()
	local s=URL
	if WORKID>0 then
	 s=AppendURLDelim(s)..(WORKID+1)..'.html'
	end
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,s)) then
		TASK.PageLinks[WORKID]=TXQuery.Create(HTTP.Document).XPathString('//*[@id="viewer"]//img[@alt]/@src')
		return true
	end
	return false
end

function getdirectorypagenumber()
	if HTTP.GET(MODULE.RootURL..'/directory/?name.az') then
		PAGENUMBER=TXQuery.Create(HTTP.Document).XPathCount('(//select)[last()]/option')
		return no_error
	else
		return net_problem
	end
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL..'/directory/'..IncStr(URL)..'.htm?name.az') then
		TXQuery.Create(HTTP.Document).XPathHREFTitleAll('//ul[@class="manga_pic_list"]/li/a',LINKS,NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '4b5f8afd9a174af7b386a6de8ed83a2f'
	m.Category='English'
	m.Name='MangaTown'
	m.RootURL='http://www.mangatown.com'
	m.LastUpdated='February 17, 2018'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetImageURL='getimageurl'
	m.OnGetDirectoryPageNumber='getdirectorypagenumber'
	m.OnGetNameAndLink='getnameandlink'
end
