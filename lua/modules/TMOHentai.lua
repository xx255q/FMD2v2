function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title=x.XPathString('//h3')
		MANGAINFO.CoverLink=x.XPathString('//*[@alt="cover"]/@src')
		MANGAINFO.Artists=x.XPathStringAll('//*[@class="tag tag-accepted"][contains(@href,"=artist")]/text()')
		MANGAINFO.Genres=x.XPathStringAll('//*[@class="list-inline"][2]/a/text()')
		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	TASK.PageNumber=0
	URL = MODULE.RootURL..URL..'/paginated/1'
	URL = string.gsub(URL, 'contents', 'reader')
	if HTTP.GET(URL) then
		TASK.PageNumber=CreateTXQuery(HTTP.Document).XPathCount('//select[@id="select-page"]/option')
	else
		return false
	end
	return true
end

function getimageurl()
	URL = MODULE.RootURL..URL..'/paginated/'
	URL = string.gsub(URL, 'contents', 'reader')
	if HTTP.GET(URL..(WORKID+1)) then
		local img = CreateTXQuery(HTTP.Document).XPathString('//*[@class="content-image lazy"]/@data-original')
		TASK.PageLinks[WORKID]=MODULE.RootURL..img
		return true
	end
	return false
end

function getnameandlink()
	local s = '/section/all?page='..(URL + 1)
	if HTTP.GET(MODULE.RootURL .. s) then
		local x = CreateTXQuery(HTTP.Document)
		local v = x.XPath('//*[@class="content-title truncate"]/a')
		local hasTitles = false
		for i = 1, v.Count do
			local v1 = v.Get(i)
			LINKS.Add(v1.GetAttribute('href'))
			NAMES.Add(v1.GetAttribute('title'))
			hasTitles = true
		end
		if hasTitles then
			UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1
		end
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '499c85a7b2e542609cb564a196dffb40'
	m.Category='H-Sites'
	m.Name='TMOHentai'
	m.RootURL='https://www.tmohentai.com'
	m.LastUpdated='May 20, 2019'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetImageURL='getimageurl'
	m.OnGetNameAndLink='getnameandlink'
	m.SortedList=true
end
