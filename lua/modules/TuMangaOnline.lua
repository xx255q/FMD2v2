function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		if MANGAINFO.Title == '' then
			MANGAINFO.Title = Trim(x.XPathString('//h1[contains(@class, "element-title")]/text()'))
		end
		MANGAINFO.CoverLink = x.XPathString('//img[contains(@class,"book-thumbnail")]/@src')
		MANGAINFO.Genres=x.XPathStringAll('//a[contains(@class, "badge")]')
		MANGAINFO.Authors=Trim(x.XPathString('//span[@class="list-group-item" and contains(., "Autor")]/a'))
		MANGAINFO.Artists=Trim(x.XPathString('//span[@class="list-group-item" and contains(., "Artist")]/a'))
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//span[contains(@class, "book-status")]'), 'public', 'final')
		MANGAINFO.Summary = x.XPathStringAll('//*[@class="element-description"]/text()', '')

		local EncodeURLElement = require "fmd.crypto".EncodeURLElement
		local v, vi, vii, ctitle, scanname, viewerurl, formparams
		for v in x.XPath('//div[contains(@class, "chapters")]/ul//li[./h4]').Get() do
			ctitle = x.XPathString('h4', v)
			for vi in x.XPath('div//ul/li/div', v).Get() do
				scanname = ' [' .. x.XPathString('div[1]', vi) .. ']'
				viewerurl = x.XPathString('div/a[contains(@class,"btn-sm")]/@href', vi)
				if viewerurl == '' then
					viewerurl = x.XPathString('div/form/@action', vi)
					formparams = ''
					for vii in x.XPath('div/form/input', vi).Get() do
						formparams = formparams .. '&' .. vii.GetAttribute('name') .. '=' .. EncodeURLElement(vii.GetAttribute('value'))
					end
					viewerurl = viewerurl .. formparams:gsub('^&','?')
				end
				MANGAINFO.ChapterNames.Add(ctitle .. ' ' .. scanname)
				MANGAINFO.ChapterLinks.Add(viewerurl)
				MANGAINFO.ChapterNames.Add(ctitle .. ' ' .. scanname)
				MANGAINFO.ChapterLinks.Add(viewerurl)
			end
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	HTTP.Headers.Values['Referer'] = ' ' .. MaybeFillHost(MODULE.RootURL, TASK.Link)
	local url = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(url) then return false; end
	local rurl = HTTP.Headers.Values['location']
	if rurl ~= '' then
		HTTP.Headers.Values['Referer'] = ' ' .. url
		if not HTTP.GET(rurl) then return false; end
	end

	local x = CreateTXQuery(HTTP.Document)
	TASK.PageNumber = tonumber(x.XPathString('count((//select[@id="viewer-pages-select"])[1]/option)')) or 1
	TASK.PageContainerLinks.Add(HTTP.LastURL)
	TASK.PageLinks.Add(x.XPathString('//img[contains(@class,"viewer-image")]/@src'))
	return true
end

function getimageurl()
	local s = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[0])
	HTTP.Headers.Values['Referer'] = ' ' .. MaybeFillHost(s)
	if HTTP.GET(s .. '/' .. tostring(WORKID+1)) then
		TASK.PageLinks[WORKID] = CreateTXQuery(HTTP.Document).XPathString('//img[contains(@class,"viewer-image")]/@src')
		return true
	end
	return false
end

function getnameandlink()
	local s = '/library?order_item=alphabetically&order_dir=asc&filter_by=title&page='..(URL + 1)
	if HTTP.GET(MODULE.RootURL .. s) then
		local x = CreateTXQuery(HTTP.Document)
		local v = x.XPath('//*[@data-identifier]/a')
		local hasTitles = false
		for i = 1, v.Count do
			local v1 = v.Get(i)
			LINKS.Add(v1.GetAttribute('href'))
			NAMES.Add(x.XPathString('div/div[@class="thumbnail-title"]', v1))
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
	m.ID = '9185eb6c49324a849c7d7925a41ef3a3'
	m.Name = 'Tumangaonline'
	m.RootURL = 'https://lectortmo.com'
	m.Category = 'Spanish'
	m.MaxTaskLimit = 1
	m.MaxConnectionLimit = 1
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
	m.OnGetImageURL='getimageurl'
end
