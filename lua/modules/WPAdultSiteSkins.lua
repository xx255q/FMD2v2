function Init()
	function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                         = id
		m.Name                       = name
		m.RootURL                    = url
		m.Category                   = 'Adult'
		m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink           = 'GetNameAndLink'
		m.OnGetInfo                  = 'GetInfo'
		m.OnGetPageNumber            = 'GetPageNumber'
		m.OnGetImageURL              = 'GetImageURL'
		m.SortedList                 = true
	end
	AddWebsiteModule('8760050d777d432dab8e776e3f1a6474', 'PornComix', 'http://www.porncomix.info')
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).x.XPathString('//div[@class="paginator"]/span[starts-with(.,"Page") and contains(.," of ")]/normalize-space(substring-after(.," of "))')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local s = MODULE.RootURL
	if URL ~= '0' then
		s = s .. '/page/' .. IncStr(URL)
	end
	if HTTP.GET(s) then
		TXQuery.Create(HTTP.Document).XPathHREFTitleAll('//div[@class="posts"]/div[starts-with(@id,"post-")]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)

		MANGAINFO.CoverLink = x.XPathString('//div[@class="single-post"]//img[starts-with(@class,"attachment-") and not(@data-lazy-src)]/@src')
		if MANGAINFO.CoverLink == '' then
			MANGAINFO.CoverLink = x.XPathString('//div[@class="single-post"]/p//img[not(@data-lazy-src)]/@src')
		end
		if MANGAINFO.CoverLink ~= '' then
			MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, MANGAINFO.CoverLink)
		end
		MANGAINFO.Title     = x.XPathString('//div[@class="posts"]/h2[@class="post-title"][1]')

		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = TXQuery.Create(HTTP.Document)
		x.XPathStringAll('//div[@class="single-post"]//dl[@class="gallery-item"]/dt/a/@href', TASK.PageContainerLinks)
		x.XPathStringAll('//div[@class="single-post"]/p//a[./img[not(data-lazy-src)]]/@href', TASK.PageContainerLinks)
		if TASK.PageContainerLinks.Count == 0 then
			local v for _,v in ipairs(x.XPathI('//div[@class="single-post"]/p//img[not(@data-lazy-src)]/@src')) do
				TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
			end
		else
			TASK.PageNumber = TASK.PageContainerLinks.Count
		end
		return true
	else
		return false
	end
end

function GetImageURL()
	require 'extras.imagehoster'
	local s = GetImageHosterDirectURL(MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID]))
	if s ~= '' then
		return true
	elseif HTTP.GET(MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])) then
			TASK.PageLinks[WORKID] = MaybeFillHost(MODULE.RootURL, TXQuery.Create(HTTP.Document).XPathString('//div[@class="attachment-image"]//img/@src'))
		return true
	else
		return false
	end
end
