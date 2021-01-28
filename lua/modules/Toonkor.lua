local dirurl = '/%EC%9B%B9%ED%88%B0'
local dirpages = {'%EC%9B%94', '%ED%99%94', '%EC%88%98', '%EB%AA%A9', '%EA%B8%88', '%ED%86%A0', '%EC%97%B4%ED%9D%98'}

function Init()
	local m = NewWebsiteModule()
	m.ID                      = '2a7d69a1e1d24f90851b4e2598cffdcd'
	m.Name                    = 'Toonkor'
	m.RootURL                 = 'https://tkor.cloud'
	m.Category                = 'Raw'
	m.OnGetNameAndLink        = 'GetNameAndLink'
	m.OnGetInfo               = 'GetInfo'
	m.OnGetPageNumber         = 'GetPageNumber'
	m.TotalDirectory          = #dirpages
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. dirurl .. '/' .. dirpages[MODULE.CurrentDirectoryIndex + 1]) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//div[@class="section-item-title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('//table[@class="bt_view1"]//td[@class="bt_title"]')
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//*[@class="bt_thumb"]/a/img/@src'))
		MANGAINFO.Authors   = x.XPathString('//table[@class="bt_view1"]//span[contains(., "작가")]/following-sibling::span')
		MANGAINFO.Summary   = x.XPathString('//table[@class="bt_view1"]//td[@class="bt_over"]')
		local v for v in x.XPath('//table[@class="web_list"]/tbody//tr/td[@class="content__title"]').Get() do
			MANGAINFO.ChapterLinks.Add(v.GetAttribute('data-role'))
			MANGAINFO.ChapterNames.Add(v.ToString())
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local crypto = require 'fmd.crypto'
		local x = CreateTXQuery(HTTP.Document)
		local s = x.XPathString('//script[contains(., "toon_img")]')
		x.ParseHTML(crypto.DecodeBase64(GetBetween("toon_img = '", "';", s)))
		local v for v in x.XPath('//img/@src').Get() do
			TASK.PageLinks.Add(MaybeFillHost(MODULE.RootURL, v.ToString()))
		end
	else
		return false
	end
	return true
end