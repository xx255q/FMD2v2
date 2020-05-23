function Init()
	local m = NewWebsiteModule()
	m.ID                         = '9f2d722f3e90432ba913d5bdddc048b4'
	m.Name                       = 'Tapas'
	m.RootURL                    = 'https://tapas.io'
	m.Category                   = 'Webcomics'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
end

local dirurl = '/comics?sortType=TITLE&browse=ALL'

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. dirurl) then
		PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).x.XPathString('//div[@class="global-pagination-wrap"]/a[last()-1]')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local s = MODULE.RootURL .. dirurl
	if URL ~= '0' then
		s = s .. '&pageNumber=' .. IncStr(URL)
	end
	if HTTP.GET(s) then
		TXQuery.Create(HTTP.Document).XPathHREFAll('//ul[contains(@class,"content-list-wrap")]//li//a[@class="preferred title"]', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)

		MANGAINFO.CoverLink = x.XPathString('//a[@id="series-thumb"]/img/@src')
		if MANGAINFO.CoverLink == '' then
			MANGAINFO.CoverLink = x.XPathString('//script[contains(.,"has-thumb")]/substring-before(substring-after(.,"src="""),"""")')
		end
		MANGAINFO.Title     = x.XPathString('//a[@class="series-header-title"]/text()')
		MANGAINFO.Authors   = x.XPathStringAll('//a[@class="name"]/span/text()')
		MANGAINFO.Genres    = x.XPathString('//div[@class="tags"]/string-join(./*,", ")')
		MANGAINFO.Summary   = x.XPathString('//span[@id="series-desc-body"]'):gsub('  ', ' ')

		local locked = ''
		local v for _,v in ipairs(x.XPathI('json(//script[contains(.,"var _data")]/concat(substring-before(substring-after(.,"episodeList :"),"]"),"]"))()')) do
			MANGAINFO.ChapterLinks.Add(MODULE.RootURL .. '/episode/' .. x.XPathString('./id', v))
			locked = ''
			if x.XPathString('./locked', v) == 'true' then
				locked = ' [locked]'
			end
			MANGAINFO.ChapterNames.Add(x.XPathString('./title', v) .. locked)
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		for _, v in ipairs(TXQuery.Create(HTTP.Document).XPathI('//img[@class="art-image"]/@src')) do
			TASK.PageLinks.Add(v.ToString():gsub('[%?%&]type=%w%d+', ''))
		end
		return true
	else
		return false
	end
end

function BeforeDownloadImage()
	if TASK.CurrentDownloadChapterPtr < TASK.ChapterLinks.Count then
		HTTP.Headers.Values['Referer'] = ' ' .. MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	end
	return true
end
