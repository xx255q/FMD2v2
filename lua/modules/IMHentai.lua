function Init()
	local m = NewWebsiteModule()
	m.ID                         = '67e22e0c766c4c9c990e179350262b3c'
	m.Name                       = 'IMHentai'
	m.RootURL                    = 'https://imhentai.com'
	m.Category                   = 'H-Sites'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()-1]/a'))
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/?page=' .. (URL + 1)) then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//h2[@class="gallery_title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//div[contains(@class,"right_details")]/h1')
		MANGAINFO.CoverLink  = x.XPathString('//div[contains(@class,"left_cover")]//img/@src')
		MANGAINFO.Artists    = x.XPathStringAll('//ul[@class="galleries_info"]/li[contains(.,"Artists")]/a/text()[not(parent::span)]')
		MANGAINFO.Genres     = x.XPathStringAll('//ul[@class="galleries_info"]/li[contains(.,"Tags")]/a/text()[not(parent::span)]')

		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local x = CreateTXQuery(HTTP.Document)
		local server    = x.XPathString('//*[@id="load_server"]/@value')
		local dir       = x.XPathString('//*[@id="load_dir"]/@value')
		local id        = x.XPathString('//*[@id="load_id"]/@value')
		TASK.PageNumber = tonumber(x.XPathString('//*[@id="load_pages"]/@value'))
		for i = 1, TASK.PageNumber do
			TASK.PageLinks.Add('https://' .. 'm' .. server .. '.imhentai.com' .. '/' .. dir .. '/' .. id .. '/' .. i .. '.jpg')
		end
		return true
	else
		return false
	end
end