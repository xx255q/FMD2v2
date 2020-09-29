function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'abe29b9f5f9e4fff94068fe547a93cef'
	m.Name                       = 'Kraw'
	m.RootURL                    = 'https://kraw.me'
	m.Category                   = 'Raw'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/all/') then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//h2[@class="eael-post-list-title"]/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//h2[contains(@class, "elementor-heading-title")]')
		MANGAINFO.CoverLink  = x.XPathString('//div[contains(@class, "bdt-gallery-thumbnail")]/img/@data-lazy-src')
		MANGAINFO.Authors    = x.XPathString('//ul[@class="elementor-icon-list-items"]/li[2]/span/substring-after(.,":")')
		MANGAINFO.Status     = MangaInfoStatusIfPos(x.XPathString('//ul[@class="elementor-icon-list-items"]/li[4]/span'))
		MANGAINFO.Summary    = x.XPathString('//div[@class="premium-unfold-editor-content"]/p')

		x.XPathHREFAll('//h3[@class="elementor-post__title"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="gallery"]/img/@data-lazy-src', TASK.PageLinks)
		return true
	else
		return false
	end
end