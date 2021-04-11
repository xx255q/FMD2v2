function Init()
	local m = NewWebsiteModule()
	m.ID                         = '3d15f8c9aa8a43788bf827318d41f188'
	m.Name                       = 'HentaiMoney'
	m.RootURL                    = 'https://hentai.money'
	m.Category                   = 'H-Sites'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/homeindex/' .. (URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		if x.XPath('json(*).data()').Count == 0 then return no_error end
		local v for v in x.XPath('json(*).data()').Get() do
			LINKS.Add('/book/'.. x.XPathString('channel', v))
			NAMES.Add(x.XPathString('Name', v))
		end
		UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	if URL:find('/1$') then
		MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL):gsub('/1$', '')
	else
		MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	end
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = x.XPathString('json(*).name')
		MANGAINFO.CoverLink = x.XPathString('json(*).cover')
		MANGAINFO.Summary   = x.XPathString('json(*).desc')

		MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('json(*).images()', TASK.PageLinks)
		return true
	else
		return false
	end
end
