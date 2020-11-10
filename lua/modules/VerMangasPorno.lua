function Init()
	local function AddWebsiteModule(id, name, url)
		local m = NewWebsiteModule()
		m.ID                         = id
		m.Name                       = name
		m.RootURL                    = url
		m.Category                   = 'H-Sites'
		m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
		m.OnGetNameAndLink           = 'GetNameAndLink'
		m.OnGetInfo                  = 'GetInfo'
		m.OnGetPageNumber            = 'GetPageNumber'
	end
	AddWebsiteModule('c3a863e704054a1a86eeafbbaae67513', 'VerMangasPorno', 'https://vermangasporno.com')
	AddWebsiteModule('951d32dd4fc4468692d3a3b7572a707c', 'VerComicsPorno', 'https://vercomicsporno.com')
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('//ul[@class="pagination"]/li[last()]/a/@href'):match('page/(%d+)')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/page/' .. (URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		local v; for v in x.XPath('//div[@class="gallery"]').Get() do
			LINKS.Add(x.XPathString('a/@href', v))
			NAMES.Add(x.XPathString('a/img/@alt', v))
		end
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = CreateTXQuery(HTTP.Document)
		if MODULE.ID == 'c3a863e704054a1a86eeafbbaae67513' then -- vermangasporno
			MANGAINFO.CoverLink  = x.XPathString('//div[@class="comicimg"]/p/img/@data-lazy-src')
		else
			MANGAINFO.CoverLink  = x.XPathString('//div[@class="comicimg"]/p/img/@src')
		end
		MANGAINFO.Title      = x.XPathString('//title')
		MANGAINFO.Genres     = x.XPathStringAll('//div[@id="tagsin"]/a')

		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		if MODULE.ID == 'c3a863e704054a1a86eeafbbaae67513' then -- vermangasporno
			CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="comicimg"]/p/img/@data-lazy-src', TASK.PageLinks)
		else
			CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="comicimg"]/p/img/@src', TASK.PageLinks)
		end
		return true
	else
		return false
	end
end