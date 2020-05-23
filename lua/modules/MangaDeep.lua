----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

DirectoryPagination = '/category/index_'
DirectorySuffix     = '.html'
MangaInfoParameters = '?waring=1'

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL) .. MangaInfoParameters

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	MANGAINFO.Title     = Trim(SeparateLeft(x.XPathString('//div[@class="book-info"]/h1'), ' Manga'))
	MANGAINFO.CoverLink = x.XPathString('//div[@class="book-info"]//img/@src')
	MANGAINFO.Authors   = x.XPathString('string-join(//dd[@class="about-book"]//span[starts-with(.,"Author")]/following-sibling::a)')
	MANGAINFO.Genres    = x.XPathString('string-join(//ul[@class="inset-menu"]/li/a[not(contains(.,"Manga Reviews"))], ", ")')
	MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//dd[@class="about-book"]//span[starts-with(.,"Status")]/following-sibling::a'));
	MANGAINFO.Summary   = x.XPathString('//dd[@class="short-info"]//span')

	for _, v in ipairs(x.XPathI('//ul[@class="chapter-box"]/li//a')) do
		MANGAINFO.ChapterNames.Add(x.XPathString('text()[not(parent::span/@class="new_up")]', v))
		MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
	end
	InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)

	return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
	local v, x = nil
	local u = MODULE.RootURL .. DirectoryPagination .. IncStr(URL)

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	if x.XPath('//dd[@class="book-list"]/a[not(@class="follow")]').Count == 0 then return no_error end
	for _, v in ipairs(x.XPathI('//dd[@class="book-list"]/a[not(@class="follow")]')) do
		LINKS.Add(v.GetAttribute('href'))
		NAMES.Add(x.XPathString('b', v))
	end
	UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1

	return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
	local x = nil
	local u = MaybeFillHost(MODULE.RootURL, URL)

	if not HTTP.GET(u) then return net_problem end

	x = TXQuery.Create(HTTP.Document)
	x.XPathStringAll('(//select[@class="sl-page"])[last()]/option/@value', TASK.PageContainerLinks)
	TASK.PageNumber = TASK.PageContainerLinks.Count

	return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
	local u = MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID])

	if HTTP.GET(u) then
		TASK.PageLinks[WORKID] = TXQuery.Create(HTTP.Document).XPathString('//img[contains(@class,"manga_pic")]/@src')
		return true
	end

	return false
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
	function AddWebsiteModule(name, url, category)
		local m = NewWebsiteModule()
		m.ID                       = id
		m.Name                     = name
		m.RootURL                  = url
		m.Category                 = category
		m.OnGetInfo                = 'GetInfo'
		m.OnGetNameAndLink         = 'GetNameAndLink'
		m.OnGetPageNumber          = 'GetPageNumber'
		m.OnGetImageURL            = 'GetImageURL'
	end
	AddWebsiteModule('742219d5bd524064a608c7521539372e', 'MangaDeep', 'http://www.mangadeep.com', 'English')
	AddWebsiteModule('64aab3bdbc264cda9c0ef2a2e8e71929', 'Manga99', 'http://www.manga99.com', 'English')
	AddWebsiteModule('7acb300c93504802b08657ba374d8bfb', 'TenManga', 'http://www.tenmanga.com', 'English')
end