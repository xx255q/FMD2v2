function Init()
	local m = NewWebsiteModule()
	m.ID                         = '2f5e1b629ce148988e56fca46798afd1'
	m.Name                       = 'Tsumino'
	m.RootURL                    = 'https://www.tsumino.com'
	m.Category                   = 'H-Sites'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetDirectoryPageNumber()
	if HTTP.POST(MODULE.RootURL .. '/Search/Operate/?type=Book', 'PageNumber=1&Text=&Sort=Newest&List=0&Length=0&MinimumRating=0&ExcludeList=0&CompletelyExcludeHated=false') then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).x.XPathString('json(*)("pageCount")')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.POST(MODULE.RootURL .. '/Search/Operate/?type=Book', 'Text=&Sort=Newest&List=0&Length=0&MinimumRating=0&ExcludeList=0&CompletelyExcludeHated=false&PageNumber=' .. (URL + 1)) then
		local v for v in CreateTXQuery(HTTP.Document).XPath('json(*)("data")().entry').Get() do
			NAMES.Add(v.GetProperty('title').ToString())
			LINKS.Add(MODULE.RootURL .. '/Entry/' .. v.GetProperty('id').ToString())
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

		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="book-page-image img-responsive"]/@src'))
		MANGAINFO.Title     = x.XPathString('//div[@class="book-line"][starts-with(.,"Title")]/div[@class="book-data"]')
		MANGAINFO.Artists   = x.XPathString('//div[@class="book-line"][starts-with(.,"Artist")]/div[@class="book-data"]')
		MANGAINFO.Genres    = x.XPathString('string-join(//div[@class="book-line"]//a[@class="book-tag"],", ")')

		if MANGAINFO.Title ~= '' then
			MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
			MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local bookid = URL:match('(%d+)$')
	if bookid == nil then return false end
	if HTTP.GET(MODULE.RootURL .. '/Read/Index/' .. bookid .. '?page=1') then
		local x = CreateTXQuery(HTTP.Document)
		local data_cdn = x.XPathString('//div[@data-cdn]/@data-cdn')
		local data_length = tonumber(x.XPathString('//h1[starts-with(.,"Page 1 of ")]'):match('(%d+)$'))
		if data_cdn and data_length then
			local i; for i=1, data_length do
				TASK.PageLinks.Add(data_cdn:gsub('%[PAGE%]',i))
			end
		end
		return true
	else
		return false
	end
end
