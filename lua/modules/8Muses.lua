function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'd5d532724aba452ea6d4b50663b76326'
	m.Name                       = '8Muses'
	m.RootURL                    = 'https://comics.8muses.com'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnGetImageURL              = 'GetImageURL'
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x = TXQuery.Create(HTTP.Document)

		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//*[@class="gallery"]/a/div/img/@data-src')):gsub('^//', 'https://')
		MANGAINFO.Title     = x.XPathString('//*[@class="top-menu-breadcrumb"]//li[last()]')
		local s = ''
		if MANGAINFO.Title:find('[iI][sS][sS][uU][eE]%s%d') then
			s = MANGAINFO.Title
			MANGAINFO.Title = x.XPathString('//*[@class="top-menu-breadcrumb"]//li[last()-1]')
		end
		-- multi
		local a, n
		if x.XPathString('//*[@class="gallery"]/a') ~= '' then
			for _, v in ipairs(x.XPathI('//*[@class="gallery"]/a')) do
				a = v.GetAttribute('href')
				if a ~= '' then
					n = v.ToString()
					if (MANGAINFO.Title ~= '') and n:find('[iI][sS][sS][uU][eE]') then
						n = MANGAINFO.Title .. '-' .. n
					end
					MANGAINFO.ChapterLinks.Add(a)
					MANGAINFO.ChapterNames.Add(n)
				end
			end
		else
			MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
			if s ~= '' then
				MANGAINFO.ChapterNames.Add(MANGAINFO.Title .. ' - ' .. s)
			else
				MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
			end
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(RemoveURLDelim(MaybeFillHost(MODULE.RootURL, URL))) then
		local x = TXQuery.Create(HTTP.Document)
		local v, s
		for _, v in ipairs(x.XPathI('//*[@class="gallery"]/a/div/img/@data-src')) do
			s = v.ToString()
			if s ~= '' then
				s = MaybeFillHost(MODULE.RootURL, s:gsub('/th/', '/fm/')):gsub('^//', 'https://')
				TASK.PageLinks.Add(s)
			end
		end
		if TASK.PageLinks.Count == 0 then
			for _, v in ipairs(x.XPathI('//*[@class="gallery"]/a/@href')) do
				TASK.PageContainerLinks.Add(v.ToString())
			end
			TASK.PageNumber = TASK.PageContainerLinks.Count
		end
		return true
	else
		return false
	end
end

function GetImageURL()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, TASK.PageContainerLinks[WORKID]):gsub('/*$', '')) then
		TASK.PageLinks[WORKID] = MaybeFillHost(MODULE.RootURL, TXQuery.Create(HTTP.Document).XPathString('string-join((//*[@id="imageDir"]/@value,//*[@id="imageName"]/@value),"")'))
		return true
	end
		return false
end

