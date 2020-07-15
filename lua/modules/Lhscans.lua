function Init()
	function AddWebsiteModule(id, name, url, cat)
		local m = NewWebsiteModule()
		m.ID                         = id
		m.Category                   = cat
		m.Name                       = name
		m.RootURL                    = url
		m.TotalDirectory             = 1
		m.OnGetInfo                  = 'GetInfo'
		m.OnGetPageNumber            = 'GetPageNumber'
		m.OnGetNameAndLink           = 'GetNameAndLink'
		m.OnBeforeDownloadImage      = 'BeforeDownloadImage'
	end
	local cat = 'Raw'
	AddWebsiteModule('9e96846a035646988e1b2eb0f356d795', 'LoveHeaven', 'https://loveheaven.net', cat)
	AddWebsiteModule('4c089029492f43c98d9f27a23403247b', 'HanaScan', 'https://hanascan.com', cat)
	AddWebsiteModule('010777f53bf2414fad039b9567c8a9ce', 'MangaHato', 'https://mangahato.com', cat)
	AddWebsiteModule('794187d0e92e4933bf63812438d69017', 'Manhwa18', 'https://manhwa18.com', cat)

	cat = 'English'
	AddWebsiteModule('80427d9a7b354f04a8f432b345f0f640', 'MangaWeek', 'https://mangaweek.com', cat)
	AddWebsiteModule('570e716a029e45cabccc2b660ed81425', 'ManhwaScan', 'https://manhwascan.com', cat)
	AddWebsiteModule('694ff34a6ae4469fbdaecf8d3aebb6eb', 'ManhuaScan', 'https://manhuascan.com', cat)
	AddWebsiteModule('3b7ab0c7342f4783910f7842ea05630b', 'EcchiScan', 'https://ecchiscan.com', cat)
	
	cat = 'English-Scanlation'
	AddWebsiteModule('7fb5fbed6d3a44fe923ecc7bf929e6cb', 'LHTranslation', 'https://lhtranslation.net', cat)
end

function GetInfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		local x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = Trim(SeparateLeft(x.XPathString('//div[@class="container"]//li[3]//span'), '- Raw'))
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="thumbnail"]/@src'))
		MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//ul[@class="manga-info"]/li[contains(., "Status")]//a'))
		MANGAINFO.Authors   = x.XPathString('//ul[@class="manga-info"]/li[contains(., "Author")]//a')
		MANGAINFO.Genres    = x.XPathStringAll('//ul[@class="manga-info"]/li[contains(., "Genre")]//a')
		MANGAINFO.Summary   = x.XPathString('string-join(//div[./h3="Description"]/p, "\r\n")')
		if MANGAINFO.Summary == '' then
			MANGAINFO.Summary = x.XPathString('//div[@class="detail"]/div[@class="content"]')
		end
		x.XPathHREFAll('//div[@id="tab-chapper"]//table/tbody/tr/td/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		if MANGAINFO.ChapterLinks.Count == 0 then
			x.XPathHREFAll('//div[@id="list-chapters"]//a[@class="chapter"]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		end
		for i = 0, MANGAINFO.ChapterLinks.Count-1 do
			MANGAINFO.ChapterLinks[i] = MODULE.RootURL .. '/' .. MANGAINFO.ChapterLinks[i]
		end
		MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	TASK.PageLinks.Clear()
	local u = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(u) then
		local x=CreateTXQuery(HTTP.Document)
		if MODULE.ID == '794187d0e92e4933bf63812438d69017' then -- manhwa18
			local v = x.XPath('//img[contains(@class, "chapter-img")]/@src')
			for i = 1, v.Count do
				local s = v.Get(i).ToString()
				s = s:gsub('app/', 'https://manhwa18.com/app/'):gsub('https://manhwa18.net/https://manhwa18.com', 'https://manhwa18.net')
				if string.find(s, ".iff") == nil then
					TASK.PageLinks.Add(s)
				end
			end
		elseif MODULE.ID == '9e96846a035646988e1b2eb0f356d795' then -- loveheaven
			local crypto = require 'fmd.crypto'
			local v = x.XPath('//img[contains(@class, "chapter-img")]/@data-src')
			for i = 1, v.Count do
				local s = v.Get(i).ToString()
				if string.find(s, "https") == nil then
					TASK.PageLinks.Add(crypto.DecodeBase64(s))
				end
			end
		else
			x.XPathStringAll('//img[contains(@class, "chapter-img")]/@src', TASK.PageLinks)
		end
	else
		return false
	end
	return true
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/manga-list.html?listType=allABC') then
		local x = CreateTXQuery(HTTP.Document)
		if MODULE.ID == '694ff34a6ae4469fbdaecf8d3aebb6eb' then -- manhuascan
			x.XPathHREFAll('//div[@id="Character"]//a', LINKS, NAMES)
		else
			local v; for v in x.XPath('//span[@manga-slug]//a)').Get() do
				NAMES.Add(Trim(SeparateLeft(v.ToString(), '- Raw')))
				LINKS.Add(v.GetAttribute('href'))
			end
		end
		return no_error
	else
		return net_problem
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = ' ' .. MaybeFillHost(MODULE.RootURL, TASK.ChapterLinks[TASK.CurrentDownloadChapterPtr])
	return true
end
