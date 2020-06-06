function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)

		MANGAINFO.Title=x.XPathString('//title/text()')
		:gsub('Baca', ''):gsub('Online Komik', ''):gsub('Komik -', ''):gsub('Komik', ''):gsub('KOMIK', '')
		:gsub('komik', ''):gsub('Manga -', ''):gsub('Manga', ''):gsub('Online -', ''):gsub('Terbaru', '')
		:gsub('Bahasa', ''):gsub('Indonesia', ''):gsub('bahasa', ''):gsub('indonesia', '')
		:gsub('BAHASA', ''):gsub('INDONESIA', ''):gsub('terbaru', ''):gsub('^%s*[–/]', '')

		MANGAINFO.CoverLink=x.XPathString('//a[@imageanchor]/img/@src')
		MANGAINFO.Authors=x.XPathString('//*[contains(., ("Author","Penulis"))]/following-sibling::text()'):gsub("^:", "")
	MANGAINFO.Artists=x.XPathString('//*[contains(., ("Artist","Seniman"))]/following-sibling::text()'):gsub("^:", "")
		MANGAINFO.Genres=x.XPathString('//*[contains(., "Genre")]/following-sibling::text()'):gsub("^:", "")
		MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//*[contains(., ("Episodes","Status"))]/following-sibling::text()'))
	MANGAINFO.Summary = x.XPathString('//*[contains(., "Sinopsis")]/following-sibling::text()'):gsub("^:", "")
		x.XPathHREFAll('//div[contains(@style, "-moz-border-radius")]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		x=CreateTXQuery(HTTP.Document)
		x.XPathStringAll('//div[@class="separator"]/a/img/@src', TASK.PageLinks)
		if TASK.PageLinks.Count == 0 then
			x.XPathStringAll('//img[./@*[starts-with(name(), "data-original")]]/@src', TASK.PageLinks)
		end
		if TASK.PageLinks.Count == 0 then
			if HTTP.GET(MaybeFillHost('http://mangaku.co',URL)) then
				x=CreateTXQuery(HTTP.Document)
				x.XPathStringAll('//div[@class="separator"]/a/img/@src', TASK.PageLinks)
				return true
			else
				return false
			end
		else
			return true
		end
	else
		return false
	end
end

function getnameandlink()
	if HTTP.GET(MODULE.RootURL..'/daftar-komik-bahasa-indonesia/') then
		CreateTXQuery(HTTP.Document).XPathHREFAll('//a[@class="screenshot"]',LINKS,NAMES)
		return no_error
	else
		return net_problem
	end
end

function Init()
	local m = NewWebsiteModule()
	m.ID = '7e28dfa03c0e42dbb3608fb7b69e6dfb'
	m.Category='Indonesian'
	m.Name='MangaKu'
	m.RootURL='http://mangaku.in'
	m.LastUpdated='July 10, 2019'
	m.OnGetInfo='getinfo'
	m.OnGetPageNumber='getpagenumber'
	m.OnGetNameAndLink='getnameandlink'
end