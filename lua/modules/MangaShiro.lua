function getinfo()
	MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title     = getTitle(x)
		MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, getCover(x))
		MANGAINFO.Authors   = getAuthors(x)
		MANGAINFO.Artists   = getArtists(x)
		MANGAINFO.Genres    = getGenres(x)
		MANGAINFO.Status    = MangaInfoStatusIfPos(getStatus(x))
		MANGAINFO.Summary   = getSummary(x)
		getMangas(x)
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getTitle(x)
	local title = ''
	if title == '' then title = x.XPathString('//*[@id="judul"]/h1') end
	if title == '' then title = x.XPathString('//*[@id="judul_komik"]/h1') end
	if title == '' then title = x.XPathString('//div[@class="infox"]/h1') end
	if title == '' then title = x.XPathString('//h1[@itemprop="headline"]') end
	if title == '' then title = x.XPathString('//h1[@itemprop="name"]') end
	if title == '' then title = x.XPathString('//div[@class="info1"]/*') end
	if title == '' then title = x.XPathString('//div[@class="mangainfo"]/h1') end
	if title == '' then title = x.XPathString('//h2[@class="entry-title"]') end
	if title == '' then title = x.XPathString('//h1') end
	if title == '' then title = x.XPathString('//h2') end
	title = title:gsub('Bahasa Indonesia$', ''):gsub(' Indonesia|Baca"', ''):gsub('Bahasa Indonesia', ''):gsub('Komik', ''):gsub(' Raw', '')
	title = title:gsub('Manga', ''):gsub('Indonesia', ''):gsub('Baca', ''):gsub('bahasa', ''):gsub('indonesia', ''):gsub('can', ''):gsub('|', '')
	title = title:gsub(string.gsub(MODULE.Name, 'https://', ''), '')
	return title
end

function getCover(x)
	local img = ''
	if img == '' then img = x.XPathString('//div[@class="thumb"]/img/@data-src') end
	if img == '' then img = x.XPathString('//div[@class="thumb"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="imgdesc"]/img/@src') end
	if img == '' then img = x.XPathString('//div[contains(@class,"leftImage")]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="imgseries"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@itemprop="image"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@class="mangainfo"]//div[@class="topinfo"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@id="m-cover"]/img/@src') end
	if img == '' then img = x.XPathString('//div[@itemprop="image"]/img/@data-lazy-src') end
	if img == '' then img = x.XPathString('//div[@class="img"]/img[@itemprop="image"]/@src') end
	if img == '' then img = x.XPathString('//div[@class="ims"]/img/@src') end
	return img
end

function getAuthors(x)
	local authors = ''
	if authors == '' then authors = x.XPathString('//div[@class="spe"]//span[starts-with(.,"المؤلف")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//li[starts-with(.,"Komikus")]/b') end
	if authors == '' then authors = x.XPathString('//div[@class="listinfo"]//li[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//span[@class="details"]//div[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//div[@class="preview"]//li[starts-with(.,"Komikus")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//div[@class="spe"]//span[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//table[@class="attr"]//tr[contains(th, "Author")]/td') end
	if authors == '' then authors = x.XPathString('//table[@class="listinfo"]//tr[contains(th, "Penulis")]/td') end
	if authors == '' then authors = x.XPathString('//*[@class="anf"]//li[starts-with(.,"Author")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//div[@class="listinfo"]//li[starts-with(.,"Pengarang")]/substring-after(.," ")') end
	if authors == '' then authors = x.XPathString('//span[@id="m-author"]') end
	if authors == '' then authors = x.XPathString('//ul[@class="baru"]/li[2][starts-with(.,"Mangaka")]/substring-after(.,":")') end
	if authors == '' then authors = x.XPathString('//table[@class="listinfo"]//tr[contains(th, "Author")]/following-sibling::td') end
	if authors == '' then authors = x.XPathString('//tr[contains(td, "Komikus")]//following-sibling::td') end
	return authors
end

function getArtists(x)
	local artists = ''
	if artists == '' then artists = x.XPathStringAll('//div[@class="spe"]//span[starts-with(.,"Artist")]/substring-after(.,":")') end
	return artists
end

function getGenres(x)
	local genre = ''
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[contains(.,"التصنيفات")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[starts-with(.,"Genres:")]/substring-after(.,":")') end
	if genre == '' then genre = x.XPathStringAll('//div[contains(@class,"animeinfo")]/div[@class="gnr"]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="gnr"]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[contains(@class,"mrgn animeinfo")]/div[@class="gnr"]/a') end
	if genre == '' then genre = x.XPathStringAll('//span[@id="m-genre"]') end
	if genre == '' then genre = x.XPathStringAll('//table[@class="listinfo"]//tr[contains(th, "Genre")]/td/a') end
	if genre == '' then genre = x.XPathStringAll('//table[@class="attr"]//tr[contains(th, "Genres")]/td/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[starts-with(.,"Genre")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="spe"]//span[starts-with(.,"Genres")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="genrex"]/a') end
	if genre == '' then genre = x.XPathStringAll('//ul[@class="genre"]/li') end
	if genre == '' then genre = x.XPathStringAll('//span[@class="details"]//div[starts-with(.,"Genre")]/a') end
	if genre == '' then genre = x.XPathStringAll('//div[@class="listinfo"]//li[starts-with(.,"Genre")]/substring-after(.,":")') end
	return genre
end

function getStatus(x)
	local status = ''
	if status == '' then status = x.XPathString('//div[@class="spe"]//span[starts-with(.,"الحالة")]/substring-after(.,":")') end
	if status == '' then status = x.XPathString('//div[@class="spe"]//span[starts-with(.,"Status:")]/substring-after(.,":")') end
	if status == '' then status = x.XPathString('//div[@class="listinfo"]//li[starts-with(.,"Status")]/substring-after(.," ")') end
	if status == '' then status = x.XPathString('//*[@class="anf"]//li[starts-with(.,"Status")]/substring-after(.,":")') end
	if status == '' then status = x.XPathString('//span[@id="m-status"]') end
	if status == '' then status = x.XPathString('//table[@class="listinfo"]//tr[contains(th, "Status")]/td') end
	if status == '' then status = x.XPathString('//table[@class="attr"]//tr[contains(th, "Status")]/td') end
	if status == '' then status = x.XPathString('//div[@class="preview"]//li[starts-with(.,"Tanggal Rilis")]/substring-after(.,"-")') end
	if status == '' then status = x.XPathString('//span[@class="details"]//div[starts-with(.,"Status")]') end
	if status == '' then status = x.XPathString('//ul[@class="baru"]/li[3]') end
	if status == '' then status = x.XPathString('//tr[contains(td, "Status")]//following-sibling::td') end
	status = status:gsub('Finished', 'Completed')
	return status
end

function getSummary(x)
	local summary = ''
	if summary == '' then summary = x.XPathString('//div[@class="sinopsis"]/p') end
	if summary == '' then summary = x.XPathString('//*[@class="desc"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@class="sinopsis"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@id="m-synopsis"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@class="sin"]/p') end
	if summary == '' then summary = x.XPathString('//*[@class="description"]/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//div[contains(@class,"animeinfo")]/div[@class="rm"]/span/string-join(.//text(),"")') end
	if summary == '' then summary = x.XPathString('//*[@class="jds"]/p') end
	summary = summary:gsub('.fb_iframe_widget_fluid_desktop iframe', ''):gsub('width: 100%% !important;', ''):gsub('{', ''):gsub('}', '')
	return summary
end

function getMangas(x)
	if MODULE.Name == 'Ngomik' then
		local v = x.XPath('//div[contains(@class, "bxcl")]//li//*[contains(@class,"lch")]/a')
		for i = 1, v.Count do
			local v1 = v.Get(i)
			local name = v1.GetAttribute('href')
			MANGAINFO.ChapterNames.Add(name:gsub(MODULE.RootURL..'/',''))
			MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'));
		end
	else
		if MANGAINFO.ChapterLinks.Count < 1 then x.XPathHREFAll('//li//span[@class="leftoff"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count < 1 then x.XPathHREFAll('//div[@class="bxcl"]//li//*[@class="lchx"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count < 1 then x.XPathHREFAll('//div[@class="bxcl"]//li//div[@class="lch"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count < 1 then x.XPathHREFAll('//div[@class="bxcl nobn"]//li//div[@class="lch"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count < 1 then x.XPathHREFAll('//ul[@class="lcp_catlist"]//li/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count < 1 then x.XPathHREFAll('//div[contains(@class, "bxcl")]//li//*[contains(@class,"lchx")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end
		if MANGAINFO.ChapterLinks.Count < 1 then x.XPathHREFAll('//div[contains(@class, "lchx")]//li//*[contains(@class,"bxcl")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames) end

		if MANGAINFO.ChapterLinks.Count < 1 or MODULE.Name == 'Mangacan' then
			local v = x.XPath('//table[@class="updates"]//td/a')
			for i = 1, v.Count do
				local v1 = v.Get(i)
				local s = v1.GetAttribute('href')
				s = string.gsub(s, '-1.htm', '.htm')
				MANGAINFO.ChapterNames.Add(Trim(SeparateLeft(v1.ToString(), '')));
				MANGAINFO.ChapterLinks.Add(s);
			end
		end

		if MANGAINFO.ChapterLinks.Count < 1 or MODULE.Name == 'Komiku' then
			local v = x.XPath('//table[@class="chapter"]//td[1]/a')
			for i = 1, v.Count do
				local v1 = v.Get(i)
				MANGAINFO.ChapterNames.Add(v1.ToString());
				MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'));
			end
		end

		if MANGAINFO.ChapterLinks.Count < 1 or MODULE.Name == 'MangaKita' then
			local v = x.XPath('//div[@class="list chapter-list"]//div/span/a')
			for i = 1, v.Count do
				local v1 = v.Get(i)
				local s = v1.ToString()
				local l = v1.GetAttribute('href')
				local title = l
				if s < 'Download PDF' then
				title = title:gsub('mangakita.net', ''):gsub('https:', '')
				title = title:gsub('/', ''):gsub('-', ' ')
				MANGAINFO.ChapterNames.Add(title);
				MANGAINFO.ChapterLinks.Add(l);
				end
			end
		end
	end
end

function getpagenumber()
	TASK.PageNumber=0
	TASK.PageLinks.Clear()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		if MODULE.Name == 'BacaManga' then
			local crypto = require 'fmd.crypto'
			local x = CreateTXQuery(HTTP.Document)
			local s = x.XPathString('*')
			x.ParseHTML(crypto.DecodeBase64(GetBetween('](atob(', ')),', s)))
			x.XPathStringAll('json(*)()', TASK.PageLinks)
		elseif MODULE.Name == 'MangaShiro' then
			local x = CreateTXQuery(HTTP.Document)
			local v=x.XPath('//*[@id="readerarea"]//a')
				for i=1,v.Count do
						local v1=v.Get(i)
						if string.find(v1.GetAttribute('href'), "shironime.png") == nil then
								TASK.PageLinks.Add(v1.GetAttribute('href'))
						end
				end
		elseif MODULE.Name == 'Kiryuu' then
			local x = CreateTXQuery(HTTP.Document)
			local v=x.XPath('//*[@id="readerarea"]//img')
				for i=1,v.Count do
						local v1=v.Get(i)
						if string.find(v1.GetAttribute('src'), ".filerun.") == nil and
							 string.find(v1.GetAttribute('src'), ",0.jpg") == nil and
							 string.find(v1.GetAttribute('src'), ",5.jpg") == nil and
							 string.find(v1.GetAttribute('src'), ".5.jpg") == nil and
							 string.find(v1.GetAttribute('src'), "00.jpg") == nil and
							 string.find(v1.GetAttribute('src'), "z10.jpg") == nil and
							 string.find(v1.GetAttribute('src'), "Komeng.jpg") == nil and
							 string.find(v1.GetAttribute('src'), "ZZ.jpg") == nil then
								TASK.PageLinks.Add(v1.GetAttribute('src'))
						end
				end
		elseif MODULE.Name == 'MangaSWAT' then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="readerarea"]/p/img/@data-src', TASK.PageLinks)
		else
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="readerarea"]/p/img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="readerarea"]/p//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="readerarea"]/div//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="readerarea"]//a/@href', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="readerarea"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="readerareaimg"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@id="imgholder"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@class="entry-content"]//img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 then CreateTXQuery(HTTP.Document).XPathStringAll('//*[@class="bc"]/img/@src', TASK.PageLinks) end
			if TASK.PageLinks.Count < 1 or MODULE.Name == 'KoMBatch' then
				local link = MaybeFillHost(MODULE.RootURL,URL)
				link = link:gsub('/read', '/api/chapter')
				if HTTP.GET(link) then
					x=CreateTXQuery(HTTP.Document)
					x.ParseHTML(HTTP.Document)
			for _, v in ipairs(x.XPathI('json(*).chapter.images()("text")')) do
						TASK.PageLinks.Add(v.ToString():gsub('^//', 'https://'))
					end
				else
					return false
				end
			end
		end
		return true
	else
		return false
	end
end

function getnameandlink()
	local dirs = {
		['MangaShiro'] = '/manga/?list',
		['KomikStation'] = '/manga/?list',
		['MangaKid'] = '/manga-lists/',
		['KomikCast'] = '/daftar-komik/?list',
		['WestManga'] = '/manga-list/?list',
		['Kiryuu'] = '/manga-lists/?list',
		['Kyuroku'] = '/manga/?list',
		['BacaManga'] = '/manga/?list',
		['PecintaKomik'] = '/daftar-manga/?list',
		['MangaIndoNet'] = '/manga-list/?list',
		['KomikIndo'] = '/manga-list/?list',
		['KomikIndoWebId'] = '/daftar-komik/',
		['Komiku'] = '/daftar-komik/',
		['KazeManga'] = '/manga-list/?list',
		['Mangacan'] =  '/daftar-komik-manga-bahasa-indonesia.html',
		['MangaIndo'] = '/manga-list-201902-v052/',
		['KomikMama'] = '/manga-list/?list',
		['MangaCeng'] = '/manga/?list',
		['MaidMangaID'] = '/manga-list/?list',
		['KomikAV'] = '/manga/?list',
		['Ngomik'] = '/daftar-komik/?list',
		['MangaPus'] = '/manga-list/?list',
		['Mangaseno'] = '/manga-list/?list',
		['SekteKomik'] = '/manga/?list',
		['BaekjinScans'] = '/manga/?list',
		['Mangakyo'] = '/daftar-manga/?list',
		['MataKomik'] = '/manga/?list',
		['Rawkuma'] = '/manga/?list',
		['KomikGoCoID'] = '/manga/?list',
		['MangaSWAT'] = '/manga/?list',
		['MangaTsuki'] = '/manga/?list'
	}
	local dirurl = '/manga-list/'
	if dirs[MODULE.Name] ~= nil then
		dirurl = dirs[MODULE.Name]
	end
	if HTTP.GET(MODULE.RootURL..dirurl) then
		local x=CreateTXQuery(HTTP.Document)
		if LINKS.Count < 1 then x.XPathHREFAll('//*[@class="daftarkomik"]//a',LINKS,NAMES) end
		if LINKS.Count < 1 then x.XPathHREFAll('//*[@class="jdlbar"]//a',LINKS,NAMES) end
		if LINKS.Count < 1 then x.XPathHREFAll('//*[@class="blix"]//a',LINKS,NAMES) end
		if LINKS.Count < 1 then x.XPathHREFAll('//*[@class="soralist"]//a',LINKS,NAMES) end
		if LINKS.Count < 1 then x.XPathHREFAll('//*[@id="a-z"]//h4/a',LINKS,NAMES) end
		if LINKS.Count < 1 then x.XPathHREFAll('//*[@class="manga-list"]/a',LINKS,NAMES) end

		if LINKS.Count < 1 or MODULE.Name == 'KoMBatch' then
			local pages = 1
			local p = 1
			while p <= pages do
				if p > 1 then
					if HTTP.GET(MODULE.RootURL..dirurl..'?page=' .. tostring(p)) then
						x=CreateTXQuery(HTTP.Document)
					else
						break
					end
				end
				if p == pages then
					local pg = x.XPathString('//*[contains(@class, "pagination")]//li[last()-1]/a/substring-after(@href, "?page=")')
					if pg ~= '' then pages = tonumber(pg) end
				end
				local v=x.XPath('//*[contains(@class, "trending")]//*[contains(@class, "box_trending")]')
				for i=1,v.Count do
					local v1=v.Get(i)
					local title = x.XPathString('.//*[contains(@class, "_2dU-m")]/text()', v1)
					local link = x.XPathString('.//*[contains(@class, "_2dU-m")]/@href', v1)
					NAMES.Add(title)
					LINKS.Add(link)
				end
				p = p + 1
			end
		end
		if LINKS.Count < 1 or MODULE.Name == 'Mangacan' then x.XPathHREFAll('//*[@class="blix"]/ul//a',LINKS,NAMES) end
		return no_error
	else
		return net_problem
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['referer'] = MODULE.RootURL
	HTTP.Headers.Values['Accept'] = 'image/webp,image/apng,image/*,*/*'
	return true
end

function Init()
	function AddWebsiteModule(id, site, url, cat)
		local m=NewWebsiteModule()
		m.ID=id
		m.Category=cat
		m.Name=site
		m.RootURL=url
		m.OnGetInfo='getinfo'
		m.OnGetPageNumber='getpagenumber'
		m.OnGetNameAndLink='getnameandlink'
		return m
	end
	local cat = 'Indonesian'
	local m = AddWebsiteModule('5eb57a1843d8462dab0fdfd0efc1eca5', 'MangaShiro', 'https://mangashiro.co', cat)
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
	AddWebsiteModule('b543e37b656e43ffb3faa034eee6c945', 'MangaKita', 'https://mangakita.net', cat)
	AddWebsiteModule('b5586745030a45bba05d0c360caa6d1a', 'KomikStation', 'https://www.komikstation.com', cat)
	AddWebsiteModule('49602ce189e844f49bfe78f7a1484dbe', 'MangaKid', 'https://mgku.me', cat)
	AddWebsiteModule('b8206e754d4541689c1d367f7e19fd64', 'KomikCast', 'https://komikcast.com', cat)
	AddWebsiteModule('35e1b3ff5dbf428889d0f316c3d881e6', 'WestManga', 'https://westmanga.info', cat)
	AddWebsiteModule('031f3cc0ae3346ad9b8c33d5377891e9', 'Kiryuu', 'https://kiryuu.co', cat)
	AddWebsiteModule('965d172c0fbd4ad7b75f39fb5cec26ac', 'Kyuroku', 'https://kyuroku.com', cat)
	AddWebsiteModule('5e66f8a12f114ba3a8408eb1d7044d76', 'BacaManga', 'https://bacamanga.co', cat)
	AddWebsiteModule('ee7abb21767d48d5b4b343ce701ae6e6', 'PecintaKomik', 'https://www.pecintakomik.net', cat)
	AddWebsiteModule('63be65ab7f004093ac26fdeb30b466e4', 'MangaIndoNet', 'https://mangaindo.net', cat)
	AddWebsiteModule('009bf49bc17a4a2a8e1c79cce6867651', 'KomikIndo', 'https://komikindo.co', cat)
	AddWebsiteModule('2cf30e2a7f3d4b4a9b2d29c3fb04e23f', 'KomikIndoWebId', 'https://www.komikindo.web.ID', cat)
	AddWebsiteModule('5af0f26f0d034fb2b42ee65d7e4188ab', 'Komiku', 'https://komiku.co.ID', cat)
	AddWebsiteModule('4ccdf84e05474a66adc14ea8a2edfd15', 'KazeManga', 'https://kazemanga.web.ID', cat)
	AddWebsiteModule('ca571825056b4850bd3693e4e1437997', 'Mangacan', 'http://www.mangacanblog.com', cat)
	AddWebsiteModule('fb5bd3aa549f4aefa112a8fe7547d2a9', 'MangaIndo', 'https://mangaindo.web.ID', cat)
	AddWebsiteModule('6f8182f08d5444dbb5244ec882430db1', 'KomikMama', 'https://komikmama.net', cat)
	AddWebsiteModule('2929eb02fcad4156a6c61576c1dc4b53', 'MangaCeng', 'https://mangaceng.com', cat)
	AddWebsiteModule('7a74b2abda1d4b329ee1d1fa58866c03', 'MaidMangaID', 'https://www.maid.my.ID', cat)
	AddWebsiteModule('a70859360a2a474ba2abdb86bc48616c', 'KomikAV', 'https://komikav.com', cat)
	AddWebsiteModule('180a930232614f81816720cefeea7954', 'KoMBatch', 'https://kombatch.com', cat)
	AddWebsiteModule('5c06401129894099bb6fc59c08a878d4', 'Ngomik', 'https://ngomik.in', cat)
	AddWebsiteModule('c16adc6202924e558b977f74c7301bed', 'MangaPus', 'https://mangapus.com', cat)
	AddWebsiteModule('0a6dd9c339c94a339dbc89c781b20d20', 'Mangaseno', 'https://mangaseno.com', cat)
	AddWebsiteModule('56f905ea80e24c4f8bbc37e05de2ad9a', 'Mangakyo', 'https://www.mangakyo.com', cat)
	AddWebsiteModule('76e6db9fe2cf4dd49589cfa9b1174684', 'MataKomik', 'https://matakomik.com', cat)
	AddWebsiteModule('cab72ea1fa4947d29e50ec8751d06c7d', 'KomikGoCoID', 'https://www.komikgo.co.ID', cat)
	AddWebsiteModule('d2ffd187eadd4c39819428a160d752cf', 'MangaTsuki', 'https://mangatsuki.web.ID', cat)

	cat = 'Webcomics'
	AddWebsiteModule('46dcfabe757140e7980ec34c65bdb30f', 'SekteKomik', 'http://sektekomik.com', cat)
	AddWebsiteModule('e34c929129c74d8aaf8383da9f6ab378', 'BaekjinScans', 'https://baekjinscans.xyz', cat)

	cat = 'Raw'
	AddWebsiteModule('21b0dfcb262d4ae28520679165282666', 'Rawkuma', 'https://rawkuma.com', cat)

	cat = 'Arabic'
	AddWebsiteModule('0e45db2650604f74a0caeb7c1d69a749', 'MangaSWAT', 'https://mangaswat.com', cat)
end
