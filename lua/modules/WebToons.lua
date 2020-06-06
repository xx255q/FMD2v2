local langs = {
	["en"] = "English",
	["id"] = "Indonesian",
	["zh-hant"] = "Chinese",
	["th"] = "Thai"
}

function getinfo()
	MANGAINFO.URL = MANGAINFO.URL:gsub('(.*)&page=.*', '%1')
	HTTP.Cookies.Values['ageGatePass'] = 'True'
	if HTTP.GET(MANGAINFO.URL) then
		x=CreateTXQuery(HTTP.Document)
		MANGAINFO.Title = x.XPathString('//meta[@property="og:title"]/@content')
		MANGAINFO.CoverLink=x.XPathString('//meta[@name="twitter:image"]/@content')
		MANGAINFO.Authors=x.XPathString('//div[@class="info"]//span[@class="author"]')
		MANGAINFO.Genres=x.XPathString('//div[@class="info"]/h2')
		if MANGAINFO.Genres == '' then
			MANGAINFO.Genres=x.XPathString('//div[@class="info challenge"]/p')
		end
		MANGAINFO.Summary=x.XPathString('//p[@class="summary"]')
		local pages = 1
		local p = 1
		while p <= pages do
			if p > 1 then
				if HTTP.GET(MANGAINFO.URL .. '&page=' .. tostring(p)) then
					x=CreateTXQuery(HTTP.Document)
				else
					break
				end
			end
			if p == pages then
				local pg = x.XPathString('//div[@class="detail_lst"]/div[@class="paginate"]/a[last()]/substring-after(@href, "&page=")')
				if pg ~= '' then pages = tonumber(pg) end
			end
			local v=x.XPath('//div[@class="detail_lst"]/ul/li/a')
			for i=1,v.Count do
				local v1=v.Get(i)
				MANGAINFO.ChapterLinks.Add(v1.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('.//span[@class="subj"]/span', v1))
			end
			p = p + 1
		end
		InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
		return no_error
	else
		return net_problem
	end
end

function getpagenumber()
	TASK.PageNumber=0
	TASK.PageLinks.Clear()
	URL = URL:gsub('(.*)&page=.*', '%1')
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@id="_imageList"]/img[@class="_images"]/@data-URL', TASK.PageLinks)
		return true
	else
		return false
	end
end

function getnameandlink()
	local selectedLang = MODULE.GetOption('lualang')
	local l = langs
	if selectedLang > 0 then
		l = {[findlang(selectedLang)] = ""}
	end
	for key, value in pairs(l) do
		local dirurl = key..'/genre'
		if HTTP.GET(MODULE.RootURL..dirurl) then
			local x=CreateTXQuery(HTTP.Document)
			local v = x.XPath('//div[@class="card_wrap genre"]/ul/li/a')
			for i = 1, v.Count do
				local v1 = v.Get(i)
				NAMES.Add(x.XPathString('.//div[@class="Info"]//p[@class="subj"]', v1)..' ['..key..']');
				LINKS.Add(v1.GetAttribute('href'));
			end
			return no_error
		else
			return net_problem
		end
	end
	if MODULE.GetOption('luaincludechallengetitles') then
		getnameandlinkforchallenge()
	end
end

function getnameandlinkforchallenge()
	local selectedLang = MODULE.GetOption('lualang')
	local l = langs
	if selectedLang > 0 then
		l = {[findlang(selectedLang)] = ""}
	end
	for key, value in pairs(l) do
		local dirurl = key..'/challenge/list?genreTab=ALL&sortOrder=UPDATE'
		if HTTP.GET(MODULE.RootURL..dirurl) then
			local x=CreateTXQuery(HTTP.Document)

			local pages = 1
			local p = 1
			while p <= pages do
				if p > 1 then
					if HTTP.GET(MODULE.RootURL..dirurl..'&page='..tostring(p)) then
						x=CreateTXQuery(HTTP.Document)
					else
						break
					end
				end
				if p == pages then
					local pg = x.XPathString('//div[@class="paginate"]/a[last()]/substring-after(@href, "&page=")')
					if pg ~= '' then pages = tonumber(pg) end
				end
				local v = x.XPath('//div[@class="challenge_cont_area"]/div[contains(@class,"challenge_lst")]/ul/li/a[contains(@class,"challenge_item")]')
				for i = 1, v.Count do
					local v1 = v.Get(i)
					NAMES.Add(x.XPathString('./p[@class="subj"]', v1)..' ['..key..']');
					LINKS.Add(v1.GetAttribute('href'));
				end
				p = p + 1
			end

			return no_error
		else
			return net_problem
		end
	end
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = URL
	return true
end

function getlang(lang)
	if langs[lang] ~= nil then
		return langs[lang]
	else
		return 'Unknown'
	end
end

function getlanglist()
	local t = {}
	for k, v in pairs(langs) do table.insert(t, v); end
	table.sort(t)
	return t
end

function findlang(lang)
	local t = getlanglist()
	for i, v in ipairs(t) do
		if i == lang then
			lang = v
			break
		end
	end
	for k, v in pairs(langs) do
		if v == lang then return k; end
	end
	return nil
end

function Init()
	local m = NewWebsiteModule()
	m.ID                    = '18f636ec7fdf47fabe95d940ad0b548f'
	m.Category              ='English'
	m.Name                  ='WebToons'
	m.RootURL               ='https://www.webtoons.com/'
	m.LastUpdated           ='April 14, 2019'
	m.OnGetInfo             ='getinfo'
	m.OnGetPageNumber       ='getpagenumber'
	m.OnGetNameAndLink      ='getnameandlink'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'

	m.AddOptionCheckBox('luaincludechallengetitles', 'Include manga titles from WebToons Challenge (takes very very long to create manga list!):', false)

	local items = 'All'
	local t = getlanglist()
	for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
	m.AddOptionComboBox('lualang', 'Language:', items, 2)
end
