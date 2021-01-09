function Init()
	local m = NewWebsiteModule()
	m.ID                       = '5eda5ccf87f1488f9dfa7a9a18f2bcf1'
	m.Category                 = 'H-Sites'
	m.Name                     = 'HentaiNexus'
	m.RootURL                  = 'https://hentainexus.com'
	m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink         = 'GetNameAndLink'
	m.OnGetInfo                = 'GetInfo'
	m.OnGetPageNumber          = 'GetPageNumber'
	m.SortedList               = true
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL) then
		PAGENUMBER = tonumber(CreateTXQuery(HTTP.Document).XPathString('(//ul[@class="pagination-list"])[1]/li[last()]/a/text()'))
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	if HTTP.GET(MODULE.RootURL .. '/page/' .. (URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		local v for v in x.XPath('//div[@class="container"]/div/div/a[contains(@href, "/view/")]').Get() do
			LINKS.Add(v.GetAttribute('href'))
			NAMES.Add(x.XPathString('div/header/@title', v))
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
		MANGAINFO.CoverLink = x.XPathString('//figure[@class="image"][1]/img/@src')
		MANGAINFO.Artists   = x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=artist:")]/text()')
		MANGAINFO.Title     = '[' .. MANGAINFO.Artists .. '] ' .. x.XPathString('//h1[@class="title"]/text()') .. ' (' .. x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=publisher:")]/text()') .. ')'
		MANGAINFO.Genres    = x.XPathStringAll('//table[@class="view-page-details"]//a[contains(@href, "=tag:")]/text()')
		MANGAINFO.Authors   = x.XPathString('//table[@class="view-page-details"]//a[contains(@href, "=publisher:")]/text()')
		MANGAINFO.Summary   = x.XPathString('//table[@class="view-page-details"]//td[contains(., "Description")]/following-sibling::td/text()')
		MANGAINFO.ChapterLinks.Add(URL)
		MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	local first_image = string.gsub(URL, "view", "read") -- fetch 1st image to get encrypted message

	if not HTTP.GET(MaybeFillHost(MODULE.RootURL, first_image)) then return net_problem end

	local x = CreateTXQuery(HTTP.Document)
	local message = GetBetween('initReader("', '", ', x.XPathString('//script[contains(., "initReader")]'))
	local decoded = DecryptMessage(message)
	x.ParseHTML(decoded)
	x.XPathStringAll('json(*).pages()', TASK.PageLinks)
	return no_error
end

function DecryptMessage(str)
	local crypto = require 'fmd.crypto'
	message = crypto.DecodeBase64(str)
	prime_array = {2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53}
	local prime_picker = 0
	for i = 1, 64 do
		prime_picker = prime_picker ~ string.byte(message, i)
		for j = 1, 8 do
            -- no need for zero shift bit correction since message byte will always be positive
			if(prime_picker & 1 == 1) then
				prime_picker = prime_picker >> 1 ~ 12
			else
				prime_picker = prime_picker >> 1
			end
		end
	end
	prime_picker = prime_picker & 7
	array = {}
	o2 = 0
	o3 = 0
	o5 = 0
	
	for i = 0, 255 do array[i] = i end
	for i = 0, 255 do 
			o2 = (o2 + array[i] + string.byte(message, (i % 64)+1)) % 256
			o4 = array[i]
			array[i] = array[o2]
			array[o2] = o4
	end
	
	prime = prime_array[prime_picker+1]
	o1 = 0
	o2 = 0
	parsed = ''
	i = 0
	while i + 64 < string.len(message) do
		o1 = (o1 + prime) % 256
		o2 = (o3 + array[(o2 + array[o1]) % 256]) % 256
		o3 = (o3 + o1 + array[o1]) % 256
		o4 = array[o1]
		array[o1] = array[o2]
		array[o2] = o4
		o5 = array[(o2 + array[(o1 + array[(o5 + o3) % 256]) % 256]) % 256]
		parsed = parsed .. string.char(string.byte(message, (i + 64)+1) ~ o5)
		i = i + 1
	end
	return parsed
end
