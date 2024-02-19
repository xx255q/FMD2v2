local dirpages = {'webtoon', 'comic'}

function Init()
	local m = NewWebsiteModule()
	m.ID                    = '8b8a11bb9b0e4cd8b30c8577c762d19c'
	m.Name                  = 'NewToki'
	m.RootURL               = 'https://newtoki325.com'
	m.Category              = 'Raw'
	m.OnGetNameAndLink      = 'GetNameAndLink'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'
	m.TotalDirectory        = #dirpages

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['delay'] = 'Delay (s) between requests'
		},
		['id_ID'] = {
			['delay'] = 'Tunda (detik) antara permintaan'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionSpinEdit('newtoki_delay', lang:get('delay'), 2)
end

function GetNameAndLink()
	Delay()
	if HTTP.GET(MODULE.RootURL .. '/' .. dirpages[MODULE.CurrentDirectoryIndex + 1] .. '/p' .. (URL + 1)) then
		local x = CreateTXQuery(HTTP.Document)
		x.XPathHREFAll('//div[@class="in-lable trans-bg-black"]/a', LINKS, NAMES)
		UPDATELIST.CurrentDirectoryPageNumber = tonumber(x.XPathString('//ul[contains(@class, "pagination")]/li[last()]/a/@href'):match('/p(%d+)')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	Delay()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
		if HTTP.GET(MANGAINFO.URL) then
			local x = CreateTXQuery(HTTP.Document)
			MANGAINFO.Title     = x.XPathString('//meta[@name="subject"]/@content')
			MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//meta[@property="og:image"]/@content'))
			MANGAINFO.Authors   = x.XPathString('//meta[@property="og:author"]/@content')
			MANGAINFO.Summary   = x.XPathString('//div[@class="view-title"]//div[@class="col-sm-8"]/div[2]')
			local v for v in x.XPath('//div[@class="wr-subject"]/a').Get() do
				MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'))
				MANGAINFO.ChapterNames.Add(x.XPathString('text()', v))
			end
			MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()

			HTTP.Reset()
			HTTP.Headers.Values['Referer'] = MANGAINFO.URL
			return no_error
		else
			return net_problem
	end
end

function GetPageNumber()
	Delay()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL,URL)) then
		local function HexToStr(str)
			return str:gsub('%x%x',function(c)return c.char(tonumber(c,16))end)
		end
		local x = CreateTXQuery(HTTP.Document)
		local s = x.XPathString('//script[contains(.,"var html_data")]')
		local decoded_hex = HexToStr(s:match("html_data%+%=\'.*\';"):gsub("[html_data+=.;%'\n]", ''))
		x.ParseHTML(decoded_hex)
		x.XPathStringAll('//img[@src="/img/loading-image.gif"]/@*[contains(name(),"data-")]', TASK.PageLinks)
	else
		return false
	end
	return true
end

function Delay()
	local lastDelay = tonumber(MODULE.Storage['lastDelay']) or 1
	local newtoki_delay = tonumber(MODULE.GetOption('newtoki_delay')) or 2 -- * MODULE.ActiveConnectionCount
	if lastDelay ~= '' then
		lastDelay = os.time() - lastDelay
		if lastDelay < newtoki_delay then
			sleep((newtoki_delay - lastDelay) * 1000)
		end
	end
	MODULE.Storage['lastDelay'] = os.time()
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Referer'] = MODULE.RootURL

	return true
end
