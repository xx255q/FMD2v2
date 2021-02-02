local dirpages = {'webtoon', 'comic'}

function Init()
	local m = NewWebsiteModule()
	m.ID                   = '8b8a11bb9b0e4cd8b30c8577c762d19c'
	m.Name                 = 'NewToki'
	m.RootURL              = 'https://newtoki95.com'
	m.Category             = 'Raw'
	m.OnGetNameAndLink     = 'GetNameAndLink'
	m.OnGetInfo            = 'GetInfo'
	m.OnGetPageNumber      = 'GetPageNumber'
	m.TotalDirectory       = #dirpages

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
	local dirurl = MODULE.RootURL .. '/' .. dirpages[MODULE.CurrentDirectoryIndex + 1]
	if not HTTP.GET(dirurl) then return net_problem end
	local x = CreateTXQuery(HTTP.Document)
	local next_url
	while true do
		x.XPathHREFAll('//div[@class="in-lable trans-bg-black"]/a', LINKS, NAMES)
		next_url = x.XPathString('//ul[contains(@class, "pagination")]/li[@class="active"]/following-sibling::li/a/@href')
		if HTTP.Terminated then break end
		if next_url == '' then break end
		UPDATELIST.UpdateStatusText('Loading page ' .. (next_url:match('m/(.-)/p') or '') .. ' ' .. (next_url:match('p(%d+)') or ''))
		Delay()
		if HTTP.GET(next_url) then
			x.ParseHTML(HTTP.Document)
		else
			break
		end
	end
	return no_error
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
			x.XPathHREFAll('//div[@class="wr-subject"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
			MANGAINFO.ChapterLinks.Reverse(); MANGAINFO.ChapterNames.Reverse()
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