function Init()
	AddWebsiteModule('f7ab487b6d29468e8280e8a3cbebbeb4', 'E-Hentai', 'https://e-hentai.org')
	local m = AddWebsiteModule('17e196ebe84c4042a6fbf4594d25d9e0', 'ExHentai', 'https://exhentai.org')
	m.AccountSupport = true
	m.OnLogin        = 'ExHentaiLogin'
end

function AddWebsiteModule(id, name, url)
	local m = NewWebsiteModule()
	m.ID                         = id
	m.Name                       = name
	m.RootURL                    = url
	m.Category                   = 'H-Sites'
	m.OnGetDirectoryPageNumber   = 'GetDirectoryPageNumber'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
	m.OnDownloadImage            = 'DownloadImage'
	m.SortedList                 = true
	m.DynamicPageLink            = true
	m.MaxTaskLimit               = 1
	m.MaxConnectionLimit         = 2
	m.AddOptionComboBox('imagesize', 'Image size:', 'Auto\n780x\n980x\n1280x\n1600x\n2400x\nOriginal', 0)
	return m
end

local dirURL           = 'f_doujinshi=on&f_manga=on&f_western=on&f_apply=Apply+Filter';
local exhentaiurllogin = 'https://forums.e-hentai.org/index.php?act=Login&CODE=01';
local imagesize        = {'xr_a', 'xr_780', 'xr_980', 'xr_1280', 'xr_1600', 'xr_2400', '_'}

function ExHentaiLogin()
	if MODULE.Account.Enabled == false then return false end
	local s = 'returntype=8&CookieDate=1&b=d&bt=pone' ..
		'&UserName=' .. EncodeURLElement(MODULE.Account.Username) ..
		'&PassWord=' .. EncodeURLElement(MODULE.Account.Password) ..
		'&ipb_login_submit=Login%21'
	MODULE.Account.Status = asChecking
	if HTTP.POST(exhentaiurllogin, s) then
		if (HTTP.ResultCode == 200) and (HTTP.Cookies.Values['ipb_pass_hash'] ~= '') then
			MODULE.Account.Status = asValid
			return true
		else
			MODULE.Account.Status = asInvalid
			return false
		end
	else
		MODULE.Account.Status = asUnknown
		return false
	end
end

function GetDirectoryPageNumber()
	if HTTP.GET(MODULE.RootURL .. '/?' .. dirURL) then
		PAGENUMBER = tonumber(TXQuery.Create(HTTP.Document).x.XPathString('css("table.ptt>tbody>tr>td:nth-last-child(2)>a")')) or 1
		return no_error
	else
		return net_problem
	end
end

function GetNameAndLink()
	local rurl
	if URL == '0' then
		rurl = MODULE.RootURL .. '/?' .. dirURL
	else
		rurl = MODULE.RootURL .. '/?page=' .. IncStr(URL) .. '&' .. dirURL
	end
	if HTTP.GET(rurl) then
		TXQuery.Create(HTTP.Document).XPathHREFAll('//table[@class="itg"]/tbody/tr/td/div/div/a', LINKS, NAMES)
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	URL = URL:gsub('/%?%w.*$', '/'):gsub('/*$', '') .. '/'
	HTTP.Cookies.Values['nw'] = ' 1'
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		-- if there's only 1 line, it's a banned message
		-- todo: identify the banned message
		local x = TXQuery.Create(HTTP.Document)

		MANGAINFO.CoverLink = x.XPathString('//*[@id="gd1"]/img/@src')
		MANGAINFO.Title     = x.XPathString('//*[@id="gn"]')
		MANGAINFO.Artists   = x.XPathStringAll('//a[starts-with(@id,"ta_artist")]')
		MANGAINFO.Genres    = x.XPathStringAll('//a[starts-with(@id,"ta_")and(not(starts-with(@id,"ta_artist")))]')
		MANGAINFO.Status    = MangaInfoStatusIfPos(MANGAINFO.Title, 'wip|ongoing', 'complete')

		if MANGAINFO.Title ~= '' then
			MANGAINFO.ChapterLinks.Add(MANGAINFO.URL)
			MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
		end
		return no_error
	else
		return net_problem
	end
end

function GetImageLink()
	x.ParseHTML(HTTP.Document)
	local v for _, v in ipairs(x.XPathI('//div[@id="gdt"]//a')) do
		TASK.PageContainerLinks.Add(v.GetAttribute('href'))
		TASK.FileNames.Add(ExtractFileNameOnly(SeparateRight(x.XPathString('img/@title', v), ':')))
	end
	while TASK.PageLinks.Count < TASK.PageContainerLinks.Count do
		TASK.PageLinks.Add('G')
	end
end

function GetPageNumber()
	URL = URL:gsub('/%?%w.*$', '/'):gsub('/*$', '') .. '/'
	HTTP.Cookies.Values['nw'] = ' 1'
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		x = TXQuery.Create()
		GetImageLink()
		local p, i
		p = tonumber(x.XPathString('//table[@class="ptt"]//td[last()-1]')) or 1
		if p > 1 then
			for i = 1, p - 1 do
				if HTTP.Terminated then break end
				if HTTP.GET(URL .. '?p=' .. tostring(i)) then
					GetImageLink()
				end
			end
		end
		-- check the max length of filenames, serialize the filenames if it's exceds available space
		SerializeAndMaintainNames(TASK.FileNames)
		local p, i, j = 0
		for i = 0, TASK.FileNames.Count - 1 do
			j = TASK.FileNames[i].len()
			if j > p then
				p = j
			end
		end
		if p > TASK.CurrentMaxFileNameLength then
			TASK.FileNames.Clear()
		end
		return true
	else
		return false
	end
end

function find_var_value(str, var)
	if str:find('var%s*' .. var) then
		return str:match('var%s*' .. var .. '%s*=%s*(.-);'):gsub('^[\'\"]*',''):gsub('[\'\"]*$','')
	else
		return ''
	end
end

function DownloadImage()
	local reconnect = HTTP.RetryCount
	local sel_imagesize = MODULE.GetOption('imagesize') or 0
	if sel_imagesize < #imagesize then
		HTTP.Cookies.Values['uconfig'] = ' ' .. imagesize[sel_imagesize]
	end
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		local result = false
		local x = TXQuery.Create(HTTP.Document)
		local rcount, nls, iurl = 0, '', ''
		while (not result) and (not HTTP.Terminated) do
			x.ParseHTML(HTTP.Document)
			iurl = ''
			if sel_imagesize == #imagesize then iurl = x.XPathString('//a/@href[contains(.,"/fullimg.php")]') end
			if iurl == '' then iurl = x.XPathString('//*[@id="img"]/@src') end
			if iurl == '' then iurl = x.XPathString('//a/img/@src[not(contains(.,"ehgt.org/"))]') end
			if iurl ~= '' then result = HTTP.GET(iurl) end
			if result then break end
			if rcount >= reconnect then break end
			if not result then
				local s, base_url, startkey, gid, startpage, nl = HTTP.Document.ToString(), '', '', '', '', ''
				-- get url param
				base_url  = find_var_value(s, 'base_url')
				startkey  = find_var_value(s, 'startkey')
				gid       = find_var_value(s, 'gid')
				startpage = find_var_value(s, 'startpage')
				if s:find('return%s*nl%(') then
					nl = s:match('return%s%([\'\"]*(.-)[\'\"]*%)')
				end
				s = ''
				if (base_url ~= '') and (startkey ~= '') and (gid ~= '') and (startpage ~= '') then
					iurl = RemoveURLDelim(base_url) .. '/s/' .. startkey .. '/' .. gid .. '-' .. startpage
				else
					iurl = MaybeFillHost(MODULE.RootURL, URL)
				end
				if nl ~= '' then
					if nls == '' then
						nls = '?nl=' .. nl
					else
						nls = nls .. '&nl=' ..nl
					end
				end
				if not HTTP.GET(iurl) then break end
				rcount = rcount + 1
			end
		end
		return result
	end
		return false
end
