function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if not HTTP.GET(MANGAINFO.URL) then return net_problem end
	
	local x = CreateTXQuery(HTTP.Document)
	if x.XPathString('//title'):lower() =='redirect' then
		if HTTP.GET(x.XPathString('//a/@href')) then
			x.ParseHTML(HTTP.Document)
		else return net_problem end
	end
	
	MANGAINFO.Title     = x.XPathString('//div[starts-with(@class,"gallery")]/h1')
	MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="cover"]//img/@src')):gsub('^//','https://')
	MANGAINFO.Authors   = x.XPathStringAll('//div[starts-with(@class,"gallery")]/h2/ul/li/a')
	MANGAINFO.Genres    = x.XPathStringAll('//div[@class="gallery-info"]/table//tr/td//a')
	
	MANGAINFO.ChapterLinks.Add(x.XPathString('//div[contains(@class,"cover-column")]/a/@href'))
	MANGAINFO.ChapterNames.Add(MANGAINFO.Title)
	return no_error
end
----------------------------------------------------------------------------------------------

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		----------------------------------------------------------------------------------------------
		--  direct translaton of https://ltn.hitomi.la/common.js and https://ltn.hitomi.la/reader.js

		local function subdomain_from_galleryid(g, number_of_frontends)
			local o = g % number_of_frontends
			return string.char(97 + o)
		end

		local function subdomain_from_url(url, base)
			local retval = 'b'
			if (base) then
				retval = base
			end

			local number_of_frontends = 3
			local b = 16

			local r = '^.*/%w/(%w%w)/.*$'
			local m = url:gsub(r, '%1')
			if not(m) then
				return 'a'
			end

			local g = tonumber(m, b) or nil
			if g then
				if (g < 0x30) then
								number_of_frontends = 2
				end
				if (g < 0x09) then
								g = 1
				end
				retval = subdomain_from_galleryid(g, number_of_frontends) .. retval
			end
			return retval
		end

		local function url_from_url(url, base)
			return url:gsub('//..?%.hitomi%.la/', '//' .. subdomain_from_url(url, base) .. '.hitomi.la/')
		end

		local function full_path_from_hash(hash)
			if (hash:len() < 3) then
				return hash
			end
			return hash:gsub('^.*(..)(.)$', '%2/%1/' .. hash)
		end

		local function url_from_hash(galleryid, image, dir, ext)
			ext = ext or dir or image.name:match('%.(.+)')
			dir = dir or 'images'
			return 'https://a.hitomi.la/' .. dir .. '/' .. full_path_from_hash(image.hash) .. '.' .. ext
		end

		function url_from_url_from_hash(galleryid, image, dir, ext, base)
			return url_from_url(url_from_hash(galleryid, image, dir, ext), base)
		end

		local function image_url_from_image(galleryid, image, no_webp)
			local webp
			if (image['hash'] and image['haswebp'] and not(no_webp)) then
				webp = 'webp'
				return url_from_url_from_hash(galleryid, image, webp, webp, 'a')
			end
			return url_from_url_from_hash(galleryid, image, webp)
			
		end
		-- end of https://ltn.hitomi.la/common.js

		local x = CreateTXQuery(HTTP.Document)
		local galleryid   = URL:match('/(%d+)%.html')
		local gallery_url = x.XPathString('//script[contains(@src,"reader.js")]/@src'):match('//(.+)/')
		if galleryid and gallery_url and HTTP.GET('https://'..gallery_url..'/galleries/'..galleryid..'.js') then
			local no_webp = not MODULE.GetOption('download_webp')
			local s = HTTP.Document.ToString():match('("files":%[.-%])'):gsub('"files":','')
			if s then
				x.ParseHTML(s)
				local image={}, v for v in x.XPath('json(*)()').Get() do
					image.hash    = x.XPathString('./hash', v)
					image.haswebp = x.XPathString('./haswebp', v) == '1'
					image.name    = x.XPathString('./name', v)
					image.hasavif = x.XPathString('./hasavif', v) == '1'
					TASK.PageLinks.Add(image_url_from_image(galleryid, image, no_webp))
				end
			end
		end
	else
		return false
	end
	return true
end

function BeforeDownloadImage()
	HTTP.Headers.Values['Pragma'] = 'no-cache'
	HTTP.Headers.Values['Cache-Control'] = 'no-cache'
	HTTP.Headers.Values['Referer'] = MaybeFillHost(MODULE.RootURL, URL)
	return true
end

function GetNameAndLink()
	if not HTTP.GET('https://ltn.hitomi.la/index-all.nozomi') then return net_problem end
	local s = HTTP.Document.ToString()
	local prefix = MODULE.RootURL .. '/galleries/'
	-- number in uint32 little-endian
	local n
	for i = 1, s:len(), 4 do
		n = s:byte(i+3) + (s:byte(i+2) << 8) + (s:byte(i+1) << 16) + (s:byte(i) << 24)
		LINKS.Add(prefix..n..'.html')
		NAMES.Add(n)
	end
	return no_error
end

function Init()
	local m = NewWebsiteModule()
	m.ID                    = '1972cec9c85b43f6b10b11920a7aafef'
	m.Name                  = 'HitomiLa'
	m.RootURL               = 'https://hitomi.la'
	m.Category              = 'H-Sites'
	m.OnGetInfo             = 'GetInfo'
	m.OnGetPageNumber       = 'GetPageNumber'
	m.OnGetNameAndLink      = 'GetNameAndLink'
	m.OnBeforeDownloadImage = 'BeforeDownloadImage'

	local fmd = require 'fmd.env'
	local slang = fmd.SelectedLanguage
	local lang = {
		['en'] = {
			['webp'] = 'Download WebP'
		},
		['id_ID'] = {
			['webp'] = 'Unduh WebP'
		},
		get =
			function(self, key)
				local sel = self[slang]
				if sel == nil then sel = self['en'] end
				return sel[key]
			end
	}
	m.AddOptionCheckBox('download_webp', lang:get('webp'), true)
end
