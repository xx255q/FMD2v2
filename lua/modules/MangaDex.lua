function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
  HTTP.Cookies.Values['mangadex_h_toggle'] = '1'
  local id = URL:match('title/(%d+)')
  if id == nil then id = URL:match('manga/(%d+)'); end
  delay()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL, '/api/manga/' .. id)) then
    local resp = HTMLEncode(StreamToString(HTTP.Document))
    local x = TXQuery.Create(resp)
    
    local info = x.XPath('json(*)')
    if MANGAINFO.Title == '' then
      MANGAINFO.Title = x.XPathString('manga/title', info)
    end
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('manga/cover_url', info))
    MANGAINFO.Authors = x.XPathString('manga/author', info)
    MANGAINFO.Artists = x.XPathString('manga/artist', info)
    MANGAINFO.Summary = x.XPathString('manga/description', info)
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('manga/status', info), '1', '2')
    
    local genres = ''
    local v = x.XPath('jn:members(manga/genres)', info)
    if v.Count > 0 then genres = getgenre(v.Get(1).ToString()); end
    for i = 2, v.Count do
      local v1 = v.Get(i)
      genres = genres .. ', ' .. getgenre(v1.ToString())
    end
    if x.XPathString('manga/hentai', info) == '1' then
      if genres ~= '' then genres = genres .. ', ' end
      genres = genres .. 'Hentai'
    end
    MANGAINFO.Genres = genres
    
    local selLang = MODULE.GetOption('lualang')
    local selLangId = findlang(selLang)
    local chapters = x.XPath('let $c := json(*).chapter return for $k in jn:keys($c) ' ..
      'return jn:object(object(("chapter_id", $k)), $c($k))')
    for i = 1, chapters.Count do
      local v1 = chapters.Get(i)
      
      if not IgnoreChaptersByGroupId(x.XPathString('group_id', v1)) then
        
        local lang = x.XPathString('lang_code', v1)
        local ts = tonumber(x.XPathString('timestamp', v1))
        if (selLang == 0 or lang == selLangId) and (ts <= os.time()) then
          MANGAINFO.ChapterLinks.Add('/chapter/' .. x.XPathString('chapter_id', v1))
          
          local s = ''
          local vol = x.XPathString('volume', v1)
          local ch = x.XPathString('chapter', v1)
          if vol ~= '' then s = s .. string.format('Vol. %s', vol); end
          if s ~= '' then s = s .. ' '; end
          if ch ~= '' then s = s .. string.format('Ch. %s', ch); end
          
          local title = x.XPathString('title', v1)
          if title ~= '' then
            if s ~= '' then s = s .. ' - '; end
            s = s .. title
          end
          
          if selLang == 0 then
            s = string.format('%s [%s]', s, getlang(lang))
          end
          
          if MODULE.GetOption('luashowscangroup') then
            local group = x.XPathString('group_name', v1)
            local group2 = x.XPathString('group_name_2', v1)
            local group3 = x.XPathString('group_name_3', v1)
            if group2:len() > 0 and group2 ~= 'null' then
              group = group .. ' | ' .. group2
            end
            if group3:len() > 0 and group3 ~= 'null' then
              group = group .. ' | ' .. group3
            end
            s = string.format('%s [%s]', s, group)
          end
          
          MANGAINFO.ChapterNames.Add(s)
        end
      end
    end
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function IgnoreChaptersByGroupId(id)
  local groups = {
    ["9097"] = "MangaPlus"
  }
  
  if groups[id] ~= nil then
    return true
  else
    return false
  end
end

function getgenre(genre)
  local genres = {
    ["1"] = "4-koma",
    ["2"] = "Action",
    ["3"] = "Adventure",
    ["4"] = "Award Winning",
    ["5"] = "Comedy",
    ["6"] = "Cooking",
    ["7"] = "Doujinshi",
    ["8"] = "Drama",
    ["9"] = "Ecchi",
    ["10"] = "Fantasy",
    ["11"] = "Gyaru",
    ["12"] = "Harem",
    ["13"] = "Historical",
    ["14"] = "Horror",
    ["15"] = "Josei",
    ["16"] = "Martial Arts",
    ["17"] = "Mecha",
    ["18"] = "Medical",
    ["19"] = "Music",
    ["20"] = "Mystery",
    ["21"] = "Oneshot",
    ["22"] = "Psychological",
    ["23"] = "Romance",
    ["24"] = "School Life",
    ["25"] = "Sci-Fi",
    ["26"] = "Seinen",
    ["27"] = "Shoujo",
    ["28"] = "Shoujo Ai",
    ["29"] = "Shounen",
    ["30"] = "Shounen Ai",
    ["31"] = "Slice of Life",
    ["32"] = "Smut",
    ["33"] = "Sports",
    ["34"] = "Supernatural",
    ["35"] = "Tragedy",
    ["36"] = "Long Strip",
    ["37"] = "Yaoi",
    ["38"] = "Yuri",
    ["39"] = "[no chapters]",
    ["40"] = "Video Games",
    ["41"] = "Isekai",
    ["42"] = "Adaptation",
    ["43"] = "Anthology",
    ["44"] = "Web Comic",
    ["45"] = "Full Color",
    ["46"] = "User Created",
    ["47"] = "Official Colored",
    ["48"] = "Fan Colored",
    ["49"] = "Gore",
    ["50"] = "Sexual Violence",
    ["51"] = "Crime",
    ["52"] = "Magical Girls",
    ["53"] = "Philosophical",
    ["54"] = "Superhero",
    ["55"] = "Thriller",
    ["56"] = "Wuxia",
    ["57"] = "Aliens",
    ["58"] = "Animals",
    ["59"] = "Crossdressing",
    ["60"] = "Demons",
    ["61"] = "Delinquents",
    ["62"] = "Genderswap",
    ["63"] = "Ghosts",
    ["64"] = "Monster Girls",
    ["65"] = "Loli",
    ["66"] = "Magic",
    ["67"] = "Military",
    ["68"] = "Monsters",
    ["69"] = "Ninja",
    ["70"] = "Office Workers",
    ["71"] = "Police",
    ["72"] = "Post-Apocalyptic",
    ["73"] = "Reincarnation",
    ["74"] = "Reverse Harem",
    ["75"] = "Samurai",
    ["76"] = "Shota",
    ["77"] = "Survival",
    ["78"] = "Time Travel",
    ["79"] = "Vampires",
    ["80"] = "Traditional Games",
    ["81"] = "Virtual Reality",
    ["82"] = "Zombies",
    ["83"] = "Incest",
    ["84"] = "Mafia"
  }
  if genres[genre] ~= nil then
    return genres[genre]
  else
    return genre
  end
end

local langs = {
  ["sa"] = "Arabic",
  ["bd"] = "Bengali",
  ["bg"] = "Bulgarian",
  ["mm"] = "Burmese",
  ["ct"] = "Catalan",
  ["cn"] = "Chinese (Simp)",
  ["hk"] = "Chinese (Trad)",
  ["cz"] = "Czech",
  ["dk"] = "Danish",
  ["nl"] = "Dutch",
  ["gb"] = "English",
  ["ph"] = "Filipino",
  ["fi"] = "Finnish",
  ["fr"] = "French",
  ["de"] = "German",
  ["gr"] = "Greek",
  ["hu"] = "Hungarian",
  ["id"] = "Indonesian",
  ["it"] = "Italian",
  ["jp"] = "Japanese",
  ["kr"] = "Korean",
  ["my"] = "Malay",
  ["mn"] = "Mongolian",
  ["ir"] = "Persian",
  ["pl"] = "Polish",
  ["br"] = "Portuguese (Br)",
  ["pt"] = "Portuguese (Pt)",
  ["ro"] = "Romanian",
  ["ru"] = "Russian",
  ["rs"] = "Serbo-Croatian",
  ["es"] = "Spanish (Es)",
  ["mx"] = "Spanish (LATAM)",
  ["se"] = "Swedish",
  ["th"] = "Thai",
  ["tr"] = "Turkish",
  ["ua"] = "Ukrainian",
  ["vn"] = "Vietnamese"
}

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

function getpagenumber()
  HTTP.Cookies.Values['mangadex_h_toggle'] = '1'
  local chapterid = URL:match('chapter/(%d+)')
  delay()
  if HTTP.GET(MaybeFillHost(MODULE.RootURL,'/api/chapter/'..chapterid)) then
    local x=TXQuery.Create(HTTP.Document)
    x.ParseHTML(StreamToString(HTTP.Document):gsub('<', ''):gsub('>', ''):gsub('&quot;', ''))
    local hash = x.XPathString('json(*).hash')
    local srv = x.XPathString('json(*).server')
    local v = x.XPath('json(*).page_array()')
    for i = 1, v.Count do
      local v1 = v.Get(i)
      local s = MaybeFillHost(MODULE.RootURL, srv .. '/' .. hash .. '/' .. v1.ToString())
      TASK.PageLinks.Add(s)
    end
    return true
  else
    return false
  end
  return true
end

local dirurl='/titles/2'

function getdirectorypagenumber()
  HTTP.Cookies.Values['mangadex_h_toggle'] = '1'
  HTTP.Cookies.Values['mangadex_title_mode'] = '2'
  delay()
  if HTTP.GET(MODULE.RootURL .. dirurl) then
    local x = TXQuery.Create(HTTP.Document)
    PAGENUMBER = tonumber(x.XPathString('(//ul[contains(@class,"pagination")]/li/a)[last()]/@href'):match('/2/(%d+)')) or 1
    return no_error
  else
    return net_problem
  end
end

function getnameandlink()
  HTTP.Cookies.Values['mangadex_h_toggle'] = '1'
  HTTP.Cookies.Values['mangadex_title_mode'] = '2'
  delay()
  if HTTP.GET(MODULE.RootURL .. dirurl .. '/' .. IncStr(URL) .. '/') then
    local x = TXQuery.Create(HTTP.Document)
    x.XPathHREFAll('//a[contains(@class, "manga_title")]',LINKS,NAMES)
    return no_error
  else
    return net_problem
  end
end

function delay()
  local interval = tonumber(MODULE.GetOption('luainterval'))
  local delay = tonumber(MODULE.GetOption('luadelay')) -- * MODULE.ActiveConnectionCount
  
  if (interval == nil) or (interval < 0) then interval = 1000; end
  if (delay == nil) or (delay < 0) then delay = 1000; end
  
  local lastDelay = MODULE.Storage['lastDelay']
  if lastDelay ~= '' then
    lastDelay = tonumber(lastDelay)
    if GetCurrentTime() - lastDelay < interval then
    print(GetCurrentTime() - lastDelay)
      Sleep(delay)
    end
  end
  
  MODULE.Storage['lastDelay'] = tostring(GetCurrentTime())
end

function getFormData(formData)
	local t=tostring(os.time())
	local b=string.rep('-',39-t:len())..t
	local crlf=string.char(13)..string.char(10)
	local r=''
	for k,v in pairs(formData) do
		r=r..'--'..b..crlf..
			'Content-Disposition: form-data; name="'..k..'"'..crlf..
			crlf..
			v..crlf
	end
	r=r..'--'..b..'--'..crlf
	return 'multipart/form-data; boundary='..b,r
end

function Login()
	MODULE.ClearCookies()
	MODULE.Account.Status=asChecking
	local login_url=MODULE.RootURL..'/login'
	if not HTTP.GET(login_url) then
		MODULE.Account.Status=asUnknown
		return false
	end
	local login_post_url=TXQuery.Create(HTTP.Document).XPathString('//form[@id="login_form"]/@action') or ''
	if login_post_url=='' then
		MODULE.Account.Status=asUnknown
		return false
	end
	login_post_url=MODULE.RootURL..login_post_url:gsub('&nojs=1','')
	HTTP.Reset()
	
	HTTP.Headers.Values['Origin']= ' '..MODULE.RootURL
	HTTP.Headers.Values['Referer']= ' '..login_url
	HTTP.Headers.Values['Accept']=' */*'
	HTTP.Headers.Values['X-Requested-With']=' XMLHttpRequest'
	
	local post_data
	HTTP.MimeType,post_data=getFormData({
		login_username=MODULE.Account.Username,
		login_password=MODULE.Account.Password,
		two_factor='',
		remember_me='1'})

	HTTP.POST(login_post_url,post_data)
	if HTTP.ResultCode==200 then
		if HTTP.Cookies.Values['mangadex_rememberme_token']~='' then
			MODULE.Account.Status=asValid
		else
			MODULE.Account.Status=asInvalid
		end
	else
		MODULE.Account.Status=asUnknown
	end
	return true
end

function Init()
  local m = NewWebsiteModule()
  m.ID                       = 'd07c9c2425764da8ba056505f57cf40c'
  m.Name                     = 'MangaDex'
  m.RootURL                  = 'https://mangadex.org'
  m.Category                 = 'English'
  m.OnGetInfo                = 'getinfo'
  m.OnGetPageNumber          = 'getpagenumber'
  m.OnGetNameAndLink         = 'getnameandlink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'  
  m.MaxTaskLimit             = 1
  m.MaxConnectionLimit       = 2
  m.AccountSupport           = true
  m.OnLogin                  = 'Login'

  m.AddOptionSpinEdit('luainterval', 'Min. interval between requests (ms)', 1000)
  m.AddOptionSpinEdit('luadelay', 'Delay (ms)', 1000)
  m.AddOptionCheckBox('luashowscangroup', 'Show scanlation group', false)
  
  local items = 'All'
  local t = getlanglist()
  for k, v in ipairs(t) do items = items .. '\r\n' .. v; end
  m.AddOptionComboBox('lualang', 'Language:', items, 11)
end
