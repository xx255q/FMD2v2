Modules = {}

function Modules.Madara()
  local Madara = {}
  
  function Madara:new()
    local obj = {}
    setmetatable(obj, self)
    self.__index = self
    return obj
  end
  
  function Madara:getinfo()
    MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
    if HTTP.GET(MANGAINFO.URL) then
      local x=TXQuery.Create(HTTP.Document)
      
      if MODULE.Name == 'NinjaScans' then
        local fixedHtml = StreamToString(HTTP.Document):gsub('a href=(.-/)>', 'a href="%1">')
        x.ParseHTML(fixedHtml)
      end
      
      MANGAINFO.Title=x.XPathStringAll('//div[@class="post-title"]/*[self::h1 or self::h2 or self::h3]/text()', '')
      if string.match(MANGAINFO.Title:upper(), ' RAW$') ~= nil then
        MANGAINFO.Title = MANGAINFO.Title:sub(1, -5)
      end
      if MODULE.Name == 'ArtemisNF' then
        MANGAINFO.Title=x.XPathStringAll('//div[@class="post-title post-sigle-title"]/*[self::h1 or self::h2 or self::h3]/text()', '')
      elseif MODULE.Name == 'GetManhwa' then
	    MANGAINFO.Title=x.XPathStringAll('//div[@class="post-title-dpage"]/h3')
      end
      MANGAINFO.CoverLink=x.XPathString('//div[@class="summary_image"]//img/@data-src')
      if MANGAINFO.CoverLink == '' then
        MANGAINFO.CoverLink=x.XPathString('//div[@class="summary_image"]//img/@src')
      end
      if MODULE.Name == 'GetManhwa' then
        MANGAINFO.CoverLink=x.XPathString('//div[@class="my_profile-manga"]/@style'):match('background%-image:URL%((.-)%)')
      end
      MANGAINFO.Authors=x.XPathStringAll('//div[@class="author-content"]/a')
      if MANGAINFO.Authors == '' then
        MANGAINFO.Authors=x.XPathStringAll('//div[@class="summary-heading-creator"]/a')
      end
      MANGAINFO.Artists=x.XPathStringAll('//div[@class="artist-content"]/a')
      MANGAINFO.Genres=x.XPathStringAll('//div[@class="genres-content"]/a')
      if MODULE.Name == 'ATMSubs' then
        MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="summary-heading" and contains(h5, "Statut")]/following-sibling::div'), 'En Cours', 'Complete')
      else
        MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="summary-heading" and contains(h5, "Status")]/following-sibling::div'))
      end
      if MODULE.Name == 'Mangareceh' then
        MANGAINFO.Summary=x.XPathString('//div[contains(@class,"description-summary")]//p[2]')
      else
        MANGAINFO.Summary=x.XPathString('//div[contains(@class,"description-summary")]//p')
      end
      
      if MODULE.Name == 'DoujinYosh' or MODULE.Name == 'MangaYosh' or MODULE.Name == 'KIDzScan' then
        local v = x.XPath('//li[contains(@class, "wp-manga-chapter")]/a')
        for i = 1, v.Count do
          local v1 = v.Get(i)
          local link = v1.GetAttribute('href')
          if MODULE.Name == 'MangaYosh' then
            link = string.gsub(link, 'https://yosh.tranivson.me', MODULE.RootURL)
          else
            link = string.gsub(link, 'https://doujinyosh.bloghadi.me', MODULE.RootURL)
          end
          MANGAINFO.ChapterNames.Add(v1.ToString());
          MANGAINFO.ChapterLinks.Add(link);
        end
      elseif MODULE.Name == 'PlotTwistNoFansub' then
        x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]//a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
      elseif MODULE.Name == 'Mangareceh' then
        x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a[1]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
      else
        x.XPathHREFAll('//li[contains(@class, "wp-manga-chapter")]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
      end
      InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
      return no_error
    end
    return net_problem
  end
  
  function Madara:getpagenumber()
    TASK.PageLinks.Clear()
    local aurl = MaybeFillHost(MODULE.RootURL, URL)
    if Pos('style=list', aurl) == 0 then
      aurl = aurl .. '?style=list'
    end
    if HTTP.GET(aurl) then
      local x = TXQuery.Create(HTTP.Document)
      if MODULE.Name == 'ManhwaHentai' then
        v = x.XPath('//div[contains(@class, "page-break")]/img')
        for i = 1, v.Count do
          v1 = v.Get(i)
          local src = v1.GetAttribute('src')
          src = src:gsub('https://cdn.shortpixel.ai/client/q_glossy,ret_img/', '')
          TASK.PageLinks.Add(src)
        end
      else
        x.XPathStringAll('//div[contains(@class, "page-break")]/img/@data-src', TASK.PageLinks)
      end
      if TASK.PageLinks.Count == 0 then
        x.XPathStringAll('//div[@class="entry-content"]//picture/img/@src', TASK.PageLinks)
      end
      if TASK.PageLinks.Count < 1 then
        x.XPathStringAll('//div[contains(@class, "page-break")]/img/@src', TASK.PageLinks)
  	  end
      if TASK.PageLinks.Count < 1 then
        x.XPathStringAll('//*[@class="wp-manga-chapter-img webpexpress-processed"]/@src', TASK.PageLinks)
      end
      if TASK.PageLinks.Count < 1 then
        x.XPathStringAll('//div[@class="reading-content"]//img/@src', TASK.PageLinks)
      end
      return true
    end
    return false
  end
  
  function Madara:getnameandlink()
    local perpage = 100
    local q = 'action=madara_load_more&page='.. URL ..'&template=madara-core%2Fcontent%2Fcontent-archive&vars%5Bpost_type%5D=wp-manga&vars%5Berror%5D=&vars%5Bm%5D=&vars%5Bp%5D=0&vars%5Bpost_parent%5D=&vars%5Bsubpost%5D=&vars%5Bsubpost_id%5D=&vars%5Battachment%5D=&vars%5Battachment_id%5D=0&vars%5Bname%5D=&vars%5Bstatic%5D=&vars%5Bpagename%5D=&vars%5Bpage_id%5D=0&vars%5Bsecond%5D=&vars%5Bminute%5D=&vars%5Bhour%5D=&vars%5Bday%5D=0&vars%5Bmonthnum%5D=0&vars%5Byear%5D=0&vars%5Bw%5D=0&vars%5Bcategory_name%5D=&vars%5Btag%5D=&vars%5Bcat%5D=&vars%5Btag_id%5D=&vars%5Bauthor%5D=&vars%5Bauthor_name%5D=&vars%5Bfeed%5D=&vars%5Btb%5D=&vars%5Bpaged%5D=1&vars%5Bmeta_key%5D=&vars%5Bmeta_value%5D=&vars%5Bpreview%5D=&vars%5Bs%5D=&vars%5Bsentence%5D=&vars%5Btitle%5D=&vars%5Bfields%5D=&vars%5Bmenu_order%5D=&vars%5Bembed%5D=&vars%5Bignore_sticky_posts%5D=false&vars%5Bsuppress_filters%5D=false&vars%5Bcache_results%5D=true&vars%5Bupdate_post_term_cache%5D=true&vars%5Blazy_load_term_meta%5D=true&vars%5Bupdate_post_meta_cache%5D=true&vars%5Bposts_per_page%5D='.. tostring(perpage) ..'&vars%5Bnopaging%5D=false&vars%5Bcomments_per_page%5D=50&vars%5Bno_found_rows%5D=false&vars%5Border%5D=ASC&vars%5Borderby%5D=post_title&vars%5Btemplate%5D=archive&vars%5Bsidebar%5D=full&vars%5Bpost_status%5D=publish'
    if HTTP.POST(MODULE.RootURL .. '/wp-admin/admin-ajax.php', q) then
      if HTTP.Headers.Values['Content-Length'] == '0' then return no_error end
      local x = TXQuery.Create(HTTP.Document)
      if x.XPath('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a').Count == 0 then return no_error end
      x.XPathHREFAll('//div[contains(@class, "post-title")]/*[self::h5 or self::h3]/a', LINKS, NAMES)
      UPDATELIST.CurrentDirectoryPageNumber = UPDATELIST.CurrentDirectoryPageNumber + 1
      return no_error
    else
      return net_problem
    end
  end
  
  return Madara
end

function Modules.ChibiManga()
  local ChibiManga = {}
  setmetatable(ChibiManga, { __index = Modules.Madara() })
  
  function ChibiManga:getpagenumber()
    TASK.PageLinks.Clear()
    if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
      local x = TXQuery.Create(HTTP.Document)
      local s = x.XPathString('//script[contains(., "chapter_preloaded_images")]', TASK.PageLinks)
      s = "["..GetBetween("[", "]", s).."]"
      x.ParseHTML(s)
      x.XPathStringAll('json(*)()', TASK.PageLinks)
      return true
    end
    return false
  end
  
  return ChibiManga
end

function Modules.HentaiRead()
  local HentaiRead = {}
  setmetatable(HentaiRead, { __index = Modules.Madara() })
  
  function HentaiRead:getpagenumber()
    TASK.PageLinks.Clear()
    if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
      local x = TXQuery.Create(HTTP.Document)
      local s = x.XPathString('//script[contains(., "chapter_preloaded_images")]', TASK.PageLinks)
      s = "["..GetBetween("[", "]", s).."]"
      x.ParseHTML(s)
      x.XPathStringAll('json(*)()', TASK.PageLinks)
      return true
    end
    return false
  end
  
  return HentaiRead
end

function Modules.OnManga()
  local OnManga = {}
  setmetatable(OnManga, { __index = Modules.Madara() })
  
  function OnManga:getpagenumber()
    TASK.PageLinks.Clear()
    if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
      local x = TXQuery.Create(HTTP.Document)
      local s = x.XPathString('//script[contains(., "chapter_preloaded_images")]', TASK.PageLinks)
      s = "{"..GetBetween("{", "}", s).."}"
      x.ParseHTML(s)
      x.XPathStringAll('let $c := json(*) return for $k in jn:keys($c) return $c($k)', TASK.PageLinks)
      return true
    end
    return false
  end
  
  return OnManga
end
-------------------------------------------------------------------------------

function createInstance()
  local m = Modules[MODULE.Name]
  if m ~= nil then
    return m():new()
  else
    return Modules.Madara():new()
  end
end

------------------------------------------------------------------------------- 

function getinfo()
  return createInstance():getinfo()
end

function getpagenumber()
  return createInstance():getpagenumber()
end

function getnameandlink()
  return createInstance():getnameandlink()
end

function BeforeDownloadImage()
  HTTP.Headers.Values['referer'] = MODULE.RootURL
  return true
end

-------------------------------------------------------------------------------

function AddWebsiteModule(id, name, url, category)
  local m = NewWebsiteModule()
  m.ID                    = id
  m.Name                  = name
  m.RootURL               = url
  m.Category              = category
  m.OnGetInfo             = 'getinfo'
  m.OnGetPageNumber       = 'getpagenumber'
  m.OnGetNameAndLink      = 'getnameandlink'
  m.OnBeforeDownloadImage = 'BeforeDownloadImage'
end

function Init()
  local cat = 'Raw'
  AddWebsiteModule('29e070b824344c8697ceb9554a6d1d4b', 'MangazukiClub', 'https://mangazuki.club', cat)
  
  cat = 'English'
  AddWebsiteModule('4bce5afe51b646c6b0d30329a069ee83', 'IsekaiScan', 'https://isekaiscan.com', cat)
  AddWebsiteModule('e9f7ec544bb146bd966ef2dd10bda097', 'MangaKomi', 'https://mangakomi.com', cat)
  AddWebsiteModule('da2696a55d66491093c8fed44cc862fc', 'MangaZukiWhatStatus', 'https://whatstatus.co', cat)
  AddWebsiteModule('b0b9bb6881b54db9a9f55d97ad2412cf', 'MangaZukiOnline', 'https://www.mangazuki.online', cat)
  AddWebsiteModule('bc9f265005764a729a937792b59ec0e8', 'MangaZukiSite', 'https://www.mangazuki.site', cat)
  AddWebsiteModule('c5013ac1bc754fdca1f6206507dad6f0', 'MangaZukiMe', 'https://mangazuki.me', cat)
  AddWebsiteModule('41f215fc066f462497a3ce177d1384d2', 'YoManga', 'https://yomanga.info', cat)
  AddWebsiteModule('2885dbccc8e8472ca21b819eb31031fa', 'OnManga', 'https://onmanga.com', cat)
  AddWebsiteModule('ab063b144d1f465697374fb8a2222edc', 'ReadRawManga', 'https://www.readrawmanga.com', cat)
  AddWebsiteModule('0d904cb0bdf6475e9b36ad835cd3050c', 'MangaStreamCC', 'https://www.mangastream.cc', cat)
  AddWebsiteModule('d42f178a248241c89f943fa1b77c5df8', 'TeabeerComics', 'https://teabeercomics.com', cat)

  cat = 'English-Scanlation'
  AddWebsiteModule('f17a22a1a24640ecb2ae4f51c47a45c8', 'TrashScanlations', 'https://trashscanlations.com', cat)
  AddWebsiteModule('5a19d20a9731446489df49fd01c7cf77', 'ChibiManga', 'http://www.cmreader.info', cat)
  AddWebsiteModule('8f7396a288e947bba73556c9b0ec41d4', 'ZinManga', 'https://zinmanga.com', cat)
  AddWebsiteModule('6812a5f225734ae5acd4b851d66a1450', 'SiXiangScans','http://www.sixiangscans.com', cat)
  AddWebsiteModule('bb35ade3f2bd46b1a6617756eddafb8d', 'NinjaScans', 'https://ninjascans.com', cat)
  AddWebsiteModule('3858e0e419ef41789e3e869ad1d56cd6', 'ReadManhua', 'https://readmanhua.net', cat)
  AddWebsiteModule('9bec8a0a18e94de1b99c2bc2598438b4', 'MangaDods', 'https://www.mangadods.com', cat)
  AddWebsiteModule('a24656d5e72544469f656e490ffc2591', 'DisasterScans', 'https://disasterscans.com', cat)
  AddWebsiteModule('a1b569bcec9147f4945383655c052676', 'RaiderScans', 'https://raiderscans.com', cat)
  
  cat = 'French'
  AddWebsiteModule('41867fa36f2f49959df9fef8aa53ffb5', 'WakaScan', 'https://wakascan.com', cat)
  AddWebsiteModule('d41da6d28179493ab074698a3a60cbcd', 'ATMSubs', 'https://atm-subs.fr', cat)
  AddWebsiteModule('d31eddfe86ee4ac584f3120d4cc6f8c9', 'NovelFrance', 'http://novel-france.fr', cat)

  cat = 'Indonesian'
  AddWebsiteModule('6c3cb7a05d8243d8817a81d3875ff1a1', 'MangaYosh', 'https://mangayosh.xyz', cat)
  AddWebsiteModule('9ff90d87df4c48fbb1cd310cbccca181', 'KomikGo', 'https://komikgo.com', cat)
  AddWebsiteModule('8949b01268974dd8a5798fec469deb65', 'KlikManga', 'https://klikmanga.com', cat)
  AddWebsiteModule('273d639c63fa4cafb4f27a4b6e0679d9', 'PojokManga', 'https://pojokmanga.com', cat)
  AddWebsiteModule('994972530f574d449db141c75d41047b', 'Mangareceh', 'https://mangareceh.ID', cat)

  cat = 'H-Sites'
  AddWebsiteModule('58bc6b05826e45e1b66e32d40ddc9fbd', 'ManhwaHand', 'https://manhwahand.com', cat)
  AddWebsiteModule('c0214763110b4d14b6e359ecef6df2e4', 'DoujinYosh', 'https://doujinyosh.fun', cat)
  AddWebsiteModule('d8e44dca037b4e159d320466adc06ca3', 'ManhwaHentai', 'https://manhwahentai.me', cat)
  AddWebsiteModule('dfdec22299bc4fc6adbad401eeca2211', 'HentaiRead', 'http://hentairead.com', cat)
  AddWebsiteModule('349e30b0c30643f4a8d0aaece2a2c41e', 'ManhwaClub', 'https://manhwa.club', cat)
  AddWebsiteModule('3f8ce23cedeb4d4983ca4ff0cb0bad50', 'NManhwa', 'https://nmanhwa.com', cat)
  AddWebsiteModule('ac3452866dd843fda8b859afe8c8faab', 'ManyToonCom', 'https://manytoon.com', cat)
  AddWebsiteModule('2b5f00cfbb124546b6c11a7e5ec9c403', 'Hiperdex', 'https://hiperdex.com', cat)
  AddWebsiteModule('c87aeae76e884adc9de8bc6b9d56f2c6', 'ShosetsuManga', 'https://www.shosetsu-manga.org', cat)

  cat = 'Spanish-Scanlation'
  AddWebsiteModule('eb72aedce86b4598b3d702bc055079e8', 'GodsRealmScan', 'https://godsrealmscan.com', cat)
  AddWebsiteModule('f1614587f5ca4f9588943f73f2434711', 'DarkskyProjects', 'https://darkskyprojects.org', cat)
  AddWebsiteModule('a142bbc912ce48e1a1fbee17d48d2aa2', 'PlotTwistNoFansub', 'https://www.plotwistscan.com', cat)
  AddWebsiteModule('c12ecc189301414aa091640c6980b531', 'KnightNoFansub', 'https://knightnoscanlation.com', cat)
  AddWebsiteModule('8ae3c5a7a1924715940594be746448a9', 'CopyPasteScanlation', 'https://copypastescan.xyz', cat)
  AddWebsiteModule('59742c2d007d47c487ce93b7db44b8f4', 'ZManga', 'https://zmanga.org', cat)
  AddWebsiteModule('206c360cb8bc45799a475e116e96ba53', 'KIDzScan', 'https://grafimanga.com', cat)
  AddWebsiteModule('a7e0464a604d43478f32083d505ff939', 'HunterFansub', 'https://hunterfansub.com', cat)
  AddWebsiteModule('3a669fd3bd104cfbbc917520d6483d0a', 'SDLGFansub', 'https://www.sdlg-fansub.tk', cat)
  AddWebsiteModule('7976200824a7422d8f16a71dea38f672', 'ArtemisNF', 'https://artemisnf.com', cat)
  AddWebsiteModule('cfc11bf4971f4db892f96867239771e7', 'LazyBoysScan', 'https://lazyboysscan.com', cat)
  AddWebsiteModule('a9e9305e841f43f588a0deff0bc6ac88', 'BDSFansub', 'https://bdsfansub.com', cat)

  cat = 'Webcomics'
  AddWebsiteModule('2e6196018ffc4c92bc7ff332e41d52d4', 'ManyToon', 'https://manytoon.me', cat)
  AddWebsiteModule('d903d55663f0423ca4dd928c8203f7ce', 'PocketAngelScan', 'https://pocketangelscans.com', cat)
  AddWebsiteModule('1bc39e1bc5e64c12989c051fe3932d4d', 'Toonily', 'https://toonily.com', cat)
  AddWebsiteModule('4f0a570000854509b71350983fa55eec', 'ManhuaBox', 'https://manhuabox.net', cat)
  AddWebsiteModule('6e4b199622604f6fb77c95457dd1145e', 'TopManhua', 'https://topmanhua.com', cat)
  AddWebsiteModule('00f2827e03654ad5bcde3b7ae536b416', 'Wakamics', 'https://wakamics.com', cat)
  AddWebsiteModule('8a230d1a3f5c416fa71d07127e8dfab5', 'GetManhwa', 'https://getmanhwa.co', cat)
  AddWebsiteModule('35da2f2d501c4035b03c7a66c449cb90', 'Manhuas', 'https://manhuas.net', cat)
  AddWebsiteModule('9dd41197c1d74ce780502dddc7515722', 'MixedManga', 'https://mixedmanga.com', cat)
  AddWebsiteModule('346075984bf043b798e9c782fc779a3a', 'MangaTX', 'https://mangatx.com', cat)
  AddWebsiteModule('689a65d9f4694e9389928304053c97fc', 'NightComic', 'http://nightcomic.com', cat)
  AddWebsiteModule('dcbd58d5134d424b8e11802bf0671873', 'MangaDao', 'https://mangadao.com', cat)
  AddWebsiteModule('74e6c5f8121846029f367d48db7da3d6', '365Manga', 'https://365manga.com', cat)
  AddWebsiteModule('59387ea5d936420290485efe70432f07', 'MangaBob', 'https://mangabob.com', cat)
  AddWebsiteModule('35a8b3b4c8064f589aa7da94bb52f1fd', 'Manga68', 'https://manga68.com', cat)
  AddWebsiteModule('01e9a8ebaa994307bef01780909e8cb7', 'EarlyManga', 'https://earlymanga.Name', cat)
  AddWebsiteModule('24619e7027134eaeb26797fa05fdd2b3', 'Mangakiss', 'https://mangakiss.org', cat)
  AddWebsiteModule('35a207b2fd0c47b68e78b531b57cde3f', 'MangaLord', 'https://www.mangalord.com', cat)
  AddWebsiteModule('5d8e32f00a0b4d0da2e5e562b63a5fc3', 'KissMangaIN', 'https://kissmanga.in', cat)
  AddWebsiteModule('03cb00729e26448294329face3a8b53b', 'MiracleScans', 'https://miraclescans.com', cat)
  AddWebsiteModule('c7aebe73845f43149bd5a8cbe84fd926', '1stKissManga', 'https://1stkissmanga.com', cat)

  cat = 'Arabic-Scanlation'
  AddWebsiteModule('7bda2905b61c49d1976777e9f2356361', '3asqOrg', 'https://3asq.org', cat)
  AddWebsiteModule('27c2c6db9ce24942a89a28aa6c6ed35d', 'AzoraManga', 'https://www.azoramanga.com', cat)

  cat = 'Turkish'
  AddWebsiteModule('baeb4d0c63d9456dbc8da6f1d29faf60', 'AdonisFansub', 'https://manga.adonisfansub.com', cat)

  cat = 'Arabic'
  AddWebsiteModule('45fe9b73641b4597a659b308061ee663', 'Mangalek', 'https://mangalek.com', cat)
  AddWebsiteModule('dec0b341fb92445ab6c435053941f2bd', 'MangaAction', 'https://manga-action.com', cat)
  AddWebsiteModule('7afaf3a070bc4bd499255c3fd8dec1f3', 'NijiTranslations', 'https://niji-translations.com', cat)
  AddWebsiteModule('c9fc048e3f82419996345bbb1626f7f7', 'MangaArabTeam', 'https://mangaarabteam.com', cat)

  cat = 'Portuguese'
  AddWebsiteModule('59d791cfd7dc425897581c231f6f6481', 'IchirinNoHanaYuri', 'https://ichirinnohanayuri.com.br', cat)
  AddWebsiteModule('c6839e89512b46dda84a5992480b63e0', 'YaoiToshokan', 'https://www.yaoitoshokan.com.br', cat)

  cat = 'Russian'
  AddWebsiteModule('b1ffe6dfda364ebcaad07eb2fd4aeae9', 'BestManga', 'https://bestmanga.club', cat)

  cat = 'Adult'
  AddWebsiteModule('53ea37e05bb6487e8bfe0dde6248fdc6', 'Milftoon', 'https://milftoon.xxx', cat)

  cat = 'Portuguese'
  AddWebsiteModule('b2395d7d5e5244fea800216c12cac7dd', 'NeoxScanlator', 'https://neoxscan.com/newsite', cat)
end
