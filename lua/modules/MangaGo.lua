local mgdirurl = '/list/directory/all/'
local rdirurl = '/directory/?gid=all&sindex=all&status=all&page='

function getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(MANGAINFO.URL) then
    local x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then MANGAINFO.Title=x.XPathString('//h1'):gsub(' manga$', ''); end
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//div[@class="left cover"]/img/@src'))
    MANGAINFO.Authors=x.XPathString('//div[@class="manga_right"]//td/label[.="Author:"]/string-join(following-sibling::*/text(),", ")')
    MANGAINFO.Genres=x.XPathString('//div[@class="manga_right"]//td/label[.="Genre(s):"]/string-join(following-sibling::*/text(),", ")')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="manga_right"]//td/label[.="Status:"]/following-sibling::*/text()'))
    MANGAINFO.Summary=x.XPathString('//div[@class="manga_summary"]/string-join(text(),codepoints-to-string(10))')
    x.XPathHREFAll('//table[@id="chapter_table"]//td//a[not(@style)]', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    InvertStrings(MANGAINFO.ChapterLinks,MANGAINFO.ChapterNames)
    HTTP.Reset()
    HTTP.Headers.Values['Referer'] = MANGAINFO.URL
    return no_error
  else
    return net_problem
  end
end

function r_getinfo()
  MANGAINFO.URL=MaybeFillHost(MODULE.RootURL,URL)
  if HTTP.GET(MANGAINFO.URL) then
    x=TXQuery.Create(HTTP.Document)
    if MANGAINFO.Title == '' then MANGAINFO.Title=x.XPathString('//div[@class="title"]/h1'); end
    MANGAINFO.CoverLink=MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="pic"]/@src'))
    MANGAINFO.Authors = x.XPathString('//div[@class="cartoon-intro"]//p[contains(span, "Author")]/text()')
    MANGAINFO.Status = MangaInfoStatusIfPos(x.XPathString('//div[@class="cartoon-intro"]//p/span[contains(., "Status")]/following-sibling::span[1]'))
    MANGAINFO.Genres = x.XPathStringAll('//div[@class="cartoon-intro"]//p[contains(span, "Author")]/a')
    MANGAINFO.Summary = x.XPathString('//div[@class="summary"]')
    x.XPathHREFAll('//table[@class="list_table"]//tr/td[@class="chapter_name_td"]/a', MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    HTTP.GET(MODULE.RootURL .. '/ywz.ico')
    HTTP.Reset()
    HTTP.Headers.Values['Referer'] = MANGAINFO.URL
    return no_error
  else
    return net_error
  end
end

function getdirectorypagenumber()
  HTTP.Headers.Values['Referer'] = MODULE.RootURL
  local s = MODULE.RootURL .. mgdirurl .. '1/'
  if HTTP.GET(s) then
    x=TXQuery.Create(HTTP.Document)
    PAGENUMBER=x.XPathCount('//div[@class="pagination"]//ol/li//select/option')
    return true
  else
    return false
  end
end

function r_getdirectorypagenumber()
  HTTP.Headers.Values['Referer'] = MODULE.RootURL
  local s = MODULE.RootURL .. rdirurl .. '1'
  if HTTP.GET(s) then
    x=TXQuery.Create(HTTP.Document)
    PAGENUMBER=x.XPathCount('//div[@class="pagination"]//ol/li//select/option')
    return true
  else
    return false
  end
end

local js = require 'utils.jsunpack'
function getpagenumber()
  TASK.PageLinks.Clear()
  TASK.PageNumber=0
  local rurl = MaybeFillHost(MODULE.RootURL, URL)
  if HTTP.GET(rurl) then
    x=TXQuery.Create(HTTP.Document)
    local cnt = x.XPathCount('//ul[@id="dropdown-menu-page"]/li/a')
    local s = x.XPathString('//script[contains(.,"imgsrcs")]')
    if s == '' then return false; end
    if Pos(' imgsrcs = new Array', s) > 0 then
      s = GetBetween('(', ')', s)
    else
      s = GetBetween(' imgsrcs = \'', '\';', s)
    end
    s = s:gsub('\'', '')
    local script = x.XPathString('//script[contains(@src, "chapter.js")]/@src')
    HTTP.Reset()
    HTTP.Headers.Values['Referer'] = rurl
    if HTTP.GET(script) then
      local function unp(script)
        script = js.replacehex(script)
        local a, c = script:match('[\'"]%s*,%s*(%d+)%s*,%s*(%d+)%s*,%s*[\'"]')
        a, c = tonumber(a), tonumber(c)
        local d = script:match('[\'"]%s*,%s*%d+%s*,%s*%d+%s*,%s*[\'"].+[\'"].+split.+%([\'"](.+)[\'"]%)')
        local w = script:match('[\'"]%s*,%s*%d+%s*,%s*%d+%s*,%s*[\'"](.+)[\'"].+split')
        w = js.splitstr(w, d)
        local txt = script:match('}%s*%(%s*[\'"](.+)[\'"]%s*,%s*%d+%s*,%s*%d+%s*,%s*[\'"](.+)[\'"].+split')
        return js.unpack36(txt, a, c, w)
      end
      local function d3(script, u)
        local function _d(a, b)
          local function rp(a, b, c) return a:sub(1, b) .. c .. a:sub(b+2); end
          local c = a:len()
          for j = 4,1,-1 do
            for i = c-1,b[j],-1 do
              if i%2 ~= 0 then
                local tmp = string.char(a:byte(i-b[j]+1))
                a = rp(a, i-b[j], string.char(a:byte(i+1)))
                a = rp(a, i, tmp)
              end
            end
          end
          return a
        end
        local bstr = script:match('%d+', script:find('dorder%s*%(%s*img'))
        local b = {}
        for i=1,bstr:len() do b[i] = bstr:byte(i) - 48; end
        return _d(u, b)
      end
      MODULE.Storage['r'] = '1'
      if Pos(',', s) == 0 then
        script = unp(StreamToString(HTTP.Document))
      elseif Pos('http', s) == 0 then
        script = unp(StreamToString(HTTP.Document))
        if script:match('referrerPolicy:"no%-referrer"') ~= nil then
          MODULE.Storage['r'] = '0'
        end
        s = d3(script, s)
      else
      end
      local a = js.splitstr(s, ',')
      for i = 1, math.min(cnt, #a) do
        TASK.PageLinks.Add(SeparateRight(a[i], '//'))
      end
      if MODULE.Storage['loaded'] == '0' then filldict(script) end
    end
  else
    return false
  end
  return true
end

function beforedownloadimage()
  if MODULE.Storage['r'] == '1' then
    HTTP.Headers.Values['Referer'] = MODULE.RootURL
  end
  return true
end

function filldef(m)
  local dict = TStrings.Create()
  dict.Values['60a2b0ed56cd458c4633d04b1b76b7e9'] = '18a72a69a64a13a1a43a3aa42a23a66a26a19a51a54a78a34a17a31a35a15a58a29a61a48a73a74a44a52a60a24a63a20a32a7a45a53a75a55a62a59a41a76a68a2a36a21a10a38a33a71a40a67a22a4a50a80a65a27a37a47a70a14a28a16a6a56a30a57a5a11a79a9a77a46a39a25a49a8a12'
  dict.Values['400df5e8817565e28b2e141c533ed7db'] = '61a74a10a45a3a37a72a22a57a39a25a56a52a29a70a60a67a41a63a55a27a28a43a18a5a9a8a40a17a48a44a79a38a47a32a73a4a6a13a34a33a49a2a42a50a76a54a36a35a14a58a7a69a46a16a30a21a11aa51a53a77a26a31a1a19a20a80a24a62a68a59a66a75a12a64a78a71a15a65a23'
  dict.Values['84ba0d8098f405b14f4dbbcc04c93bac'] = '61a26a35a16a55a10a72a37a2a60a66a65a33a44a7a28a70a62a32a56a30a40a58a15a74a47aa36a78a75a11a6a77a67a39a23a9a31a64a59a13a24a80a14a38a45a21a63a19a51a17a34a50a46a5a29a73a8a57a69a48a68a49a71a41a12a52a18a79a76a54a42a22a4a1a3a53a20a25a43a27'
  dict.Values['56665708741979f716e5bd64bf733c33'] = '23a7a41a48a57a27a69a36a76a62a40a75a26a2a51a6a10a65a43a24a1aa20a71a28a30a13a38a79a78a72a14a49a55a56a58a25a70a12a80a3a66a11a39a42a17a15a54a45a34a74a31a8a61a46a73a63a22a64a19a77a50a9a59a37a68a52a18a32a16a33a60a67a21a44a53a5a35a4a29a47'
  dict.Values['37abcb7424ce8df47ccb1d2dd9144b49'] = '67a45a39a72a35a38a61a11a51a60a13a22a31a25a75a30a74a43a69a50a6a26a16a49a77a68a59a64a17a56a18a1a10a54a44a62a53a80a5a23a48a32a29a79a24a70a28a58a71a3a52a42a55a9a14a36a73a34a2a27a57a0a21a41a33a37a76a8a40a65a7a20a12a19a47a4a78a15a63a66a46'
  dict.Values['874b83ba76a7e783d13abc2dabc08d76'] = '26a59a42a43a4a20a61a28a12a64a37a52a2a77a34a13a46a74a70a0a44a29a73a66a55a38a69a67a62a9a63a6a54a79a21a33a8a58a40a47a71a49a22a50a57a78a56a25a17a15a36a16a48a32a5a10a14a80a24a72a76a45a3a53a23a41a60a11a65a19a27a51a68a35a31a1a75a39a30a7a18'
  dict.Values['930b87ad89c2e2501f90d0f0e92a6b97'] = '9a29a49a67a62a40a28a50a64a77a46a31a16a73a14a45a51a44a7a76a22a78a68a37a74a69a25a65a41a11a52aa18a36a10a38a12a15a2a58a48a8a27a75a20a4a80a61a55a42a13a43a47a39a35a60a26a30a63a66a57a33a72a24a71a34a23a3a70a54a56a32a79a5a21a6a59a53a17a1a19'
  dict.Values['1269606c6c3d8bb6508426468216d6b1'] = '49a15a0a60a14a26a34a69a61a24a35a4a77a80a70a40a39a6a68a17a41a56a28a46a79a16a21a1a37a42a44a58a78a18a52a73a32a9a12a50a8a13a20a19a67a36a45a75a48a10a65a7a38a66a3a2a43a27a29a31a72a74a55a23a54a22a59a57a11a62a47a53a30a5a64a25a76a71a51a33a63'
  dict.Values['33a3b21bb2d14a09d15f995224ae4284'] = '30a59a35a34a42a8a10a56a70a64a48a69a26a18a6a16a54a24a73a79a68a33a32a2a63a53a31a14a17a57a41a80a76a40a60a12a43a29a39a4a77a58a66a36a38a52a13a19a0a75a28a55a25a61a71a11a67a49a23a45a5a15a1a50a51a9a44a47a65a74a72a27a7a37a46a20a22a62a78a21a3'
  dict.Values['9ae6640761b947e61624671ef841ee78'] = '62a25a21a75a42a61a73a59a23a19a66a38a71a70a6a55a3a16a43a32a53a37a41a28a49a63a47a17a7a30a78a46a20a67a56a79a65a14a69a60a8a52a22a9a24a2a4a13a36a27a0a18a33a12a44a5a76a26a29a40a1a11a64a48a39a51a80a72a68a10a58a35a77a54a34a74a57a31a50a45a15'
  dict.Values['a67e15ed870fe4aab0a502478a5c720f'] = '8a12a59a52a24a13a37a21a55a56a41a71a65a43a40a66a11a79a67a44a33a20a72a2a31a42a29a34a58a60a27a48a28a15a35a51a76a80a0a63a69a53a39a46a64a50a75a1a57a9a62a74a18a16a73a14a17a6a19a61a23a38a10a3a32a26a36a54a4a30a45a47a70a22a7a68a49a77a5a25a78'
  dict.Values['b6a2f75185754b691e4dfe50f84db57c'] = '47a63a76a58a37a4a56a21a1a48a62a2a36a44a34a42a23a9a60a72a11a74a70a20a77a16a15a35a69a0a55a46a24a6a32a75a68a43a41a78a31a71a52a33a67a25a80a30a5a28a40a65a39a14a29a64a3a53a49a59a12a66a38a27a79a45a18a22a8a61a50a17a51a10a26a13a57a19a7a54a73'
  dict.Values['db99689c5a26a09d126c7089aedc0d86'] = '57a31a46a61a55a41a26a2a39a24a75a4a45a13a23a51a15a8a64a37a72a34a12a3a79a42a80a17a62a49a19a77a48a68a78a65a14a10a29a16a20a76a38a36a54a30a53a40a33a21a44a22a32a5a1a7a70a67a58a0a71a74a43a66a6a63a35a56a73a9a27a25a59a47a52a11a50a18a28a60a69'
  dict.Values['d320d2647d70c068b89853e1a269c609'] = '77a38a53a40a16a3a20a18a63a9a24a64a50a61a45a59a27a37a8a34a11a55a79a13a47a68a12a22a46a33a1a69a52a54a31a23a62a43a0a2a35a28a57a36a51a78a70a5a32a75a41a30a4a80a19a21a42a71a49a10a56a74a17a7a25a6a14a73a29a44a48a39a60a58a15a66a67a72a65a76a26'
  dict.Values['c587e77362502aaedad5b7cddfbe3a0d'] = '50aa59a70a68a30a56a10a49a43a45a29a23a28a61a15a40a71a14a44a32a34a17a26a63a76a75a33a74a12a11a21a67a31a19a80a7a64a8a3a51a53a38a18a6a42a27a9a52a20a41a60a1a22a77a16a54a47a79a24a78a2a46a37a73a65a36a35a39a5a4a25a72a13a62a55a57a58a69a66a48'
  dict.Values['f4ab0903149b5d94baba796a5cf05938'] = '40a37a55a73a18a42a15a59a50a13a22a63a52a58a6a80a47a17a38a71a74a70a30a11a10a19a0a31a36a21a51a68a1a3a14a66a45a2a79a7a76a75a8a67a20a78a25a69a43a28a35a60a4a23a65a54a34a9a5a39a27a57a26a33a12a24a46a72a56a44a49a61a64a29a53a48a32a62a41a16a77'
  dict.Values['f5baf770212313f5e9532ec5e6103b61'] = '55a69a78a75a38a25a20a60a6a80a46a5a48a18a23a24a17a67a64a70a63a57a22a10a49a19a8a16a11a12a61a76a34a27a54a73a44a0a56a3a15a29a28a13a4a2a7a77a74a35a37a26a30a58a9a71a50a1a43a79a47a32a14a53a52a66a72a59a68a31a42a45a62a51a40a39a33a65a41a36a21'
  dict.Values['e2169a4bfd805e9aa21d3112d498d68c'] = '54a34a68a69a26a20a66a1a67a74a22a39a63a70a5a37a75a15a6a14a62a50a46a35a44a45a28a8a40a25a29a76a51a77a17a47a0a42a2a9a48a27a13a64a58a57a18a30a80a23a61a36a60a59a71a32a7a38a41a78a12a49a43a79a24a31a52a19a3a53a72a10a73a11a33a16a4a55a65a21a56'
  m.Storage['ik'] = '18a72a69a64a13a1a43a3aa42a23a66a26a19a51a54a78a34a17a31a35a15a58a29a61a48a73a74a44a52a60a24a63a20a32a7a45a53a75a55a62a59a41a76a68a2a36a21a10a38a33a71a40a67a22a4a50a80a65a27a37a47a70a14a28a16a6a56a30a57a5a11a79a9a77a46a39a25a49a8a12'
  m.Storage['dict'] = dict.Text
  m.Storage['loaded'] = '0'
end

function filldict(script)
  local dict = TStrings.Create()
  dict.Text = MODULE.Storage['dict']
  local dkvar = 'deskeys'
  local function fill(p)
    for k, v in script:gmatch(p) do
      if (k ~= nil) and (v ~= nil) then dict.Values[k] = v; end
    end
  end
  fill(dkvar..'%s*%[%s*[\'"]([^"\']+)[\'"]%s*%]%s*=%s*["\']([^"\']+)["\']')
  fill(dkvar..'%s*%.%s*([^"\']+)%s*=%s*["\']([^"\']+)["\']')
  MODULE.Storage['dict'] = dict.Text
  local ik = MODULE.Storage['ik']
  local s = script:match('[%s;,][fg]%s*=%s*["\']([^\'"]+)["\']')
  if s ~= nil then ik = s; end
  MODULE.Storage['ik'] = ik
  MODULE.Storage['loaded'] = '1'
end

function downloadimage()
  if HTTP.GET(URL) then
    if Pos('mangapicgallery.com/r/cspiclink', URL:lower()) > 0 then
      local ik = MODULE.Storage['ik']
      local dict = TStrings.Create()
      dict.Text = MODULE.Storage['dict']
      for i = 0,dict.Count-1 do
        local t = js.splitstr(dict.Get(i), dict.NameValueSeparator)
        if Pos(t[1], URL) > 0 then ik = t[2]; break; end
      end
      local a = js.splitstr(ik, 'a')
      local s = TImagePuzzle.Create(9, 9)
      for i = 1, #a do
        local n = tonumber(a[i])
        if n == nil then n = 0 end
        s.matrix[i-1] = n;
      end
      s.descramble(HTTP.Document, HTTP.Document)
    end
  else
    return false
  end
  return true
end

function getnameandlink()
  local ref = MODULE.RootURL
  local s = MODULE.RootURL
  s = s .. mgdirurl .. IncStr(URL) .. '/'
  if URL ~= '0' then ref = ref .. mgdirurl .. URL .. '/'; end
  HTTP.Headers.Values['Referer'] = ref
  if HTTP.GET(s) then
    local x = TXQuery.Create(HTTP.Document)
    x.XPathHREFTitleAll('//div[@class="directory_left"]//li/h3/a', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function r_getnameandlink()
  local ref = MODULE.RootURL
  local s = MODULE.RootURL
  s = s .. rdirurl .. IncStr(URL)
  if URL ~= '0' then ref = ref .. rdirurl .. URL; end
  HTTP.Headers.Values['Referer'] = ref
  if HTTP.GET(s) then
    local x = TXQuery.Create(HTTP.Document)
    x.XPathHREFTitleAll('//div[@id="sa-comic_show_list"]/ul/li/p/a[not(@class)]', LINKS, NAMES)
    return no_error
  else
    return net_problem
  end
end

function AddWebsiteModule(id, name, url)
  local m = NewWebsiteModule()
  filldef(m)
  m.ID = id
  m.Name = name
  m.RootURL = url
  m.Category = 'English'
  m.LastUpdated='March 2, 2018'
  m.OnGetInfo='getinfo'
  m.OnGetPageNumber = 'getpagenumber'
  m.OnGetNameAndLink = 'getnameandlink'
  m.OnGetDirectoryPageNumber = 'getdirectorypagenumber'
  m.OnBeforeDownloadImage = 'beforedownloadimage'
  m.OnDownloadImage = 'downloadimage'
  return m
end

function Init()
  AddWebsiteModule('77c3c3cd38444bd7bb4e5f63f2c5c93c', 'MangaGo', 'http://www.mangago.me')
  local m = AddWebsiteModule('872e595f6f52477090b8d50942a7c2aa', 'Rocaca', 'http://www.rocaca.com')
  m.OnGetInfo = 'r_getinfo'
  m.OnGetNameAndLink = 'r_getnameandlink'
  m.OnGetDirectoryPageNumber = 'r_getdirectorypagenumber'
end
