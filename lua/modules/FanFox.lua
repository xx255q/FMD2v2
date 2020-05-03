function Init()
  local cat = 'English'
  AddWebsiteModule('8e7bd7b38aa041aa9bc1bddeec33b6f4', 'MangaHere', 'https://www.mangahere.cc', cat)  
  local m = AddWebsiteModule('0d3653a8d9b747a381374f32e0a1641e', 'FanFox', 'https://fanfox.net', cat)
  m.OnAfterImageSaved = 'AfterImageSaved'
  m.AddOptionCheckBox('mf_removewatermark', 'Remove watermark', true)
  m.AddOptionCheckBox('mf_saveaspng', 'Save as PNG', false)
  MangaFoxLoadTemplate(FMD.LuaDirectory .. 'extras' .. PathDelim .. 'mangafoxtemplate')
end

function AddWebsiteModule(id, name, rooturl, cat)
  local m = NewWebsiteModule()
  m.ID                       = id
  m.Name                     = name
  m.RootURL                  = rooturl
  m.Category                 = cat
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetInfo                = 'GetInfo'
  m.OnGetPageNumber          = 'GetPageNumber'
  return m
end

function GetDirectoryPageNumber()
  if HTTP.GET(MODULE.RootURL .. '/directory/?az') then
    local x = TXQuery.Create(HTTP.Document)
    PAGENUMBER = tonumber(x.XPathString('//div[@class="pager-list"]//a[last()-1]')) or 1
    return no_error
  else
    return net_problem
  end
end

function GetNameAndLink()
  if HTTP.GET(MODULE.RootURL .. '/directory/' .. IncStr(URL) .. '.html?az') then
    TXQuery.Create(HTTP.Document).XPathHREFAll('//ul[contains(@class, "manga-list")]/li/a', Links, Names)
    return no_error
  else
    return net_problem
  end
end

function GetInfo()
  MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
  HTTP.Cookies.Values['isAdult'] = '1'
  if HTTP.GET(MANGAINFO.URL) then
    local x = TXQuery.Create(HTTP.Document)
    MANGAINFO.Title     = x.XPathString('//span[@class="detail-info-right-title-font"]')
    MANGAINFO.CoverLink = MaybeFillHost(MODULE.RootURL, x.XPathString('//img[@class="detail-info-cover-img"]/@src'))
    MANGAINFO.Authors   = x.XPathString('//p[@class="detail-info-right-say"]/a')
    MANGAINFO.Genres    = x.XPathStringAll('//p[@class="detail-info-right-tag-list"]/a')
    MANGAINFO.Summary   = x.XPathString('//p[@class="fullcontent"]')
    MANGAINFO.Status    = MangaInfoStatusIfPos(x.XPathString('//span[@class="detail-info-right-title-tip"]'))
	for _, v in ipairs(x.XPathI('//ul[@class="detail-main-list"]/li/a')) do
      MANGAINFO.ChapterLinks.Add(v.GetAttribute('href'):gsub('1%.html$', ''))
      MANGAINFO.ChapterNames.Add(x.XPathString('./div/p[@class="title3"]', v))
    end
    InvertStrings(MANGAINFO.ChapterLinks, MANGAINFO.ChapterNames)
    return no_error
  else
    return net_problem
  end
end

function GetPageNumber()
  TASK.PageLinks.Clear()
  TASK.PageNumber = 0
  HTTP.Cookies.Values['isAdult'] = '1'
  local aurl = MaybeFillHost(MODULE.RootURL, URL):gsub('1%.html$', '')
  local lurl = aurl .. '1.html'
  if HTTP.GET(lurl) then
    local x = TXQuery.Create(HTTP.Document)
    local key = ExecJS('var $=function(){return{val:function(){}}},newImgs,guidkey;' .. x.XPathString('//script[contains(., "eval")]') .. ';newImgs||guidkey;')
    if key:len() > 16 then
      TASK.PageLinks.CommaText = key
    else
      local s = x.XPathString('//script[contains(., "chapterid")]')
	  local cid = s:match('chapterid%s*=%s*(.-)%s*;') or '0'
	  TASK.PageNumber = tonumber(s:match('imagecount%s*=%s*(%d-)%s*;') or '0')
      if TASK.PageNumber == nil then TASK.PageNumber = 1 end
      local p = 1
      while p <= TASK.PageNumber do
        HTTP.Reset()
        HTTP.Headers.Values['Pragma'] = 'no-cache'
        HTTP.Headers.Values['Cache-Control'] = 'no-cache'
        HTTP.Headers.Values['Referer'] = lurl
        if HTTP.XHR(aurl .. string.format('chapterfun.ashx?cid=%s&page=%d&key=%s', cid, p, key)) then
		  s = HTTP.Document.ToString()
          if s ~= '' then
            s = ExecJS(s .. ';d;')
			for i in s:gmatch('[^,]+') do
			  TASK.PageLinks.Add(i)
			end
          end
        end
		-- sometimes the server will give more images than the actual chapter have
		-- it is an ads or file not found(404 not found)
		-- remove invalid images here
		while TASK.PageLinks.Count > TASK.PageNumber do
		  TASK.PageLinks.Delete(TASK.PageLinks.Count-1)
		end
        if TASK.PageLinks.Count >= TASK.PageNumber then break end
        p = TASK.PageLinks.Count + 1
        Sleep(2000) -- without minimum delay of 2 seconds server will only give 2 images for each xhr request
      end
    end
    return true
  else
    return false
  end
end

function AfterImageSaved()
  if MODULE.GetOption('mf_removewatermark') then
    MangaFoxRemoveWatermark(FILENAME, MODULE.GetOption('mf_saveaspng'))
  end
  return true
end;
