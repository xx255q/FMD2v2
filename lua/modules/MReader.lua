function Init()
    local m = NewWebsiteModule()
    m.ID                       = 'd297f1eb6b784ded9b50d3b85cee5276'
    m.Name                     = 'MangaNeko'
    m.RootURL                  = 'https://www.mgeko.cc'
    m.Category                 = 'English'
    m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
    m.OnGetNameAndLink         = 'GetNameAndLink'
    m.OnGetInfo                = 'GetInfo'
    m.OnGetPageNumber          = 'GetPageNumber'
    m.SortedList               = true
    return m
end

local DirectoryPagination = '/browse-comics/?results=%s&filter=New'

local function userDataToString(userData)
    if type(userData) == "userdata" then
        if userData.ToString then
            return userData:ToString()
        elseif userData.Read then
            return userData:Read(userData.Size)
        else
            return tostring(userData)
        end
    else
        return tostring(userData)
    end
end

function GetDirectoryPageNumber()
    local u = MODULE.RootURL .. DirectoryPagination:format('1')
    if not HTTP.GET(u) then 
        return net_problem 
    end
    local documentString = userDataToString(HTTP.Document)
    local query = CreateTXQuery(documentString)
    local pageNumberString = query.XPathString('(//ul[@class="pagination"])[1]/li[last()-1]')
    PAGENUMBER = tonumber(pageNumberString) or 1
    return no_error
end

function GetNameAndLink()
    local u = MODULE.RootURL .. DirectoryPagination:format(tostring(URL + 1))
    if not HTTP.GET(u) then 
        return net_problem 
    end
    local documentString = userDataToString(HTTP.Document)
    local query = CreateTXQuery(documentString)
    query.XPathHREFTitleAll('//li[@class="novel-item"]/a', LINKS, NAMES)
    return no_error
end

function GetInfo()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    if not HTTP.GET(u) then 
        return net_problem
    end
    local query = CreateTXQuery(HTTP.Document)
    
    MANGAINFO.Title = query.XPathString('//h1[@class="novel-title text2row"]')
    
    -- Improved cover image extraction
    local coverLink = query.XPathString('//figure[@class="cover"]//img/@data-src')
    if not coverLink or coverLink == '' then
        coverLink = query.XPathString('//figure[@class="cover"]//img/@src')
    end
    
    if coverLink and coverLink ~= '' then
        coverLink = coverLink:gsub('%?.*$', '')
        coverLink = coverLink:gsub('%%', '%%%%')
        if not coverLink:match('^https?://') then
            coverLink = MODULE.RootURL .. coverLink
        end
        MANGAINFO.CoverLink = coverLink
    end
    
    MANGAINFO.Authors   = query.XPathString('//div[@class="author"]/a/span[@itemprop="author"]')
    MANGAINFO.Genres    = query.XPathStringAll('//div[@class="categories"]/ul/li/a')
    MANGAINFO.Status    = query.XPathString('//div[@class="header-stats"]//strong[contains(@class, "ongoing") or contains(@class, "completed")]')
    MANGAINFO.Summary   = query.XPathString('//p[@class="description"]')

    -- Check for "All Chapters" link
    local allChaptersLink = query.XPathString('//a[contains(@href, "/all-chapters/")]/@href')
    
    -- If "All Chapters" link exists, use it to get chapters
    if allChaptersLink and allChaptersLink ~= '' then
        if HTTP.GET(MaybeFillHost(MODULE.RootURL, allChaptersLink)) then
            query = CreateTXQuery(HTTP.Document)
        end
    end

    -- Get chapters
    local chapters = query.XPath('//ul[@class="chapter-list"]/li/a')
    if chapters.Count == 0 then
        -- If no chapters found, try a different XPath
        chapters = query.XPath('//ul[contains(@class, "chapter-list")]/li/a')
    end

    for v in chapters.Get() do
        local chapterTitle = query.XPathString('.//span[contains(@class, "chapter-title")] | .//strong[@class="chapter-title"]', v)
        local chapterLink = v.GetAttribute('href')
        if chapterTitle and chapterTitle ~= '' and chapterLink and chapterLink ~= '' then
            -- Extract chapter number
            local chapterNumber = chapterTitle:match("Chapter (%d+)")
            if chapterNumber then
                local standardizedTitle = string.format("Chapter %s", chapterNumber)
                local chapterID = chapterLink:match("/chapter%-([%d%-]+)")
                if chapterID then
                    standardizedTitle = string.format("%s [%s]", standardizedTitle, chapterID)
                end
                MANGAINFO.ChapterNames.Add(standardizedTitle)
                MANGAINFO.ChapterLinks.Add(chapterLink)
            else
                -- If no chapter number found, use the original title
                MANGAINFO.ChapterNames.Add(chapterTitle)
                MANGAINFO.ChapterLinks.Add(chapterLink)
            end
        end
    end

    -- Reverse chapter order if needed and if chapters were found
    if MANGAINFO.ChapterLinks.Count > 0 and MANGAINFO.ChapterLinks.Reverse then
        MANGAINFO.ChapterLinks.Reverse()
        MANGAINFO.ChapterNames.Reverse()
    end

    return no_error
end

function GetPageNumber()
    local u = MaybeFillHost(MODULE.RootURL, URL)
    if not HTTP.GET(u) then 
        return net_problem 
    end
    local documentString = userDataToString(HTTP.Document)
    local query = CreateTXQuery(documentString)
    query.XPathStringAll('//section[@class="page-in content-wrap"]//center/div/img/@src', TASK.PageLinks)
    return no_error
end
