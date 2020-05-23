----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.NineManga'
-- DirectoryPagination = '/'            --> Override template variable by uncommenting this line.
-- DirectorySuffix     = ''             --> Override template variable by uncommenting this line.
-- MangaInfoParameters = ''             --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  Template.GetInfo()

  return no_error
end

-- Get LINKS and NAMES from the manga list of the current website.
function GetNameAndLink()
  Template.GetNameAndLink()

  return no_error
end

-- Get the page count for the current chapter.
function GetPageNumber()
  Template.GetPageNumber()

  return no_error
end

-- Extract/Build/Repair image urls before downloading them.
function GetImageURL()
  Template.GetImageURL()

  return no_error
end

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  function AddWebsiteModule(id, name, url, category)
    local m = NewWebsiteModule()
    m.ID               = id
    m.Name             = name
    m.RootURL          = url
    m.Category         = category
    m.OnGetInfo        = 'GetInfo'
    m.OnGetNameAndLink = 'GetNameAndLink'
    m.OnGetPageNumber  = 'GetPageNumber'
    m.OnGetImageURL    = 'GetImageURL'
  end
  AddWebsiteModule('68c51e4148ee471fa221cd2d05cb216a', 'NineManga', 'http://www.ninemanga.com', 'English')
  AddWebsiteModule('a28662b823d449aa9f4cb246d9cd2921', 'NineManga', 'http://en.ninemanga.com', 'English')
end