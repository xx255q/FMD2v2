----------------------------------------------------------------------------------------------------
-- Local Constants
----------------------------------------------------------------------------------------------------

local Template = require 'templates.AnyACG'
-- DirectoryPagination = '/'   --> Override template variable by uncommenting this line.

----------------------------------------------------------------------------------------------------
-- Event Functions
----------------------------------------------------------------------------------------------------

-- Get info and chapter list for current manga.
function GetInfo()
  Template.GetInfo()

  return no_error
end

-- Get the page count of the manga list of the current website.
function GetDirectoryPageNumber()
  Template.GetDirectoryPageNumber()

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

----------------------------------------------------------------------------------------------------
-- Module Initialization
----------------------------------------------------------------------------------------------------

function Init()
  local m = NewWebsiteModule()
  m.ID                       = '79315399013948dbbe289c2590bde8e0'
  m.Name                     = 'MangaWindow'
  m.RootURL                  = 'https://mangawindow.net'
  m.Category                 = 'English'
  m.OnGetInfo                = 'GetInfo'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
end