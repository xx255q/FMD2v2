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
  m.ID                       = 'b98e788b9e9e4a08b79e4a91acba06e4'
  m.Name                     = 'MangaTensei'
  m.RootURL                  = 'https://www.mangatensei.com'
  m.Category                 = 'English'
  m.OnGetInfo                = 'GetInfo'
  m.OnGetDirectoryPageNumber = 'GetDirectoryPageNumber'
  m.OnGetNameAndLink         = 'GetNameAndLink'
  m.OnGetPageNumber          = 'GetPageNumber'
end