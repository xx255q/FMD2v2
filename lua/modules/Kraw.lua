function Init()
	local m = NewWebsiteModule()
	m.ID                         = 'abe29b9f5f9e4fff94068fe547a93cef'
	m.Name                       = 'Kraw'
	m.RootURL                    = 'https://kraw.me'
	m.Category                   = 'Raw'
	m.OnGetNameAndLink           = 'GetNameAndLink'
	m.OnGetInfo                  = 'GetInfo'
	m.OnGetPageNumber            = 'GetPageNumber'
end

function GetNameAndLink()
	local rooturl = MODULE.RootURL
	local dirurl = rooturl .. '/all/'
	if HTTP.GET(dirurl) then
		local dirurl_next = rooturl .. '/wp-admin/admin-ajax.php'
		local next_page = 1
		local post_data = 'action=load_more&class=Essential_Addons_Elementor%5CPro%5CElements%5CPost_List&args=orderby%3Dtitle%26order%3Dasc%26ignore_sticky_posts%3D1%26post_status%3Dpublish%26posts_per_page%3D20%26offset%3D0%26post_type%3Dpost%26tax_query%255B0%255D%255Btaxonomy%255D%3Dpost_tag%26tax_query%255B0%255D%255Bfield%255D%3Dterm_id%26tax_query%255B0%255D%255Bterms%255D%255B0%255D%3D14%26tax_query%255B0%255D%255Bterms%255D%255B1%255D%3D466%26tax_query%255Brelation%255D%3DAND&taxonomy%5Btaxonomy%5D=all&taxonomy%5Bfield%5D=term_id&taxonomy%5Bterms%5D%5B%5D=&settings=eael_post_list_post_feature_image%3Dyes%26eael_post_list_post_meta%3D%26eael_post_list_post_title%3Dyes%26eael_post_list_post_excerpt%3Dno%26eael_post_list_featured_area%3Dyes%26eael_post_list_featured_meta%3D%26eael_post_list_featured_title%3Dyes%26eael_post_list_featured_excerpt%3Dno%26eael_post_list_pagination%3Dyes%26eael_post_list_layout_type%3Ddefault%26eael_post_list_title_tag%3Dh2&page='
		local x = CreateTXQuery()
		local function getList()
			HTTP.Document.SaveToFile('temp\\kraw_'..tostring(next_page)..'.html')
			x.ParseHTML(HTTP.Document)
			local count = LINKS.Count
			x.XPathHREFAll('//h2[@class="eael-post-list-title"]/a', LINKS, NAMES)
			return LINKS.Count > count
		end
		while true do			
			if HTTP.Terminated then break end			
			if not getList() then break end			
			next_page = next_page + 1
			UPDATELIST.UpdateStatusText('Loading page ' .. next_page)
			HTTP.Reset()
			HTTP.Headers.Values['Origin'] = rooturl
			HTTP.Headers.Values['Referer'] = dirurl
			if not HTTP.POST(dirurl_next, post_data .. tostring(next_page)) then break end			
		end
		return no_error
	else
		return net_problem
	end
end

function GetInfo()
	MANGAINFO.URL = MaybeFillHost(MODULE.RootURL, URL)
	if HTTP.GET(MANGAINFO.URL) then
		x = CreateTXQuery(HTTP.Document)
		MANGAINFO.Title      = x.XPathString('//h2[contains(@class, "elementor-heading-title")]')
		MANGAINFO.CoverLink  = x.XPathString('//div[contains(@class, "bdt-gallery-thumbnail")]/img/@data-lazy-src')
		MANGAINFO.Authors    = x.XPathString('//ul[@class="elementor-icon-list-items"]/li[2]/span/substring-after(.,":")')
		MANGAINFO.Status     = MangaInfoStatusIfPos(x.XPathString('//ul[@class="elementor-icon-list-items"]/li[4]/span'))
		MANGAINFO.Summary    = x.XPathString('//div[@class="premium-unfold-editor-content"]/p')

		local chapters, v = {}
		for v in x.XPath('//h3[@class="elementor-post__title"]/a').Get() do
			table.insert(chapters, {url=v.GetAttribute('href'),title=v.ToString()})
		end
		table.sort(chapters, function (a,b) return (tonumber(a.title:match('%d+')) or 0) < (tonumber(b.title:match('%d+')) or 0) end)
		for _, v in ipairs(chapters) do
			MANGAINFO.ChapterLinks.Add(v.url)
			MANGAINFO.ChapterNames.Add(v.title)
		end
		return no_error
	else
		return net_problem
	end
end

function GetPageNumber()
	if HTTP.GET(MaybeFillHost(MODULE.RootURL, URL)) then
		CreateTXQuery(HTTP.Document).XPathStringAll('//div[@class="gallery"]/img/@data-lazy-src', TASK.PageLinks)
		return true
	else
		return false
	end
end