-- don't change the function name(hardcoded, case sensitive)
-- don't change the first 2 parameters method and url(hardcoded)
-- must return 1 boolean value
function ____WebsiteBypass(METHOD, URL, srv)
	local bypass
	if HTTP.Headers.Values['Server']:lower():find('cloudflare') then
		bypass = require 'websitebypass.cloudflare'
	end
	if bypass ~= nil then
		return bypass:bypass(METHOD, URL)
	end
	return false
end