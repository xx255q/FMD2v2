-- don't change the function name (hardcoded, case sensitive)
-- must return 1 boolean value
-- this function will be called very often(for each of http request), keep it minimal
function ____CheckAntiBot()
	local rc = HTTP.ResultCode
	if ((rc == 403) or (rc == 429) or (rc == 503)) and HTTP.Headers.Values["Content-Type"]:lower():find("text/html") then
		if HTTP.Headers.Values['Server']:lower():find('cloudflare') then
			return true, 'cloudflare'
		end
	end
	return false
end