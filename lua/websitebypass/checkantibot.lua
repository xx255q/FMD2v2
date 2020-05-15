local rc = HTTP.ResultCode
if (rc ~= 503) or (rc ~= 429) then return false end
if not HTTP.Headers.Values["Content-Type"]:find("text/html") then return false end

-- cloudflare IUAM challenge
if HTTP.Headers.Values['Server']:find('cloudflare') then
	-- capthca, not supported right now. in the future can use third party captcha solver
	-- ResultCode must be 403
	-- if HTTP.Document.ToString():find('<form.*="challenge%-form".*__cf_chl_captcha_tk__=') then
		-- return false
	-- end
	-- IUAM challenge
	if HTTP.Document.ToString():find('<form.*="challenge%-form".*__cf_chl_jschl_tk__=') then
		return true, "cf"
	end
end


return false