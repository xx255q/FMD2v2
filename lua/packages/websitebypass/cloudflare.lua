local _cf = {}

function _cf.getcf(self, url)
	local result, method, surl, postdata, sleeptime = false, "", "", "", 5000
	local body = HTTP.Document.ToString()
	if (body == "") or (url == "") then return result, method, surl, postdata, sleeptime end

	local r, jschl_vc, pass, jschl_answer = "", "", "", ""

	local x = TXQuery.Create(body)
	method   = x.XPathString('//form[@id="challenge-form"]/@method'):upper()
	surl     = x.XPathString('//form[@id="challenge-form"]/@action')
	r        = x.XPathString('//input[@name="r"]/@value')
	jschl_vc = x.XPathString('//input[@name="jschl_vc"]/@value')
	pass     = x.XPathString('//input[@name="pass"]/@value')

	if (method == "") or (surl == "") or (r == "") or (jschl_vc == "") or (pass == "") then return result, method, surl, postdata, sleeptime end

	-- main script
	local javascript = body:match('<script type="text/javascript">\r?\n(.-)</script>') or ""

	-- challenge
	local challenge, _, sleeptime = body:match('setTimeout%(function%(%)%{%s*(var ' ..
									's,t,o,p,b,r,e,a,k,i,n,g.-\r?\n.-a%.value%s*=.-)\r?\n' ..
									'([^%{<>]*%},%s*(%d))?')
	if challenge == nil then challenge = "" end
	if sleeptime == nil then sleeptime = 5000 else sleeptime = tonumber(sleeptime) or 5000 end

	local innerHTML, k = "", ""
	local i; for i in javascript:gmatch("([^;]+)") do
		print('get inner k=' .. tostring(i))
		if Trim(SeparateLeft(i, "=")) == "k" then
			k = Trim(SeparateRight(i, "="), " '")
			innerHTML = body:match('<div.-id="'..k..'".->(.-)</div>')
		end
	end
	local domain = SplitURL(URL, false, false)
	challenge = string.format([[
        var document = {
            createElement: function () {
              return { firstChild: { href: "http://%s/" } }
            },
            getElementById: function () {
              return {"innerHTML": "%s"};
            }
          };

        %s; a.value
]], domain, innerHTML, challenge)
print('challenge:\n' .. challenge)
	jschl_answer = ExecJS(challenge)

	if jschl_answer ~= "" then
		result = true
		postdata = "r=" .. EncodeURLElement(r) .. "&jschl_vc=" .. EncodeURLElement(jschl_vc) .. "&pass=" .. EncodeURLElement(pass) .. "&jschl_answer" .. EncodeURLElement(jschl_answer)
	end

	return result, method, surl, postdata, sleeptime
end

function _cf.bypass(self)
	print('call cf:bypass')
	local counter = 0
	local maxretry = HTTP.RetryCount; HTTP.RetryCount = 0
	local method, url, postdata, host = "POST", "", "", AppendURLDelim(GetHostURL(URL):gsub("http://", "https"))
	local sleeptime, sleepcount = 5000, 0
	local result, result_getcf = false, false

	while true do
		counter = counter + 1
		result_getcf, method, url, postdata, sleeptime = self:getcf(host)
		if resultgetjs and (method ~= "") and (url ~= "") then
			HTTP.Reset()
			if method == "POST" then
				StringToStream(HTTP.Document, postdata)
				HTTP.MimeType = "application/x-www-form-urlencoded"
			end
			HTTP.Headers.Values["Referer"] = " " + URL
			if sleeptime < 5000 then sleeptime = 5000 end
			sleepcount = 0
			while sleepcount < sleeptime do
				if HTTP.Terminated then break end
				sleepcount = sleepcount + 250
				Sleep(250)
			end
			HTTP.FollowRedirection = False
			HTTP.Request(method, FillHost(host, url))
			result = HTTP.Cookies.Values["cf_clearance"] ~= ""
			if HTTP.ResultCode == 403 then
				LOGGER.SendError("cloudflare bypass failed, probably asking for captcha? " .. URL)
			end
			HTTP.FollowRedirection = true
		end
		if result then break end
		if HTTP.Terminated then break end
		if (maxretry > -1) and (maxretry <= counter) then break end
		HTTP.Reset()
		HTTP.GET(URL)
	end

	HTTP.RetryCount = maxretry
	return result
end

return _cf