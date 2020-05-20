local _cf = {}

function _cf.IUAMChallengeAnswer(self, body, url)
	local js = body:match('setTimeout%(function%(%){%s+(.-a%.value%s*=%s*%S+)')

	if js == nil then
		LOGGER.SendError('WebsitBypass[clounflare]: IUAM challenge detected but failed to parse the javascript\r\n' .. body)
		return nil
	end

	local subVars = ''
	local k = body:match('k%s*=%s*[\'"](.-)[\'"];')
	if k ~= nil then
		k = k:gsub('-', '%%-')
		local i, jsi = '', ''
		for i, jsi in body:gmatch('<div id="' .. k .. '(%d+)">%s*([^<>]*)</div>') do
			subVars = subVars .. string.format('\n\t\t%s%s: %s,\n', k, i, jsi)
		end
		subVars = subVars:gsub(',\n$', '')
	end

	return ExecJS(string.format([[String.prototype.italics=function(str) {return "<i>" + this + "</i>";};
        var subVars= {%s};
        var document = {
            createElement: function () {
                return { firstChild: { href: "%s/" } }
            },
            getElementById: function (str) {
                return {"innerHTML": subVars[str]};
            }
        };
    ]], subVars, SplitURL(url)):gsub('%s+', ' ') .. js)
end

function _cf.sleepOrBreak(self, delay)
	local count = 0
	while count < delay do
		if HTTP.Terminated then break end
		count = count + 250
		Sleep(250)
	end
end

function _cf.solveIUAMChallenge(self, body, url)
	local answer = self:IUAMChallengeAnswer(body, url)
	if (answer == nil) or (answer == 'NaN') then
		LOGGER.SendError('WebsitBypass[clounflare]: IUAM challenge detected but failed to solve the javscript challenge\r\n' .. body)
		return false
	end

	local form, challengeUUID = body:match('<form (.-="challenge%-form" action="(.-__cf_chl_jschl_tk__=%S+)"(.-)</form>)')
	if (form == nil) or (challengeUUID == nil) then
		LOGGER.SendError('WebsitBypass[clounflare]: IUAM challenge detected but failed to parse the form\r\n' .. body)
		return false
	end

	local payload = {}
	local k, n, v = '', '', ''
	for k in form:gmatch('\n%s*<input%s(.-name=".-)/>') do
		n = k:match('name="(.-)"')
		v = k:match('value="(.-)"') or ''
		payload[n] = v
	end

	local i = 0; for _ in pairs(payload) do i = i + 1 end
	if i == 0 then
		LOGGER.SendError('WebsitBypass[clounflare]: IUAM challenge detected but failed to parse the form payload\r\n' .. body)
		return false
	end
	payload['jschl_answer'] = answer

	local rawdata = ''
	for k, v in pairs(payload) do
		rawdata = rawdata .. k .. '=' .. EncodeURLElement(payload[k]) .. '&'
	end
	rawdata = rawdata:gsub('&$', '')

	local domain = SplitURL(url)

	-- no need to redirect if it success
	HTTP.FollowRedirection = false
	HTTP.Reset()
	HTTP.Headers.Values['Origin'] = ' ' .. domain
	HTTP.Headers.Values['Referer'] = ' ' .. url
	HTTP.MimeType = "application/x-www-form-urlencoded"
	HTTP.Document.WriteString(rawdata)
	HTTP.FollowRedirection = true

	-- cloudflare requires a delay
	local delay = tonumber(body:match('submit%(%);\r?\n%s*},%s*(%d%d%d%d+)')) or 6000
	if delay < 6000 then delay = 6000 end
	self:sleepOrBreak(delay)

	HTTP.Request('POST', domain .. challengeUUID)
	return HTTP.Cookies.Values["cf_clearance"] ~= ""
end

function _cf.solveChallenge(self, url)
	local body = HTTP.Document.ToString()
	local rc = HTTP.ResultCode

	-- firewall blocked
	if (rc == 403) and body:find('<span class="cf%-error%-code">1020</span>') then
		LOGGER.SendError('WebsitBypass[clounflare]: Cloudflare has blocked this request (Code 1020 Detected). ')
		return false
	end
	-- reCapthca challenge
	if (rc == 403) and body:find('action="/.-__cf_chl_captcha_tk__=%S+".-data%-sitekey=.-') then
		LOGGER.SendError('WebsitBypass[clounflare]: detected reCapthca challenge, not supported right now. can be redirected to third party capthca solver in the future')
		return false
	end
	-- new IUAM challenge
	if ((rc == 429) or (rc == 503)) and body:find('cpo.src%s*=%s*"/cdn%-cgi/challenge%-platform/orchestrate/jsch/v1"') then
		LOGGER.SendError('WebsitBypass[clounflare]: detected the new Cloudflare challenge, not supported yet')
		return false
	end
	-- IUAM challenge
	if ((rc == 429) or (rc == 503)) and body:find('<form .-="challenge%-form" action="/.-__cf_chl_jschl_tk__=%S+"') then
		return self:solveIUAMChallenge(body, url)
	end

	LOGGER.SendWarning('WebsitBypass[clounflare]: no Cloudflare solution found!')
	return false
end

function _cf.bypass(self, METHOD, URL)
	local counter = 0
	local maxretry = HTTP.RetryCount; HTTP.RetryCount = 0

	while true do
		counter = counter + 1
		if self:solveChallenge(URL) then break end
		if HTTP.Terminated then break end
		-- delay before retry
		self:sleepOrBreak(2000)
		if (maxretry > -1) and (maxretry <= counter) then break end
		HTTP.Reset()
		HTTP.Request('GET', URL)
	end

	HTTP.RetryCount = maxretry
	return result
end

return _cf