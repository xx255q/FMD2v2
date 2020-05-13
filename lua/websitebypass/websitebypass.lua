if S == 'cf' then bypass = require 'websitebypass.cloudflare' end
if bypass ~= nil then
	return bypass.bypass()
else
	return false
end