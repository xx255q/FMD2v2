if HTTP.ResultCode < 500 then return false end
if not HTTP.Headerss.Values['Content-Type']:find('text/html') then return false end
-- cloudflare
if HTTP.Document.ToString():find('name="jschl_vc"') then return true, 'cf' end
return false