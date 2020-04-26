if http.ResultCode < 500 then return false end
if not http.Headers.Values['Content-Type']:find('text/html') then return false end
if http.Document.ToString():find('name="jschl_vc"') then
  return true
end
return false
