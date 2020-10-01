import sys
import json
from time import sleep
from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By
from selenium.common.exceptions import TimeoutException

url = sys.argv[1]
ua = sys.argv[2]

# options = webdriver.ChromeOptions()
# options.add_argument("--headless")
# options.add_argument('ignore-certificate-errors')
# options.add_argument("user-agent="+ua)
# browser = webdriver.Chrome(options=options, executable_path="chromedriver.exe")

options = webdriver.FirefoxOptions()
# options.add_argument("--headless")
profile = webdriver.FirefoxProfile()
profile.accept_untrusted_certs = True
profile.set_preference('general.useragent.override', ua)
profile.set_preference('dom.webdriver.enabled', False)
profile.set_preference('useAutomationExtension', False)
profile.set_preference('dom.webdriver.enabled', False)
profile.set_preference('useAutomationExtension', False)
profile.set_preference('app.normandy.enabled', False)
profile.set_preference('app.normandy.optoutstudies.enabled', False)
profile.set_preference('browser.newtabpage.activity-stream.feeds.asrouterfeed', False)
profile.set_preference('browser.safebrowsing.downloads.remote.enabled', False)
profile.set_preference('browser.safebrowsing.downloads.remote.enabled', False)
profile.set_preference('dom.push.connection.enabled', False)
profile.set_preference('dom.push.enabled', False)
profile.set_preference('dom.push.serverURL', '')
profile.set_preference('extensions.blocklist.enabled', False)
profile.set_preference('extensions.getAddons.cache.enabled', False)
profile.set_preference('extensions.pocket.enabled', False)
profile.set_preference('geo.enabled', False)
profile.set_preference('geo.provider.ms-windows-location', False)
profile.set_preference('geo.provider.network.url', '')
profile.set_preference('messaging-system.rsexperimentloader.enabled', False)
profile.set_preference('network.captive-portal-service.enabled', False)
profile.set_preference('network.dns.disablePrefetch', True)
profile.set_preference('network.http.speculative-parallel-limit', 0)
profile.set_preference('network.prefetch-next', False)
profile.set_preference('security.OCSP.enabled', 0)
profile.update_preferences()
browser = webdriver.Firefox(firefox_profile=profile,options=options,executable_path='geckodriver.exe')

browser.get(url)
try:
    timeout = 60
    # WebDriverWait(browser, timeout).until(EC.url_contains('/?__cf_chl_jschl_tk__='))
    tcount = 0
    while tcount < timeout:
        if browser.get_cookie('cf_clearance'):
            break
        tcount += 1
        sleep(1)
except:
    pass
finally:
    print(json.dumps(browser.get_cookies(), separators=(',', ':')))
    browser.quit()