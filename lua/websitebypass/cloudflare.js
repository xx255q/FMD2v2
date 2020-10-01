const { Builder, By, Key, promise, until } = require('selenium-webdriver');
const firefox = require('selenium-webdriver/firefox');

const url = process.argv[2];
const ua = process.argv[3];

async function getCookies() {
	const driver = new Builder()
		.forBrowser('firefox')
		.setFirefoxOptions(new firefox.Options()
			// .headless()
			.setAcceptInsecureCerts(true)
			.setPreference('general.useragent.override', ua)
			.setPreference('dom.webdriver.enabled', false)
			.setPreference('useAutomationExtension', false)
			.setPreference('app.normandy.enabled', false)
			.setPreference('app.normandy.optoutstudies.enabled', false)
			.setPreference('browser.newtabpage.activity-stream.feeds.asrouterfeed', false)
			.setPreference('browser.safebrowsing.downloads.remote.enabled', false)
			.setPreference('browser.safebrowsing.downloads.remote.enabled', false)
			.setPreference('dom.push.connection.enabled', false)
			.setPreference('dom.push.enabled', false)
			.setPreference('dom.push.serverURL', '')
			.setPreference('extensions.blocklist.enabled', false)
			.setPreference('extensions.getAddons.cache.enabled', false)
			.setPreference('extensions.pocket.enabled', false)
			.setPreference('geo.enabled', false)
			.setPreference('geo.provider.ms-windows-location', false)
			.setPreference('geo.provider.network.url', '')
			.setPreference('messaging-system.rsexperimentloader.enabled', false)
			.setPreference('network.captive-portal-service.enabled', false)
			.setPreference('network.dns.disablePrefetch', true)
			.setPreference('network.http.speculative-parallel-limit', 0)
			.setPreference('network.prefetch-next', false)
			.setPreference('security.OCSP.enabled', 0)
			)
		.setFirefoxService(new firefox.ServiceBuilder()
			.setHostname('127.0.0.1')
			)
		.build();
	await driver.get(url);
	try {
		const timeout = 60
		// await driver.wait(until.urlContains('/?__cf_chl_jschl_tk__='), timeout*1000);
		var tcount = 0;
		while (tcount < timeout) {
			try {
				await driver.manage().getCookie('cf_clearance');
				break;
				} catch {};
			tcount++;
			await driver.sleep(1000);
			};
		}
	catch {}
	finally {
		console.log(JSON.stringify(await driver.manage().getCookies()));
		await driver.quit();
		};
}

getCookies();