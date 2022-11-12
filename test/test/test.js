// Require modules used in the logic below
const jasmine = require('jasmine');
const {Builder, By, Key, until} = require('selenium-webdriver');

// You can use a remote Selenium Hub, but we are not doing that here
require('chromedriver');
const driver = new Builder()
    .forBrowser('chrome')
    .build();

const baseUrl = "http://localhost:8080";
const password = "739f75e86e8d7843df146bac";

var login = async function() {
    let loginContainer = By.css('.login-form');
    let inpPassword = By.css('.password');
    let btnLogin = By.css('.submit');
    var enterCredentialsAndLogin = async function() {
        console.log('Entering credentials...');
        // Wait until an input element appears
        await driver.wait(until.elementLocated(inpPassword), 10 * 1000);
        // Enter credentials and log in
        console.log('sending password');
        await driver.findElement(inpPassword).sendKeys(password);
        console.log('pressing button');
        await driver.findElement(btnLogin).click();
    };

    // Load the login page
    await driver.get(baseUrl);

    // Wait until the page is loaded
    await driver.wait(until.elementLocated(loginContainer), 40 * 1000);
    console.log('Login screen loaded.');

    // Wait to be logged in, assuming it was was successful
    // once the Log in button has gone "stale."
    await enterCredentialsAndLogin();
    console.log('Logged in.');
};

// Configure Jasmine's timeout value to account for longer tests.
// Adjust this value if you find our tests failing due to timeouts.
jasmine.DEFAULT_TIMEOUT_INTERVAL = 40 * 1000;
jest.setTimeout(20 * 1000);

async function rightClick(element){
    const actions = driver.actions({async: true});
    await actions.contextClick(element).perform();
}

function byVisibleText(str) {
    return By.xpath(`//*[contains(text(),'${str}')]`);
}

function byExactText(str) {
    return By.xpath(`//*[text()='${str}']`);
}

// Define a category of tests using test framework, in this case Jasmine
describe("Basic element tests", function() {
    // Before every test, open a browser and login
    // using the logic written above.
    beforeEach(async function() {
        await login();
        console.log('Test beginning.');
    });

    // After each test, close the browser.
    afterAll(async function() {
        await driver.quit();
    });

    // Specify a test
    it("Can run extension", async function() {
        // Provide basic data used to evaluate the test.
        // This test should pass.
        console.log('Running test...');

        // Preview the test page
        await driver.get(baseUrl);

        // Wait for button
        await driver.wait(until.elementLocated(By.css('.monaco-workbench')), 10 * 1000);

        // Dismiss trust dialog if it comes up.
        let trustDialog = await driver.findElements(By.css('.dialog-shadow'));
        if (trustDialog.length !== 0) {
            let trustButton = await driver.wait(until.elementLocated(byVisibleText("Yes, I trust the authors")));
            trustButton.click();
        }

        let projectDir = await driver.wait(until.elementLocated(byExactText("project")));
        console.log('clicking project dir');
        projectDir.click();

        let chialispExt = await driver.wait(until.elementLocated(byVisibleText(".vsix")));
        console.log('right click chialisp ext');
        await rightClick(chialispExt);

        let installChoice = await driver.wait(until.elementLocated(byVisibleText("VSIX")));
        console.log('click install');
        installChoice.click();

        let chialisp = await driver.wait(until.elementLocated(byVisibleText("collatz.cl")));
        console.log('select chialisp file');
        chialisp.click();

        // If these elements can be found, we're highlighting.
        console.log('finding highlighting');
        let keyword_classed_element = await driver.wait(until.elementLocated(By.css(".mtk15")));

        console.log('checking mod highlight');
        let mod_keyword = await driver.wait(until.elementLocated(byExactText("mod")));
        expect(await mod_keyword.getAttribute("class")).toBe("mtk15");

        console.log('checking comment highlight');
        let comment = await driver.wait(until.elementLocated(byVisibleText("COLLATZ")));

        expect(await comment.getAttribute("class")).toBe("mtk4");

        // Ok, the above didn't throw so we succeeded.
        console.log('we found the styled elements');
    });
});
