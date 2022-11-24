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
jasmine.DEFAULT_TIMEOUT_INTERVAL = 300 * 1000;
jest.setTimeout(300 * 1000);

async function rightClick(element) {
    const actions = driver.actions({async: true});
    await actions.contextClick(element).perform();
}

async function hover(element) {
    const actions = driver.actions({async: true});
    await actions.move({origin: element}).perform();
}

async function sendControlP(element) {
    const actions = driver.actions({async: true});
    await actions.pause(2000).keyDown(Key.CONTROL).sendKeys('p').keyUp(Key.CONTROL).perform();
}

async function sendReturn() {
    const actions = driver.actions({async: true});
    await actions.pause(2000).keyDown(Key.RETURN).keyUp(Key.RETURN).perform();
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
        console.log(`navigate to ${baseUrl}`);
        await driver.get(baseUrl);

        // Wait for button
        console.log('wait for an element indicating that the workspace is up');
        await driver.wait(until.elementLocated(By.css('.monaco-workbench')), 10 * 1000);

        // Dismiss trust dialog if it comes up.
        console.log('check for the trust dialog');
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

        console.log('we should have an error shown');
        let squiggly = await driver.wait(until.elementLocated(By.css(".squiggly-error")));

        console.log('try to resolve includes automatically');
        let include_name_to_hover = await driver.wait(until.elementLocated(byVisibleText("test-inc.clsp")));
        await hover(include_name_to_hover);

        console.log('find the quick fix selection');
        let quickFixElement = await driver.wait(until.elementLocated(byVisibleText("Quick Fix")));
        quickFixElement.click();

        console.log('find the include path button');
        await sendReturn();

        console.log('find the input box');
        let inputBox = await driver.wait(until.elementLocated(By.css(".input")));
        await inputBox.sendKeys("include/test-inc.clsp");

        console.log('accept input');
        let okBox = await driver.wait(until.elementLocated(byVisibleText("OK")));
        okBox.click();

        console.log('Check the content of chialisp.json');
        await sendControlP();

        inputBox = await driver.wait(until.elementLocated(By.css(".input")));
        await inputBox.sendKeys("chialisp.json");

        let chialispFilename = await driver.wait(until.elementLocated(byExactText("chialisp.json")));
        await sendReturn();

        console.log('Check content');
        let chialispText = await driver.wait(until.elementLocated(byVisibleText('"./project/include"')));

        await sendControlP();

        inputBox = await driver.wait(until.elementLocated(By.css(".input")));
        await inputBox.sendKeys("collatz.cl");
        await sendReturn();

        console.log('edit');
        await sendReturn();

        console.log('comments should move');
        let otherComment = await driver.wait(until.elementLocated(byVisibleText("defun-inline")));
        expect(await otherComment.getAttribute("class")).toBe("mtk15");

        console.log('check for squigglies');
        let squigglies = await driver.findElements(By.css('.squiggly-error'));
        expect(squigglies.length).toBe(0);


        // Ok, the above didn't throw so we succeeded.
        console.log('we found the styled elements');
    });
});
