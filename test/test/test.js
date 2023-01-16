// Require modules used in the logic below
const jasmine = require('jasmine');
const os = require('os');
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

async function sendControlChar(char) {
    const actions = driver.actions({async: true});
    if (os.platform() === 'darwin') {
      await actions.pause(2000).keyDown(Key.COMMAND).sendKeys(char).keyUp(Key.COMMAND).pause(500).perform();
    } else {
      await actions.pause(2000).keyDown(Key.CONTROL).sendKeys(char).keyUp(Key.CONTROL).pause(500).perform();
    }
}

async function pressTab() {
    const actions = driver.actions({async: true});
    await actions.pause(500).keyDown(Key.TAB).keyUp(Key.TAB).perform();
}

async function sendControlA() { await sendControlChar('a'); }
async function sendControlF() { await sendControlChar('f'); }
async function sendControlH() { await sendControlChar('h'); }
async function sendControlP() { await sendControlChar('p'); }

async function wait(secs) {
    const actions = driver.actions({async: true});
    await actions.pause(secs * 1000).perform();
}

async function sendReturn() {
    const actions = driver.actions({async: true});
    await actions.pause(2000).keyDown(Key.RETURN).keyUp(Key.RETURN).pause(500).perform();
}

async function sendEscape() {
    const actions = driver.actions({async: true});
    await actions.pause(2000).keyDown(Key.ESCAPE).keyUp(Key.ESCAPE).pause(500).perform();
}

async function sendPgUp() {
    const actions = driver.actions({async: true});
    await actions.pause(1000).keyDown(Key.PAGE_UP).keyUp(Key.PAGE_UP).pause(500).perform();
}

async function sendRight(n) {
    if (n === undefined) {
        n = 1;
    }
    const actions = driver.actions({async: true});
    let a = actions.pause(1000);
    for (var i = 0; i < n; i++) {
        a = a.keyDown(Key.ARROW_RIGHT).keyUp(Key.ARROW_RIGHT).pause(500);
    }
    await a.perform();
}

async function sendString(s) {
    let actions = driver.actions({async: true});
    for (var i = 0; i < s.length; i++) {
        if (s.charAt(i) === ' ') {
            actions = actions.keyDown(Key.SPACE).keyUp(Key.SPACE).pause(200);
        } else if (s.charAt(i) >= 'a' && s.charAt(i) <= 'z') {
            let letter = s.charAt(i).toUpperCase().charCodeAt(0);
            actions = actions.keyDown(letter).keyUp(letter).pause(200);
        } else if (s.charAt(i) >= 'A' && s.charAt(i) <= 'Z') {
            let letter = s.charCodeAt(i);
            actions = actions.keyDown(letter).keyUp(letter).pause(200);
        } else {
            throw(new Error("unknown letter to type: " + s.charAt(i)));
        }
    }

    await actions.perform();
}

async function findString(s) {
    await sendControlF();
    let inputBox = await driver.wait(until.elementLocated(By.css(".input")));
    await inputBox.sendKeys(s);
    await sendEscape();
}

function byVisibleText(str) {
    return By.xpath(`//*[contains(text(),'${str}')]`);
}

function byExactText(str) {
    return By.xpath(`//*[text()='${str}']`);
}

function byAttribute(attr,val) {
    return By.xpath(`//*[@${attr}='${val}']`);
}

async function openFile(file) {
    console.log(`Check the content of ${file}`);
    await sendControlP();

    var inputBox = await driver.wait(until.elementLocated(By.css(".input")));
    await inputBox.sendKeys(file);

    let chialispFilename = await driver.wait(until.elementLocated(byExactText(file)));
    await sendReturn();
}

async function replaceString(olds, news) {
    await sendControlH();

    console.log('rename a function so some errors appear');
    let inputBox = await driver.wait(until.elementLocated(byAttribute("aria-label", "Find")));
    await inputBox.click();
    await sendControlA();
    await inputBox.sendKeys(olds);

    inputBox = await driver.wait(until.elementLocated(byAttribute("aria-label", "Replace")));
    await inputBox.click();
    await sendControlA();
    await inputBox.sendKeys(news);

    let nextMatchButton = await driver.wait(until.elementLocated(byAttribute("aria-label", "Next Match (Enter)")));
    await nextMatchButton.click();

    let replaceButton = await driver.wait(until.elementLocated(byAttribute("aria-label", "Replace (Enter)")));
    await replaceButton.click();

    let closeButton = await driver.wait(until.elementLocated(byAttribute("aria-label", "Close (Escape)")));
    await closeButton.click();

    const actions = driver.actions({async: true});
    await actions.pause(3000).perform();
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
        await projectDir.click();

        let chialispExt = await driver.wait(until.elementLocated(byVisibleText(".vsix")));
        console.log('right click chialisp ext');
        await rightClick(chialispExt);

        let installChoice = await driver.wait(until.elementLocated(byVisibleText("VSIX")));
        console.log('click install');
        await installChoice.click();

	      await sendReturn();

	      await openFile('collatz.cl');

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

	      await wait(3.0);

        console.log('Check the content of chialisp.json');
	      await openFile("chialisp.json");

        console.log('Check content');
        let chialispText = await driver.wait(until.elementLocated(byVisibleText('"./project/include"')));

	      await openFile("collatz.cl");

        console.log('comments should move');
        let otherComment = await driver.wait(until.elementLocated(byVisibleText("defun-inline")));
        expect(await otherComment.getAttribute("class")).toBe("mtk15");

        console.log('check for squigglies');
        let squigglies = await driver.findElements(By.css('.squiggly-error'));
        expect(squigglies.length).toBe(0);

        console.log('change an instance of odd to obd');
        await replaceString('odd', 'obd');

        console.log('check for squigglies caused by rename');
        squigglies = await driver.findElements(By.css('.squiggly-error'));
        expect(squigglies.length).toBe(1);

        console.log('change back to see the error disappear');
        await replaceString('obd', 'odd');
        squigglies = await driver.findElements(By.css('.squiggly-error'));
        expect(squigglies.length).toBe(0);

        // Add a parameter to the module and try to complete it.
        console.log('check for completion prompt');
        let actions = driver.actions({async: true});

        // Pgup right 7 space QQEX C-f defun-inline C-f logand right 6 Q
        // find a monaco-list-rows with a descendant that contains QQEX.
        await sendPgUp();
        await sendRight(7);
        await sendString(" QQEX");

        await findString('defun-inline');
        await findString('logand');
        await sendRight(5);
        await sendString(" Q");

        let monacoLists = await driver.findElements(By.css(".monaco-list"));
        var foundCompletionOffers = 0;
        for (var i = 0; i < monacoLists.length; i++) {
            var mlist = monacoLists[i];
            var foundElements = await mlist.findElements(byVisibleText("QQEX"));
            if (foundElements.length !== 0) {
                foundCompletionOffers += 1;
                break;
            }
        }
        expect(foundCompletionOffers).toBe(1);

        console.log("Accept offer");
        await sendReturn();

        await sendControlF();
        await sendControlA();
        inputBox = await driver.wait(until.elementLocated(By.css(".input")));
        await inputBox.sendKeys("QQEX");

        // Ensure there are 2 matches.
        await driver.wait(until.elementLocated(byVisibleText("1 of 2")));

        // Ok, the above didn't throw so we succeeded.
        console.log('all things we know how to test passed so far');
    });
});
