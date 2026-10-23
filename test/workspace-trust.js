const {By: by, error, until} = require('selenium-webdriver');

// Only accept the trust prompt for the disposable test workspace. Other dialogs
// must remain failures, with their text available in the test output.
async function dismissTrustDialogue(driver) {
    const dialogs = await driver.findElements(by.css('.monaco-dialog-box[role="dialog"]'));
    for (const dialog of dialogs) {
        if (!await dialog.isDisplayed()) {
            continue;
        }
        const buttons = await dialog.findElements(by.css('.monaco-button'));
        for (const button of buttons) {
            if ((await button.getText()).includes('Yes, I trust the authors')) {
                await button.click();
                await driver.wait(until.stalenessOf(dialog), 10000,
                    'The workspace trust dialog did not close');
                return true;
            }
        }
        throw new Error(`Unexpected editor dialog: ${await dialog.getText()}`);
    }
    return false;
}

async function clickWithWorkspaceTrust(driver, locator) {
    await driver.wait(async () => {
        await dismissTrustDialogue(driver);
        const element = await driver.findElement(locator);
        try {
            await element.click();
            return true;
        } catch (e) {
            if (!(e instanceof error.ElementClickInterceptedError)) {
                throw e;
            }
            // The trust prompt can appear between checking for it and clicking.
            // Retry only when that specific prompt accounts for the interception.
            if (!await dismissTrustDialogue(driver)) {
                throw e;
            }
            return false;
        }
    }, 10000, 'Could not click after accepting workspace trust');
}

module.exports = {dismissTrustDialogue, clickWithWorkspaceTrust};
