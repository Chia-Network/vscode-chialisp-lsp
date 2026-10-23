const {test} = require('node:test');
const assert = require('node:assert/strict');
const {error} = require('selenium-webdriver');
const {dismissTrustDialogue, clickWithWorkspaceTrust} = require('./workspace-trust');

function fixture({late = false, unexpected = false} = {}) {
    let open = !late;
    let accepted = false;
    let projectClicks = 0;
    const button = {
        getText: async () => unexpected ? 'Cancel' : 'Yes, I trust the authors',
        click: async () => {
            // Resolve on the next turn to expose an unawaited click.
            await new Promise(resolve => setImmediate(resolve));
            accepted = true;
            open = false;
        }
    };
    const dialog = {
        isDisplayed: async () => open,
        findElements: async () => [button],
        getText: async () => 'An unrelated confirmation',
        getTagName: async () => {
            if (!open) {
                throw new error.StaleElementReferenceError();
            }
            return 'div';
        }
    };
    const driver = {
        findElements: async () => open ? [dialog] : [],
        findElement: async () => ({click: async () => {
            projectClicks++;
            if (late && !accepted) {
                open = true;
                throw new error.ElementClickInterceptedError();
            }
            assert.equal(open, false, 'the project must not be clicked through a modal');
        }}),
        wait: async condition => {
            for (let i = 0; i < 10; i++) {
                const result = typeof condition === 'function' ? await condition(driver) : await condition.fn(driver);
                if (result) {
                    return result;
                }
            }
            throw new Error('condition never completed');
        }
    };
    return {driver, state: () => ({open, accepted, projectClicks})};
}

test('waits for trust acceptance and dialog removal before returning', async () => {
    const f = fixture();
    assert.equal(await dismissTrustDialogue(f.driver), true);
    assert.deepEqual(f.state(), {open: false, accepted: true, projectClicks: 0});
});

test('handles a trust prompt that appears between checking and clicking', async () => {
    const f = fixture({late: true});
    await clickWithWorkspaceTrust(f.driver, {});
    assert.deepEqual(f.state(), {open: false, accepted: true, projectClicks: 2});
});

test('does not accept an unrelated editor confirmation', async () => {
    const f = fixture({unexpected: true});
    await assert.rejects(dismissTrustDialogue(f.driver), /Unexpected editor dialog: An unrelated confirmation/);
    assert.equal(f.state().accepted, false);
});
