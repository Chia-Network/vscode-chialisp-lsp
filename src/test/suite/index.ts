// Mocha 12+ is ESM-only at its main entry. Import the bundled CJS module
// directly so the test runner works under Node16 CJS resolution.
// eslint-disable-next-line @typescript-eslint/no-require-imports
const Mocha = require('mocha/lib/mocha.cjs');
import {glob} from 'glob';
import * as path from 'path';

export function run(): Promise<void> {
	// Create the mocha test
	const mocha = new Mocha({
		ui: 'tdd',
		color: true
	});

	const testsRoot = __dirname;

	return new Promise((c, e) => {
		glob('**/**.test.js', { cwd: testsRoot }).then((files: string[]) => {
			// Add files to the test suite
			files.forEach((f: string) => mocha.addFile(path.resolve(testsRoot, f)));

			try {
				// Run the mocha test
				mocha.run((failures: number) => {
					if (failures > 0) {
						e(new Error(`${failures} tests failed.`));
					} else {
						c();
					}
				});
			} catch (err) {
				console.error(err);
				e(err);
			}
		});
	});
}
