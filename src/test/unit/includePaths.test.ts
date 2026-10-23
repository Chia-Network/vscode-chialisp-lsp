import {strict as assert} from 'node:assert';
import {test} from 'node:test';
import {addIncludePath} from '../../utils';

test('selecting an existing include directory preserves serializable configuration', () => {
    const original = {include_paths: ['./includes'], other: 'preserved'};
    const updated = addIncludePath(original, './includes');
    assert.deepEqual(JSON.parse(JSON.stringify(updated)), original);
    assert.deepEqual(updated.include_paths, ['./includes']);
});

test('adding an include directory preserves existing fields and removes no paths', () => {
    const updated = addIncludePath({include_paths: ['./old'], other: 3}, './new');
    assert.deepEqual(updated, {include_paths: ['./old', './new'], other: 3});
});
