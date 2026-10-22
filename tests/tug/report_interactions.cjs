/* Execute real handlers without a browser or external packages. */
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');

class Element {
  constructor() {
    this.textContent = '';
    this.value = '';
    this.children = [];
    this.handlers = {};
  }
  append(child) { this.children.push(child); }
  replaceChildren() { this.children = []; }
  addEventListener(name, handler) { this.handlers[name] = handler; }
}
const ids = Object.fromEntries([
  'comment-section', 'comment-text', 'comment-list', 'comment-status',
  'add-comment', 'save-comments', 'load-comments', 'change-data', 'changes-panel',
].map((id) => [id, new Element()]));
ids['change-data'].textContent = JSON.stringify({changes: []});
const heading = {textContent: 'Summary'};
const section = {id: 'summary', querySelector: () => heading};
const document = {
  getElementById: (id) => ids[id],
  createElement: () => new Element(),
  querySelectorAll: (selector) => selector === 'section[id]' ? [section] : [],
};
vm.runInNewContext(fs.readFileSync(path.join(__dirname,
  '../../docs/reports/modules/tug/interactions.js'), 'utf8'), {document});

const add = (text) => {
  ids['comment-section'].value = 'summary';
  ids['comment-text'].value = text;
  ids['add-comment'].handlers.click();
};
const load = (text) => ids['load-comments'].handlers.change({target: {files: [{text}]}});
const rows = () => ids['comment-list'].children.map((item) => item.textContent);
const record = (comments) => JSON.stringify({revision: 'A', comments});
const comment = (text) => ({section: 'summary', text});

(async () => {
  add('Local unsaved');
  ids['comment-text'].value = 'Draft still being typed';
  const scenario = process.argv[2];
  if (scenario === 'merge') {
    await load(async () => record([comment('Imported <script>text</script>')]));
    assert.deepEqual(rows(), ['Summary: Local unsaved', 'Summary: Imported <script>text</script>']);
    assert.match(ids['comment-status'].textContent, /merged.*Save/);
  } else if (scenario === 'duplicate') {
    const text = record([comment('Local unsaved'), comment('Imported'), comment('Imported')]);
    await load(async () => text);
    await load(async () => text);
    assert.deepEqual(rows(), ['Summary: Local unsaved', 'Summary: Imported']);
  } else if (scenario === 'invalid') {
    for (const text of ['{', record([comment('Valid'), {section: 'missing', text: 'Invalid'}])]) {
      await load(async () => text);
      assert.deepEqual(rows(), ['Summary: Local unsaved']);
      assert.match(ids['comment-status'].textContent, /Invalid.*retained/);
    }
  } else if (scenario === 'pending') {
    let resolve;
    const pending = load(() => new Promise((done) => { resolve = done; }));
    add('Added during read');
    ids['comment-text'].value = 'Draft still being typed';
    resolve(record([comment('Imported')]));
    await pending;
    assert.deepEqual(rows(), ['Summary: Local unsaved', 'Summary: Added during read', 'Summary: Imported']);
  } else {
    throw new Error('Unknown test scenario');
  }
  assert.equal(ids['comment-text'].value, 'Draft still being typed');
})().catch((error) => { console.error(error); process.exitCode = 1; });
