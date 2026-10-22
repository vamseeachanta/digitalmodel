/* Exercise the shipped offline controls without opening a page or a network. */
'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const root = path.resolve(__dirname, '../..');
const page = fs.readFileSync(path.join(root,
  'docs/reports/modules/raospectralfatigue/index.html'), 'utf8');
const data = JSON.parse(page.match(/<script id="report-data" type="application\/json">(.*?)<\/script>/s)[1]);
const elements = new Map();
function element(id) {
  if (!elements.has(id)) elements.set(id, {
    value: '', textContent: '', innerHTML: '', listeners: {}, files: [],
    addEventListener(type, callback) { this.listeners[type] = callback; },
  });
  return elements.get(id);
}
element('report-data').textContent = JSON.stringify(data);
element('comment-section').value = 's5';
element('spectrum-case').value = '0:0';
let downloaded = '';
const document = { getElementById: element, createElement: () => ({
  click() { downloaded = this.download; },
}) };
let saved = '';
let existing = '';
const window = { showDirectoryPicker: async () => ({
  getFileHandle: async name => {
    assert.equal(name, 'index.comments.json');
    return {getFile: async () => ({text: async () => existing}),
      createWritable: async () => ({write: async text => { saved = text; existing = text; },
        close: async () => {}})};
  },
}) };
const context = {document, window, console, Blob, Set, Map, URL: {
  createObjectURL: () => 'blob:report-comments', revokeObjectURL: () => {},
}};
vm.runInNewContext(fs.readFileSync(path.join(root,
  'scripts/reporting/rao_spectral_fatigue_ui.js'), 'utf8'), context);

async function main() {
  assert.match(element('life-chart').innerHTML, /Required life/);
  assert.match(element('life-chart').innerHTML, /tabindex="0"/);
  element('spectrum-case').value = '1:0';
  element('spectrum-case').listeners.change();
  assert.match(element('spectrum-chart').innerHTML, /Keel-joint detail/);
  const tooltip = {getAttribute: () => 'Keel-joint detail; demand 12; limit not established; units MPa²/Hz'};
  element('spectrum-chart').listeners.focusin({target: {closest: () => tooltip}});
  assert.match(element('chart-readout').textContent, /limit not established/);

  element('comment-text').value = '<img src=x onerror=alert(1)>';
  element('add-comment').listeners.click();
  assert.match(element('comment-list').textContent, /<img/);
  await element('save-comments').listeners.click();
  const comments = JSON.parse(saved);
  assert.equal(comments.comments.length, 1);
  assert.equal(comments.content_id, data.content_id);
  assert.match(element('comment-status').textContent, /selected folder/);

  existing = JSON.stringify({...comments, comments: [...comments.comments,
    {section: 's8', text: 'Preserved existing comment'}]});
  await element('save-comments').listeners.click();
  assert.equal(JSON.parse(saved).comments.length, 2, 'Existing adjacent comments must survive saving');

  const previous = saved;
  element('load-comments').files = [{text: async () => previous}];
  await element('load-comments').listeners.change();
  await element('save-comments').listeners.click();
  assert.equal(JSON.parse(saved).comments.length, 2, 'Repeated import must not duplicate comments');
  const wrong = {...comments, content_id: 'different'};
  element('load-comments').files = [{text: async () => JSON.stringify(wrong)}];
  await element('load-comments').listeners.change();
  assert.match(element('comment-status').textContent, /different report/);
  const invalid = {...comments, comments: [{section: 'missing', text: 'comment'}]};
  element('load-comments').files = [{text: async () => JSON.stringify(invalid)}];
  await element('load-comments').listeners.change();
  assert.match(element('comment-status').textContent, /Invalid comment/);
  existing = JSON.stringify({...comments, content_id: 'another-document'});
  const retained = saved;
  await element('save-comments').listeners.click();
  assert.equal(saved, retained, 'A different report sidecar must not be overwritten');
  assert.match(element('comment-status').textContent, /not saved/);
  window.showDirectoryPicker = async () => {throw new Error('cancelled');};
  await element('save-comments').listeners.click();
  assert.match(element('comment-status').textContent, /not saved/);
  delete window.showDirectoryPicker;
  await element('save-comments').listeners.click();
  assert.equal(downloaded, 'index.comments.json');
  assert.match(element('comment-status').textContent, /Download requested/);
  console.log('Offline charts, keyboard readout, adjacent save, import binding and cancellation passed');
}
main().catch(error => {console.error(error); process.exitCode = 1;});
