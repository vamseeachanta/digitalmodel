/* Exercise offline chart and comment behavior without starting a browser. */
'use strict';
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const {webcrypto} = require('node:crypto');
const page = fs.readFileSync(process.argv[2], 'utf8');
const scripts = [...page.matchAll(/<script([^>]*)>([\s\S]*?)<\/script>/g)];
const elements = new Map();
class Element {
  constructor(tag = 'div') {
    this.tag = tag; this.attrs = {}; this.children = []; this.textContent = '';
    this.value = ''; this.files = []; this.events = {};
  }
  setAttribute(k, v) {this.attrs[k] = String(v);}
  append(...nodes) {this.children.push(...nodes);}
  appendChild(node) {this.append(node);}
  addEventListener(event, fn) {this.events[event] = fn;}
  click() {this.clicked = true;}
}
const el = id => {
  if (!elements.has(id)) elements.set(id, new Element());
  return elements.get(id);
};
const sections = [...page.matchAll(/<section id="([^"]+)"[^>]*><h[23]>([^<]+)/g)]
  .map(([,id,title]) => ({id, querySelector: () => ({textContent:title})}));
const document = {
  getElementById: el,
  querySelectorAll: () => sections,
  createElementNS: (_, tag) => new Element(tag),
  createElement: tag => new Element(tag),
};
const documentBytes = Buffer.from(page);
let reportBytes = documentBytes;
let saved = null;
let readError = null;
let writes = 0;
let downloaded = null;
let downloadBlob = null;
const folder = {
  async getFileHandle(name, options = {}) {
    if (name === 'report.html') return {getFile:async()=>({arrayBuffer:async()=>reportBytes})};
    assert.equal(name, 'report.comments.json');
    if (readError && !options.create) throw readError;
    if (saved === null && !options.create)
      throw Object.assign(new Error('No saved comments.'), {name:'NotFoundError'});
    return {
      createWritable:async()=>{writes++;return {write:async data=>{saved=data;},close:async()=>{}};},
      getFile:async()=>({text:async()=>saved}),
    };
  },
};
function reopen() {
  elements.clear();
  for (const [, attrs, code] of scripts) {
    const id = /id="([^"]+)"/.exec(attrs)?.[1];
    if (id) el(id).textContent = code;
  }
  const context = vm.createContext({
  document, crypto:webcrypto, TextDecoder, TextEncoder, Blob,
  DOMParser:class {parseFromString(text){return {getElementById:id=>{
    const match=new RegExp(`<script id="${id}" type="application/json">([\\s\\S]*?)</script>`).exec(text);
    return match ? {textContent:match[1]} : null;
  }};}},
  window:{showDirectoryPicker:async()=>folder},
  URL:{createObjectURL:blob=>{downloadBlob=blob;return 'blob:comments';},revokeObjectURL:()=>{}},
  setTimeout:fn=>fn(),
  });
  for (const [, attrs, code] of scripts)
    if (!attrs.includes('application/json')) vm.runInContext(code, context);
}
reopen();

function add(text) {
  el('comment-section').value='detailed-results';
  el('comment-text').value=text;
  el('comment-add').onclick();
}

async function savedComment() {
  add('Previously saved comment');
  await el('comment-folder').onclick();
  await el('comment-save').onclick();
  assert.ok(el('comment-status').textContent.includes('read back'));
  return JSON.parse(saved);
}

async function preserve() {
  const first = await savedComment();
  reopen();
  add('Comment after reopening');
  await el('comment-folder').onclick();
  // A comment saved after folder selection must also survive the next save.
  saved=JSON.stringify({...first,context:'Retain additional metadata',comments:[...first.comments,
    {section:'findings',text:'Added to the saved file'}]});
  await el('comment-save').onclick();
  assert.deepEqual(JSON.parse(saved).comments.map(c=>c.text), [
    'Previously saved comment','Added to the saved file','Comment after reopening',
  ], 'Saving after reopening must preserve the existing file');
  await el('comment-save').onclick();
  assert.equal(JSON.parse(saved).comments.length,3,'Repeated save must not duplicate comments');
  assert.equal(JSON.parse(saved).context,'Retain additional metadata');
}

async function fallback() {
  const first=await savedComment();
  reopen();
  el('comment-load').files=[{text:async()=>JSON.stringify({...first,context:'Retain loaded metadata',report_digest:null})}];
  await el('comment-load').onchange();
  add('Downloaded comment');
  const create=document.createElement;
  document.createElement=tag=>{const node=create(tag);node.click=()=>{downloaded=node.download;};return node;};
  await el('comment-save').onclick();
  assert.equal(downloaded,'report.comments.new.json','A download must not suggest replacing the existing file');
  assert.equal(JSON.parse(await downloadBlob.text()).comments.length,2);
  assert.equal(JSON.parse(await downloadBlob.text()).context,'Retain loaded metadata');
  assert.ok(el('comment-status').textContent.includes('new filename'));
  assert.equal(JSON.parse(saved).comments.length,1,'The saved file remains unchanged');
}

async function wrongFolder() {
  reportBytes=Buffer.from(page.replace(el('report-data').textContent,'different calculation'));
  await el('comment-folder').onclick();
  assert.ok(el('comment-status').textContent.includes('different calculation'));
  assert.equal(writes,0);
}

async function wrongRevision() {
  reportBytes=Buffer.from(page.replace(el('change-data').textContent,'{"revisions":[{"revision":"previous"}]}'));
  await el('comment-folder').onclick();
  assert.ok(el('comment-status').textContent.includes('different report revision'));
  assert.equal(writes,0);
}

async function invalid() {
  const first=await savedComment();
  const cases=[
    'broken JSON', 'null', '[]', JSON.stringify({...first,report:'different'}),
    JSON.stringify({...first,revision:'different'}),
    JSON.stringify({...first,calculation:'different'}),
    JSON.stringify({...first,report_digest:'0'.repeat(64)}),
    JSON.stringify({...first,report_digest:null}),
    JSON.stringify({...first,comments:null}),
    JSON.stringify({...first,comments:[null]}),
    JSON.stringify({...first,comments:[{section:'unknown',text:'Unmatched section'}]}),
    JSON.stringify({...first,comments:[{section:'findings',text:42}]}),
    JSON.stringify({...first,comments:[{section:'findings',text:' '}]}),
  ];
  for (const value of cases) {
    saved=value;
    const before=writes;
    await el('comment-save').onclick();
    assert.ok(saved===value,'Invalid saved comments must remain unchanged');
    assert.equal(writes,before,'Validation must happen before opening a writable stream');
  }
}

async function unreadable() {
  await savedComment();
  const previous=saved, before=writes;
  for (const name of ['NotAllowedError','NotReadableError']) {
    readError=Object.assign(new Error('Saved comments cannot be read.'),{name});
    await el('comment-save').onclick();
    assert.equal(saved,previous);
    assert.equal(writes,before,'A read failure must not be treated as a missing file');
  }
}

async function main() {
  const data = JSON.parse(el('chart-data').textContent);
  data.forEach((series, index) => {
    const svg = el(`life-chart-${index}`);
    const lines = svg.children.filter(child=>child.tag==='polyline');
    assert.equal(lines.length,2);
    assert.equal(lines[1].attrs['data-limit'],'required-life');
    assert.equal(lines[1].attrs['stroke-dasharray'],'8 5');
    const circles = svg.children.filter(child=>child.tag==='circle');
    assert.equal(circles.length,4);
    assert.ok(circles.every(circle=>circle.attrs.tabindex==='0'));
    circles[0].events.focus();
    assert.ok(el(`life-hover-${index}`).textContent.includes(series.label));
    assert.ok(el(`life-hover-${index}`).textContent.includes('years'));
    assert.ok(el(`life-hover-${index}`).textContent.includes('limit:'));
    assert.ok(el(`life-hover-${index}`).textContent.includes('demand:'));
  });
  assert.equal(el('change-list').children.length,3);
  assert.equal(el('comment-section').children.length,11);
  el('comment-section').value='detailed-results';
  el('comment-text').value='<img onerror="unexpected"> engineering comment';
  el('comment-add').onclick();
  assert.ok(el('comment-preview').textContent.includes('<img'));
  assert.equal(el('comment-preview').children.length,0);
  await el('comment-folder').onclick();
  await el('comment-save').onclick();
  assert.ok(el('comment-status').textContent.includes('read back'));
  const persisted = JSON.parse(saved);
  assert.equal(persisted.comments.length,1);
  assert.equal(persisted.comments[0].section,'detailed-results');
  assert.equal(persisted.calculation,el('report-data').textContent);
  assert.ok(persisted.report_digest);
  el('comment-load').files=[{text:async()=>saved}];
  await el('comment-load').onchange();
  await el('comment-save').onclick();
  assert.equal(JSON.parse(saved).comments.length,1);
  const previous = saved;
  el('comment-load').files=[{text:async()=>JSON.stringify({...persisted,calculation:'different'})}];
  await el('comment-load').onchange();
  assert.ok(el('comment-status').textContent.includes('does not match'));
  reportBytes=Buffer.from('changed report');
  await el('comment-save').onclick();
  assert.ok(el('comment-status').textContent.includes('Report changed'));
  assert.equal(saved,previous);
}
const run={core:main,preserve,invalid,unreadable,fallback,'wrong-folder':wrongFolder,'wrong-revision':wrongRevision}[process.argv[3] || 'core'];
run().catch(error=>{console.error(error.message);process.exitCode=1;});
