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
for (const [, attrs, code] of scripts) {
  const id = /id="([^"]+)"/.exec(attrs)?.[1];
  if (id) el(id).textContent = code;
}
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
const folder = {
  async getFileHandle(name) {
    if (name === 'report.html') return {getFile:async()=>({arrayBuffer:async()=>reportBytes})};
    assert.equal(name, 'report.comments.json');
    return {
      createWritable:async()=>({write:async data=>{saved=data;},close:async()=>{}}),
      getFile:async()=>({text:async()=>saved}),
    };
  },
};
const context = vm.createContext({
  document, crypto:webcrypto, TextDecoder, TextEncoder, Blob,
  DOMParser:class {parseFromString(){return {getElementById:el};}},
  window:{showDirectoryPicker:async()=>folder},
  URL:{createObjectURL:()=> 'blob:comments',revokeObjectURL:()=>{}},
  setTimeout:fn=>fn(),
});
for (const [, attrs, code] of scripts)
  if (!attrs.includes('application/json')) vm.runInContext(code, context);

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
  assert.equal(JSON.parse(saved).comments.length,2);
  const previous = saved;
  el('comment-load').files=[{text:async()=>JSON.stringify({...persisted,calculation:'different'})}];
  await el('comment-load').onchange();
  assert.ok(el('comment-status').textContent.includes('does not match'));
  reportBytes=Buffer.from('changed report');
  await el('comment-save').onclick();
  assert.ok(el('comment-status').textContent.includes('Report changed'));
  assert.equal(saved,previous);
}
main().catch(error=>{console.error(error.message);process.exitCode=1;});
