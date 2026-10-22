/* Regression checks for the offline report script, using the Node test runner. */
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const test = require('node:test');
const vm = require('node:vm');

class Element {
  constructor(tag) {
    this.tag = tag;
    this.children = [];
    this.attributes = {};
    this.handlers = {};
    this.value = '';
    this.textContent = '';
  }
  setAttribute(key, value) { this.attributes[key] = value; }
  appendChild(child) {
    this.children.push(child);
    if (child.tag === 'option' && !this.value) this.value = child.value;
  }
  replaceChildren(...children) { this.children = children; }
  addEventListener(event, handler) { this.handlers[event] = handler; }
}

test('negative scientific indices use a true minus for both signs', () => {
  const elements = new Map();
  const el = id => {
    if (!elements.has(id)) elements.set(id, new Element('div'));
    return elements.get(id);
  };
  el('report-data').textContent = JSON.stringify({
    families: ['calcite'], criterion: 0,
    profile: [
      {p_psia: 100, t_f: 90, si_calcite: -0.001},
      {p_psia: 200, t_f: 110, si_calcite: 0.03},
    ],
  });
  el('report-changes').textContent = JSON.stringify({revisions: []});
  const document = {
    getElementById: el, querySelectorAll: () => [],
    createElement: tag => new Element(tag),
    createElementNS: (_namespace, tag) => new Element(tag),
  };
  const script = path.resolve(__dirname, '../../scripts/production_chemistry/module_report.js');
  vm.runInNewContext(fs.readFileSync(script, 'utf8'), {document});
  const svg = el('profile-chart').children[0];
  const points = svg.children.filter(item => item.tag === 'circle');
  assert.equal(points.length, 2);
  assert.match(points[0].attributes['aria-label'], /demand SI −1\.0e−3;/);
  assert.match(points[1].attributes['aria-label'], /demand SI 3\.0e−2;/);
  points[0].handlers.focus();
  assert.match(el('chart-detail').textContent, /demand SI −1\.0e−3;/);
});
