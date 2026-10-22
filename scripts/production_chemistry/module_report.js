/* Offline presentation and comments; all chemistry is supplied by Python. */
(() => {
  'use strict';
  const data = JSON.parse(document.getElementById('report-data').textContent);
  const changes = JSON.parse(document.getElementById('report-changes').textContent);
  const el = id => document.getElementById(id);
  const format = value => (value !== 0 && Math.abs(value) < 0.05 ?
    value.toExponential(1) : value.toFixed(1)).replace('-', '−');
  const namespace = 'http://www.w3.org/2000/svg';
  const node = (tag, attributes, text = '') => {
    const item = document.createElementNS(namespace, tag);
    Object.entries(attributes).forEach(([key, value]) => item.setAttribute(key, value));
    item.textContent = text;
    return item;
  };
  const option = (select, value, label) => {
    const item = document.createElement('option');
    item.value = value;
    item.textContent = label;
    select.appendChild(item);
  };
  data.families.forEach(family => option(el('mineral'), family,
    family.charAt(0).toUpperCase() + family.slice(1)));

  function chart() {
    const family = el('mineral').value;
    const rows = data.profile;
    const values = rows.map(row => row[`si_${family}`]);
    const xmin = Math.min(...rows.map(row => row.p_psia));
    const xmax = Math.max(...rows.map(row => row.p_psia));
    const ymin = Math.min(data.criterion, ...values) - 0.3;
    const ymax = Math.max(data.criterion, ...values) + 0.3;
    const x = pressure => 70 + 530 * (pressure - xmin) / (xmax - xmin);
    const y = index => 290 - 250 * (index - ymin) / (ymax - ymin);
    const svg = node('svg', {viewBox: '0 0 650 355', role: 'img',
      'aria-label': `${family} saturation index versus absolute pressure`});
    for (let tick = 0; tick <= 4; tick++) {
      const pressure = xmin + tick * (xmax - xmin) / 4;
      const index = ymin + tick * (ymax - ymin) / 4;
      svg.appendChild(node('line', {x1: 70, x2: 600, y1: y(index), y2: y(index),
        stroke: 'var(--hairline)'}));
      svg.appendChild(node('text', {x: 60, y: y(index) + 4, 'text-anchor': 'end'}, format(index)));
      svg.appendChild(node('text', {x: x(pressure), y: 315, 'text-anchor': 'middle'}, format(pressure)));
    }
    svg.appendChild(node('line', {x1: 70, x2: 600, y1: y(data.criterion), y2: y(data.criterion),
      stroke: 'var(--proj)', 'stroke-width': 2, 'stroke-dasharray': '6 4'}));
    svg.appendChild(node('text', {x: 600, y: y(data.criterion) - 8, 'text-anchor': 'end'},
      `Saturation boundary SI = ${format(data.criterion)} [2, 4]`));
    svg.appendChild(node('polyline', {points: rows.map((row, i) => `${x(row.p_psia)},${y(values[i])}`).join(' '),
      fill: 'none', stroke: 'var(--cfd)', 'stroke-width': 2}));
    rows.forEach((row, i) => {
      const detail = `Synthetic production water; ${family}; pressure ${format(row.p_psia)} psia; ` +
        `temperature ${format(row.t_f)} °F; demand SI ${format(values[i])}; ` +
        `limit SI ${format(data.criterion)}; indices dimensionless. Source: [1, 2, 4].`;
      const point = node('circle', {cx: x(row.p_psia), cy: y(values[i]), r: 5,
        fill: 'var(--cfd)', tabindex: 0, class: 'point', 'aria-label': detail});
      point.appendChild(node('title', {}, detail));
      point.addEventListener('mouseenter', () => { el('chart-detail').textContent = detail; });
      point.addEventListener('focus', () => { el('chart-detail').textContent = detail; });
      svg.appendChild(point);
    });
    svg.appendChild(node('text', {x: 335, y: 345, 'text-anchor': 'middle'}, 'Absolute pressure (psia)'));
    svg.appendChild(node('text', {x: 18, y: 175, 'text-anchor': 'middle',
      transform: 'rotate(-90 18 175)'}, 'Saturation index (dimensionless)'));
    el('profile-chart').replaceChildren(svg);
    el('chart-detail').textContent = 'Hover over a point or select it with the keyboard to read its conditions.';
  }
  el('mineral').addEventListener('change', chart);
  chart();

  changes.revisions.forEach(revision => {
    const paragraph = document.createElement('p');
    paragraph.textContent = `Revision ${revision.revision} · ${revision.date}`;
    el('changes').appendChild(paragraph);
    const list = document.createElement('ul');
    revision.changes.forEach(change => {
      const item = document.createElement('li');
      item.textContent = change;
      list.appendChild(item);
    });
    el('changes').appendChild(list);
  });

  const sections = Array.from(document.querySelectorAll('main > section'));
  sections.forEach(section => option(el('comment-section'), section.id,
    section.querySelector('h2').textContent));
  let comments = [];
  const message = text => { el('comment-message').textContent = text; };
  const renderComments = () => {
    el('comment-list').replaceChildren();
    comments.forEach(comment => {
      const item = document.createElement('li');
      item.textContent = `${comment.section}: ${comment.quote ? `“${comment.quote}” — ` : ''}${comment.text}`;
      el('comment-list').appendChild(item);
    });
  };
  const envelope = () => ({report: changes.report, revision: changes.revision,
    document_digest: data.document_digest, comments});
  function validate(incoming) {
    if (!incoming || incoming.report !== changes.report || incoming.revision !== changes.revision ||
        incoming.document_digest !== data.document_digest || !Array.isArray(incoming.comments))
      throw new Error('Comments do not match this report content and revision.');
    if (incoming.comments.some(comment => !comment ||
        !sections.some(section => section.id === comment.section) ||
        typeof comment.text !== 'string' || !comment.text.trim() || comment.text.length > 10000 ||
        typeof comment.quote !== 'string' || comment.quote.length > 10000))
      throw new Error('Invalid section or comment text.');
  }
  el('capture-quote').addEventListener('click', () => {
    const selection = window.getSelection();
    if (!selection || !selection.rangeCount) return message('Select report text first.');
    const anchor = selection.anchorNode;
    const section = (anchor.nodeType === 1 ? anchor : anchor.parentElement).closest('section');
    if (!section || !sections.includes(section)) return message('Select text inside a report section.');
    el('comment-section').value = section.id;
    el('comment-quote').value = selection.toString().slice(0, 10000);
    message('Selected text captured.');
  });
  el('add-comment').addEventListener('click', () => {
    const text = el('comment-text').value.trim();
    if (!text) return message('Enter a comment before adding it.');
    comments.push({section: el('comment-section').value, quote: el('comment-quote').value, text});
    el('comment-text').value = '';
    el('comment-quote').value = '';
    renderComments();
    message('Comment added. Save the comments file to retain it after closing.');
  });
  el('save-comments').addEventListener('click', async () => {
    try {
      validate(envelope());
      const text = JSON.stringify(envelope(), null, 2) + '\n';
      if (typeof window.showSaveFilePicker === 'function') {
        const handle = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
          types: [{description: 'Document comments', accept: {'application/json': ['.json']}}]});
        const stream = await handle.createWritable();
        await stream.write(text);
        await stream.close();
        if (await (await handle.getFile()).text() !== text) throw new Error('Saved comments differ on readback.');
        message('Comments saved and read back. Retain the file beside the HTML.');
      } else {
        const url = URL.createObjectURL(new Blob([text], {type: 'application/json'}));
        const link = document.createElement('a');
        link.href = url;
        link.download = 'report.comments.json';
        link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        message('Download requested. Place the file beside the report and load it to check the saved copy.');
      }
    } catch (error) { message(error.message); }
  });
  el('load-comments').addEventListener('change', async () => {
    try {
      const file = el('load-comments').files[0];
      if (!file) return;
      if (file.size > 1000000) throw new Error('Comments file is too large.');
      const incoming = JSON.parse(await file.text());
      validate(incoming);
      // Preserve unsaved local comments when importing another saved copy.
      const unique = new Map([...comments, ...incoming.comments].map(comment => [JSON.stringify(comment), comment]));
      comments = Array.from(unique.values());
      renderComments();
      message('Matching comments loaded. Save to retain the combined comments.');
    } catch (error) { message(error.message); }
    el('load-comments').value = '';
  });
})();
