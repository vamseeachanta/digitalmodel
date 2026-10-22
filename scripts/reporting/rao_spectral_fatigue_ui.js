/* Offline numerical charts and content-bound comment transfer. */
(() => {
  'use strict';
  const el = id => document.getElementById(id);
  const data = JSON.parse(el('report-data').textContent);
  const example = data.example;
  const sections = new Set(Array.from({length: 10}, (_, i) => `s${i + 1}`));
  const comments = [];
  const esc = text => String(text).replace(/[&<>"']/g, c =>
    ({'&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'}[c]));
  const number = n => Math.abs(n) > 0 && Math.abs(n) < 0.1 ?
    n.toPrecision(3) : n.toFixed(1);
  const message = text => { el('comment-status').textContent = text; };

  function chart(id, series, {xLabel, yLabel, units, limit = null, categories = null}) {
    const points = series.flatMap(s => s.x.map((x, i) => [x, s.y[i]]));
    const xmin = Math.min(...points.map(p => p[0]));
    const xmax = Math.max(...points.map(p => p[0]));
    const ymax = Math.max(...points.map(p => p[1]), limit || 0) * 1.15;
    const x = v => 90 + (v - xmin) / (xmax - xmin || 1) * 590;
    const y = v => 280 - v / (ymax || 1) * 220;
    const palette = ['var(--brand)', 'var(--nogo)', 'var(--accent)'];
    let svg = `<svg viewBox="0 0 760 370" role="img" aria-label="${esc(yLabel)} against ${esc(xLabel)}" style="width:100%">`;
    svg += '<path d="M90 55V280H700" fill="none" stroke="var(--mut)"/>';
    for (let i = 0; i <= 4; i++) {
      const v = ymax * i / 4;
      svg += `<text x="82" y="${y(v) + 5}" text-anchor="end">${number(v)}</text>`;
    }
    if (limit !== null) {
      svg += `<path d="M90 ${y(limit)}H700" stroke="var(--nogo)" stroke-dasharray="6 4"/>`;
      svg += `<text x="100" y="${y(limit) - 8}">Required life: ${number(limit)} ${esc(units)}</text>`;
    }
    for (const [index, seriesItem] of series.entries()) {
      const color = palette[index % palette.length];
      const coords = seriesItem.x.map((v, i) => `${x(v)},${y(seriesItem.y[i])}`).join(' ');
      svg += `<polyline points="${coords}" fill="none" stroke="${color}" stroke-width="2"/>`;
      for (let i = 0; i < seriesItem.x.length; i++) {
        const variable = categories ? categories[i] : `${number(seriesItem.x[i])} Hz`;
        const tip = `${seriesItem.label}; ${variable}; demand ${number(seriesItem.y[i])}; ` +
          `limit ${limit === null ? 'not established' : number(limit)}; units ${units}`;
        svg += `<circle cx="${x(seriesItem.x[i])}" cy="${y(seriesItem.y[i])}" r="4" ` +
          `fill="${color}" tabindex="0" data-tooltip="${esc(tip)}" aria-label="${esc(tip)}"><title>${esc(tip)}</title></circle>`;
      }
      svg += `<text x="100" y="${22 + index * 18}" fill="${color}">${esc(seriesItem.label)}</text>`;
    }
    if (categories) {
      for (let i = 0; i < categories.length; i++)
        svg += `<text x="${x(i)}" y="305" text-anchor="middle">${esc(categories[i])}</text>`;
    } else {
      for (let i = 0; i <= 4; i++) {
        const v = xmin + (xmax - xmin) * i / 4;
        svg += `<text x="${x(v)}" y="305" text-anchor="middle">${number(v)}</text>`;
      }
    }
    svg += `<text x="390" y="350" text-anchor="middle">${esc(xLabel)}</text>`;
    svg += `<text transform="translate(18 180) rotate(-90)" text-anchor="middle">${esc(yLabel)}</text></svg>`;
    el(id).innerHTML = svg;
    const readout = event => {
      const point = event.target.closest('[data-tooltip]');
      if (point) el('chart-readout').textContent = point.getAttribute('data-tooltip');
    };
    el(id).addEventListener('pointermove', readout);
    el(id).addEventListener('focusin', readout);
    el(id).addEventListener('click', readout);
  }

  chart('life-chart', [{label: 'Screening arithmetic',
    x: example.locations.map((_, i) => i),
    y: example.locations.map(location => location.fatigue_life_years)}], {
    xLabel: 'Structural detail', yLabel: 'Screening life (years)', units: 'years',
    limit: example.locations[0].required_life_years,
    categories: example.locations.map(location => location.label),
  });
  chart('rao-chart', example.locations.map(location => ({label: location.label,
    x: location.rao_frequency_Hz, y: location.rao_stress_MPa_per_m})), {
    xLabel: 'Cyclic frequency (Hz)', yLabel: 'Stress transfer (MPa/m)', units: 'MPa/m',
  });
  el('spectrum-case').innerHTML = example.locations.map((location, i) =>
    location.sea_states.map((state, j) => `<option value="${i}:${j}">${esc(location.label)}; ${esc(state.label.toLowerCase())}</option>`).join('')).join('');
  function spectrum() {
    const [i, j] = (el('spectrum-case').value || '0:0').split(':').map(Number);
    const location = example.locations[i], state = location.sea_states[j];
    chart('spectrum-chart', [{label: `${location.label}; ${state.label.toLowerCase()}`,
      x: state.frequency_Hz, y: state.stress_psd_MPa2_Hz}], {
      xLabel: 'Cyclic frequency (Hz)', yLabel: 'Stress density (MPa²/Hz)', units: 'MPa²/Hz',
    });
  }
  el('spectrum-case').addEventListener('change', spectrum);
  spectrum();

  el('change-panel').textContent = data.changes.changes.map(change =>
    `${change.section}: ${change.description}`).join('\n');
  el('change-panel').style && (el('change-panel').style.whiteSpace = 'pre-wrap');
  const showComments = () => {
    el('comment-list').textContent = comments.map(comment =>
      `${comment.section}: ${comment.text}`).join('\n\n');
  };
  function mergeComments(incoming) {
    if (incoming.report !== 'rao-spectral-fatigue' || incoming.revision !== data.changes.revision ||
        incoming.content_id !== data.content_id)
      throw new Error('Comments belong to a different report content or revision.');
    if (!Array.isArray(incoming.comments) || incoming.comments.some(comment =>
      !comment || !sections.has(comment.section) || typeof comment.text !== 'string' || !comment.text.trim()))
      throw new Error('Invalid comment data.');
    const existing = new Set(comments.map(comment => JSON.stringify(comment)));
    for (const comment of incoming.comments) {
      const normalized = {section: comment.section, text: comment.text};
      if (!existing.has(JSON.stringify(normalized))) {
        comments.push(normalized); existing.add(JSON.stringify(normalized));
      }
    }
    showComments();
  }
  el('add-comment').addEventListener('click', () => {
    const text = el('comment-text').value.trim(), section = el('comment-section').value;
    if (!text || !sections.has(section)) return message('Select a section and enter a comment.');
    comments.push({section, text});
    el('comment-text').value = '';
    showComments();
    message('Comment added; save comments before closing the document.');
  });
  el('save-comments').addEventListener('click', async () => {
    const serialize = () => JSON.stringify({report: 'rao-spectral-fatigue', revision: data.changes.revision,
      content_id: data.content_id, comments}, null, 2);
    try {
      if (window.showDirectoryPicker) {
        const folder = await window.showDirectoryPicker({mode: 'readwrite'});
        const file = await folder.getFileHandle('index.comments.json', {create: true});
        const previous = await (await file.getFile()).text();
        if (previous.trim()) mergeComments(JSON.parse(previous));
        const stream = await file.createWritable();
        await stream.write(serialize());
        await stream.close();
        message('Comments saved as index.comments.json in the selected folder.');
      } else {
        const url = URL.createObjectURL(new Blob([serialize()], {type: 'application/json'}));
        const link = document.createElement('a');
        link.href = url; link.download = 'index.comments.json';
        link.click(); URL.revokeObjectURL(url);
        message('Download requested; place index.comments.json beside this report.');
      }
    } catch (error) { message(`Comments were not saved: ${error.message}`); }
  });
  el('load-comments').addEventListener('change', async () => {
    try {
      const file = el('load-comments').files[0];
      if (!file) return;
      mergeComments(JSON.parse(await file.text()));
      message('Comments loaded for this report content.');
    } catch (error) { message(error.message); }
    el('load-comments').value = '';
  });
})();
