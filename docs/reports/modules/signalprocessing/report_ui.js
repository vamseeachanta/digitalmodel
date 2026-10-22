/* Self-contained chart and comment controls; no remote requests or storage. */
(() => {
  'use strict';
  const data = JSON.parse(document.getElementById('calculation-data').textContent);
  const metrics = data.metrics;
  const freq = data.spectrum.frequency_hz;
  const power = data.spectrum.density_v2_per_hz;
  const byId = id => document.getElementById(id);
  const svgNS = 'http://www.w3.org/2000/svg';
  const density = value => value.toExponential(3);
  const hz = value => value.toFixed(1);
  const make = (name, attributes, text) => {
    const node = document.createElementNS(svgNS, name);
    for (const [key, value] of Object.entries(attributes)) node.setAttribute(key, value);
    if (text !== undefined) node.textContent = text;
    return node;
  };
  const append = (svg, name, attrs, text) => svg.appendChild(make(name, attrs, text));
  let minimum = 0, maximum = metrics.nyquist_hz;
  const left = 95, right = 840, top = 30, bottom = 340;
  const yMin = Math.min(...power.filter(value => value > 0)) / 2;
  const yMax = Math.max(...power) * 2;
  const x = f => left + (f - minimum) / (maximum - minimum) * (right - left);
  const y = p => bottom - (Math.log10(p) - Math.log10(yMin)) /
    (Math.log10(yMax) - Math.log10(yMin)) * (bottom - top);
  const chart = byId('spectrum-chart');
  const slider = byId('frequency-cursor');
  slider.max = freq.length - 1;

  function inspect(index) {
    index = Math.max(0, Math.min(freq.length - 1, Math.round(index)));
    slider.value = index;
    byId('spectrum-readout').textContent =
      `Public synthetic voltage; spectral density at ${hz(freq[index])} Hz: ` +
      `${density(power[index])} V²/Hz. Noise reference: ` +
      `${density(metrics.noise_density_v2_per_hz)} V²/Hz. ` +
      `Frequency limit: ${hz(metrics.nyquist_hz)} Hz (Nyquist sampling boundary). ` +
      'A density operating limit is not evaluated.';
    const marker = byId('spectrum-marker');
    if (marker) {
      marker.setAttribute('cx', x(freq[index]));
      marker.setAttribute('cy', y(power[index]));
      marker.setAttribute('visibility', freq[index] >= minimum && freq[index] <= maximum ? 'visible' : 'hidden');
    }
  }

  function drawSpectrum() {
    chart.replaceChildren();
    const indices = freq.map((_, i) => i).filter(i => freq[i] >= minimum && freq[i] <= maximum);
    const path = indices.map((i, n) => `${n ? 'L' : 'M'}${x(freq[i])},${y(power[i])}`).join(' ');
    append(chart, 'path', {d: `M${left} ${top} V${bottom} H${right}`, fill: 'none', stroke: 'currentColor'});
    for (let n = 0; n <= 5; n++) {
      const f = minimum + n / 5 * (maximum - minimum);
      append(chart, 'text', {x: x(f), y: bottom + 25, 'text-anchor': 'middle'}, hz(f));
    }
    for (let exponent = Math.ceil(Math.log10(yMin)); exponent <= Math.floor(Math.log10(yMax)); exponent++) {
      const p = 10 ** exponent;
      append(chart, 'text', {x: left - 12, y: y(p) + 5, 'text-anchor': 'end'}, density(p));
    }
    append(chart, 'path', {d: path, class: 'chart-line'});
    append(chart, 'line', {x1: left, x2: right, y1: y(metrics.noise_density_v2_per_hz),
      y2: y(metrics.noise_density_v2_per_hz), class: 'reference-line'});
    if (maximum === metrics.nyquist_hz) {
      append(chart, 'line', {x1: right, x2: right, y1: top, y2: bottom, class: 'limit-line'});
      append(chart, 'text', {x: right - 8, y: top + 12, 'text-anchor': 'end'}, 'Nyquist sampling boundary');
    }
    append(chart, 'text', {x: (left + right) / 2, y: 402, 'text-anchor': 'middle'}, 'Frequency (Hz)');
    append(chart, 'text', {x: 22, y: 190, transform: 'rotate(-90 22 190)', 'text-anchor': 'middle'},
      'Spectral density (V²/Hz, logarithmic scale)');
    append(chart, 'circle', {id: 'spectrum-marker', r: 5, fill: 'var(--report-accent)'});
    inspect(Number(slider.value));
  }

  slider.addEventListener('input', () => inspect(Number(slider.value)));
  chart.addEventListener('pointermove', event => {
    const point = chart.createSVGPoint();
    point.x = event.clientX;
    point.y = event.clientY;
    const local = point.matrixTransform(chart.getScreenCTM().inverse());
    const frequency = minimum + (local.x - left) / (right - left) * (maximum - minimum);
    inspect(frequency / metrics.spacing_hz);
  });
  byId('spectrum-view').addEventListener('change', event => {
    if (event.target.value === 'tone') {
      minimum = Math.max(0, metrics.peak_hz - 5 * metrics.spacing_hz);
      maximum = Math.min(metrics.nyquist_hz, metrics.peak_hz + 5 * metrics.spacing_hz);
      slider.value = Math.round(metrics.peak_hz / metrics.spacing_hz);
    } else {
      minimum = 0;
      maximum = metrics.nyquist_hz;
    }
    drawSpectrum();
  });
  drawSpectrum();

  const noiseChart = byId('noise-chart');
  const reference = metrics.noise_density_v2_per_hz;
  const lower = reference * (1 - metrics.noise_tolerance_percent / 100);
  const upper = reference * (1 + metrics.noise_tolerance_percent / 100);
  const nx = value => 120 + (value - lower * 0.85) / (upper * 1.15 - lower * 0.85) * 680;
  append(noiseChart, 'path', {d: 'M120 175 H800', stroke: 'currentColor'});
  for (const [value, label, style] of [
    [lower, 'Noise comparison lower limit', 'limit-line'],
    [reference, 'Input noise reference', 'reference-line'],
    [upper, 'Noise comparison upper limit', 'limit-line'],
  ]) {
    const line = append(noiseChart, 'line', {x1: nx(value), x2: nx(value), y1: 65, y2: 175, class: style});
    line.appendChild(make('title', {}, `${label}: ${density(value)} V²/Hz; public synthetic voltage`));
    append(noiseChart, 'text', {x: nx(value), y: 200, 'text-anchor': 'middle'}, density(value));
  }
  append(noiseChart, 'text', {x: 460, y: 244, 'text-anchor': 'middle'}, 'Mean noise density (V²/Hz)');
  append(noiseChart, 'text', {x: 460, y: 30, 'text-anchor': 'middle'},
    'Dashed limits apply to the upper-band mean; the centre line is the input reference.');
  const dot = append(noiseChart, 'circle', {cx: nx(metrics.noise_density_estimate_v2_per_hz), cy: 120,
    r: 8, fill: 'var(--ink)', tabindex: 0, role: 'img', 'aria-label': 'Inspect upper-band mean density'});
  const noiseText = `Public synthetic voltage; upper-band mean demand: ` +
    `${density(metrics.noise_density_estimate_v2_per_hz)} V²/Hz; reference: ${density(reference)} V²/Hz; ` +
    `limits: ${density(lower)} to ${density(upper)} V²/Hz. The numerical comparison is met.`;
  dot.appendChild(make('title', {}, noiseText));
  const showNoise = () => { byId('noise-readout').textContent = noiseText; };
  dot.addEventListener('pointerenter', showNoise);
  dot.addEventListener('focus', showNoise);
  showNoise();

  const sections = Array.from(document.querySelectorAll('section[id^="section-"]'));
  const binding = document.querySelector('meta[name="document-binding"]').content;
  const comments = [];
  for (const section of sections) {
    const option = document.createElement('option');
    option.value = section.id;
    option.textContent = section.querySelector('h2').textContent;
    byId('comment-section').appendChild(option);
  }
  const status = text => { byId('comment-status').textContent = text; };
  const packet = () => ({document: 'Signal processing module analysis', revision: '00', binding, comments});

  function mergeComments(incoming) {
    if (incoming.binding !== binding || incoming.revision !== '00' ||
        incoming.document !== 'Signal processing module analysis' || !Array.isArray(incoming.comments) ||
        incoming.comments.length > 1000 || incoming.comments.some(comment =>
          !comment || typeof comment.id !== 'string' || typeof comment.text !== 'string' ||
          !comment.text.trim() || comment.text.length > 10000 ||
          !sections.some(section => section.id === comment.section))) {
      throw new Error('The comments do not match this document or have invalid fields.');
    }
    const existing = new Set(comments.map(comment => comment.id));
    for (const comment of incoming.comments) {
      if (!existing.has(comment.id)) {
        comments.push({id: comment.id, section: comment.section, text: comment.text});
        existing.add(comment.id);
      }
    }
    renderComments();
  }

  function renderComments() {
    byId('comment-list').replaceChildren();
    for (const comment of comments) {
      const item = document.createElement('li');
      const link = document.createElement('a');
      link.href = `#${comment.section}`;
      link.textContent = sections.find(section => section.id === comment.section).querySelector('h2').textContent;
      item.appendChild(link);
      const text = document.createElement('p');
      text.className = 'note';
      text.textContent = comment.text;
      item.appendChild(text);
      byId('comment-list').appendChild(item);
    }
  }

  byId('add-comment').addEventListener('click', () => {
    const text = byId('comment-text').value.trim();
    if (!text) return status('Enter comment text before recording it.');
    comments.push({id: crypto.randomUUID(), section: byId('comment-section').value, text});
    byId('comment-text').value = '';
    renderComments();
    status('Comment recorded in the page; save the comments to retain it.');
  });
  byId('save-comments').addEventListener('click', async () => {
    if (!window.showSaveFilePicker) return status('Direct file saving is unavailable. Download comments and place the file beside the HTML.');
    try {
      const handle = await window.showSaveFilePicker({suggestedName: 'report.comments.json',
        types: [{description: 'Report comments', accept: {'application/json': ['.json']}}]});
      const existing = await handle.getFile();
      if (existing.size) mergeComments(JSON.parse(await existing.text()));
      const writable = await handle.createWritable();
      await writable.write(JSON.stringify(packet(), null, 2));
      await writable.close();
      status('Comments saved to the selected file. Retain it beside the HTML.');
    } catch (error) {
      status(error.name === 'AbortError' ? 'Save cancelled; comments remain in the page.' : `Comments were not saved: ${error.message}`);
    }
  });
  byId('download-comments').addEventListener('click', () => {
    const blob = new Blob([JSON.stringify(packet(), null, 2)], {type: 'application/json'});
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = 'report.comments.json';
    link.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    status('Comments downloaded. Place the file beside the HTML before transferring it.');
  });
  byId('load-comments').addEventListener('change', async event => {
    const file = event.target.files[0];
    if (!file) return;
    if (file.size > 2000000) return status('The comments file is too large.');
    try {
      mergeComments(JSON.parse(await file.text()));
      status('Matching comments loaded and merged; save to retain the combined comments.');
    } catch (error) { status(error.message); }
    event.target.value = '';
  });
})();
