/* Offline portfolio review: bind full HTML to a separately supplied receipt. */
(() => {
  'use strict';
  const config = JSON.parse(document.getElementById('cp-review-config').textContent);
  const el = id => document.getElementById(`cp-${id}`);
  const allowed = new Set(['pending', 'accept', 'revise', 'reject']);
  const ids = new Set(config.sections.map(section => section.id));
  let state = null;
  let selectedHash = null;
  let verifiedForSave = false;
  const message = text => { el('message').textContent = text; };
  const show = () => { el('preview').textContent = JSON.stringify(state, null, 2); };
  const unique = values => [...new Map(values.map(v => [JSON.stringify(v), v])).values()];
  const sha256 = async data => [...new Uint8Array(await crypto.subtle.digest('SHA-256', data))]
    .map(v => v.toString(16).padStart(2, '0')).join('');

  for (const section of config.sections) {
    const option = document.createElement('option');
    option.value = section.id;
    option.textContent = section.title;
    el('section').appendChild(option);
  }

  function validate(incoming) {
    if (!incoming || incoming.schema_version !== 1 ||
        incoming.report_id !== config.report_id || incoming.revision !== config.revision ||
        incoming.report_sha256 !== selectedHash || !selectedHash)
      throw new Error('Report identity, revision or full HTML digest mismatch.');
    if (incoming.standard_revision !== 'c955b86159acc4f9e9f5964690c24dd1411e5b48')
      throw new Error('Unknown reporting standard revision.');
    if (!Array.isArray(incoming.results) || incoming.results.length !== ids.size ||
        new Set(incoming.results.map(row => row && row.id)).size !== ids.size ||
        incoming.results.some(row => !row || !ids.has(row.id) || !allowed.has(row.decision)))
      throw new Error('Missing, duplicate or invalid review decision.');
    for (const field of ['comments', 'prior_rounds', 'decision_conflicts'])
      if (!Array.isArray(incoming[field])) throw new Error(`Invalid ${field}.`);
    if (incoming.comments.some(c => !c || !ids.has(c.section) ||
        typeof c.text !== 'string' || typeof c.reviewer !== 'string' ||
        typeof c.id !== 'string' || typeof c.quote !== 'string' ||
        !['incorporated', 'deferred', 'requiring a decision'].includes(c.disposition) ||
        (c.disposition === 'deferred' && !c.reason)))
      throw new Error('Invalid review comment.');
  }

  el('html').onchange = async () => {
    verifiedForSave = false;
    selectedHash = null;
    try {
      const file = el('html').files[0];
      if (!file) return;
      const data = await file.arrayBuffer();
      const text = new TextDecoder('utf-8', {fatal: true}).decode(data);
      const start = '<!-' + '-CP_REVIEW_START-->';
      const end = '<!-' + '-CP_REVIEW_END-->';
      const core = text.replace(new RegExp(start + '[\\s\\S]*?' + end), '');
      if (await sha256(new TextEncoder().encode(core)) !== config.content_sha256)
        throw new Error('Selected HTML content differs from this open report.');
      selectedHash = await sha256(data);
      if (state) validate(state);
      verifiedForSave = true;
      message(state ? 'HTML matches loaded review. Save or Copy enabled once.' :
        'HTML selected. Load its matching comments JSON to bind this review.');
    } catch (error) { message(error.message); }
    el('html').value = '';
  };

  function merge(incoming) {
    if (!state) { state = incoming; return; }
    const prior = JSON.parse(JSON.stringify({...state, prior_rounds: []}));
    const other = JSON.parse(JSON.stringify({...incoming, prior_rounds: []}));
    const queue = [...state.prior_rounds, ...incoming.prior_rounds, prior, other];
    const flattened = [];
    for (let i = 0; i < queue.length; i++) {
      const round = queue[i];
      if (!round || typeof round !== 'object') continue;
      if (Array.isArray(round.prior_rounds)) queue.push(...round.prior_rounds);
      flattened.push(JSON.parse(JSON.stringify({...round, prior_rounds: []})));
    }
    for (const row of state.results) {
      const next = incoming.results.find(r => r.id === row.id);
      if (row.decision === 'pending') row.decision = next.decision;
      else if (next.decision !== 'pending' && row.decision !== next.decision)
        state.decision_conflicts.push({section: row.id, retained: row.decision,
          incoming: next.decision, resolution: 'pending'});
    }
    state.comments = unique([...state.comments, ...incoming.comments]);
    state.decision_conflicts = unique([...state.decision_conflicts, ...incoming.decision_conflicts]);
    state.prior_rounds = unique(flattened);
  }

  el('load').onchange = async () => {
    try {
      const file = el('load').files[0];
      if (!file) return;
      if (file.size > 5000000) throw new Error('Review JSON exceeds the 5 MB limit.');
      const incoming = JSON.parse(await file.text());
      validate(incoming);
      merge(incoming);
      show();
      message('Matching review loaded; any conflicting decisions remain recorded.');
    } catch (error) { message(error.message); }
    el('load').value = '';
  };

  el('add').onclick = () => {
    if (!state) return message('Bind HTML and load matching JSON first.');
    const reviewer = el('reviewer').value.trim();
    const text = el('comment').value.trim();
    if (!reviewer || !text) return message('Enter reviewer and comment text.');
    const section = el('section').value;
    const disposition = el('disposition').value;
    const reason = el('reason').value.trim();
    if (disposition === 'deferred' && !reason) return message('A deferred comment requires a reason.');
    state.comments.push({id: crypto.randomUUID(), section, reviewer, text,
      quote: el('quoted').value, disposition, reason, recorded_at: new Date().toISOString()});
    const row = state.results.find(result => result.id === section);
    const decision = el('decision').value;
    if (row.decision !== 'pending' && row.decision !== decision)
      state.decision_conflicts.push({section, retained: row.decision,
        incoming: decision, resolution: 'pending'});
    else row.decision = decision;
    el('comment').value = '';
    show();
    message('Comment recorded locally. Save JSON to retain it after closing.');
  };

  el('quote').onclick = () => {
    const selection = window.getSelection();
    if (!selection || !selection.rangeCount) return message('Select report text first.');
    const node = selection.anchorNode;
    const section = (node.nodeType === 1 ? node : node.parentElement).closest('section');
    if (!section || !ids.has(section.id)) return message('Select text inside a calculation or review section.');
    el('section').value = section.id;
    el('quoted').value = selection.toString().slice(0, 10000);
    message('Quoted text and revision-specific section captured.');
  };

  function exportJson() {
    if (!state || !verifiedForSave) throw new Error('Re-select the current HTML before Save or Copy.');
    validate(state);
    verifiedForSave = false;
    return JSON.stringify(state, null, 2) + '\n';
  }

  el('save').onclick = async () => {
    try {
      const data = exportJson();
      if (typeof window.showSaveFilePicker === 'function') {
        const handle = await window.showSaveFilePicker({suggestedName: `${config.report_id}.comments.json`,
          types: [{description: 'Review JSON', accept: {'application/json': ['.json']}}]});
        const writable = await handle.createWritable();
        await writable.write(data);
        await writable.close();
        if (await (await handle.getFile()).text() !== data) throw new Error('Saved JSON read-back differs.');
        return message('JSON saved and read back successfully.');
      }
      const url = URL.createObjectURL(new Blob([data], {type: 'application/json'}));
      const link = document.createElement('a');
      link.href = url;
      link.download = `${config.report_id}.comments.json`;
      link.click();
      setTimeout(() => URL.revokeObjectURL(url), 1000);
      message('JSON download requested in browser Downloads. Re-load it to verify the saved copy.');
    } catch (error) { message(error.message); }
  };

  el('copy').onclick = async () => {
    try {
      const data = exportJson();
      await navigator.clipboard.writeText(data);
      message('JSON copied. Re-select HTML before another export.');
    } catch (error) { message(`${error.message} Use Save JSON if clipboard access is unavailable.`); }
  };
})();
