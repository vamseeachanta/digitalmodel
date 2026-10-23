/* Local-only chart inspection and portable section comments. */
(() => {
  const byId = (id) => document.getElementById(id);
  const comments = [];
  const revision = JSON.parse(byId('change-data').textContent).revision;
  const sections = [...document.querySelectorAll('section[id]')];
  for (const section of sections) {
    const option = document.createElement('option');
    option.value = section.id;
    option.textContent = section.querySelector('h2').textContent;
    byId('comment-section').append(option);
  }
  for (const bar of document.querySelectorAll('[data-detail]')) {
    for (const event of ['pointerenter', 'focus', 'click']) {
      bar.addEventListener(event, () => {
        byId('chart-detail').textContent = bar.dataset.detail;
      });
    }
  }
  const showComments = () => {
    byId('comment-list').replaceChildren();
    for (const comment of comments) {
      const item = document.createElement('li');
      const section = sections.find((s) => s.id === comment.section);
      item.textContent = section.querySelector('h2').textContent + ': ' + comment.text;
      byId('comment-list').append(item);
    }
  };
  byId('add-comment').addEventListener('click', () => {
    const text = byId('comment-text').value.trim();
    if (!text) return;
    comments.push({section: byId('comment-section').value, text});
    byId('comment-text').value = '';
    showComments();
    byId('comment-status').textContent = 'Comment added. Save the comment file to retain it.';
  });
  byId('save-comments').addEventListener('click', async () => {
    const content = JSON.stringify({revision, comments}, null, 2) + '\n';
    try {
      if (window.showSaveFilePicker) {
        const file = await window.showSaveFilePicker({suggestedName: 'comments.json',
          types: [{description: 'Comment record', accept: {'application/json': ['.json']}}]});
        const stream = await file.createWritable();
        await stream.write(content);
        await stream.close();
        byId('comment-status').textContent = 'Comment file saved to the selected location.';
      } else {
        const url = URL.createObjectURL(new Blob([content], {type: 'application/json'}));
        const link = document.createElement('a');
        link.href = url;
        link.download = 'comments.json';
        link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        byId('comment-status').textContent = 'Download requested. Place comments.json beside the report.';
      }
    } catch (error) {
      byId('comment-status').textContent = error.name === 'AbortError'
        ? 'Save cancelled. Comments remain unsaved.' : 'Save failed. Comments remain unsaved.';
    }
  });
  byId('load-comments').addEventListener('change', async (event) => {
    const file = event.target.files[0];
    if (!file) return;
    try {
      const record = JSON.parse(await file.text());
      if (!['A', revision].includes(record.revision) || !Array.isArray(record.comments)
          || !record.comments.every((c) => c && typeof c.text === 'string'
            && sections.some((s) => s.id === c.section))) throw new Error('Invalid record');
      for (const comment of record.comments) {
        if (!comments.some((c) => c.section === comment.section && c.text === comment.text)) {
          comments.push(comment);
        }
      }
      showComments();
      byId('comment-status').textContent = 'Saved comments merged. Save the comment file to retain them.';
    } catch {
      byId('comment-status').textContent = 'Invalid comment file; existing comments retained.';
    }
  });
  const changeData = JSON.parse(byId('change-data').textContent);
  for (const change of changeData.changes) {
    const paragraph = document.createElement('p');
    paragraph.textContent = change;
    byId('changes-panel').append(paragraph);
  }
})();
