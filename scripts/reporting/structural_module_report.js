/* Offline calculation readout and comments; no remote service is used. */
(() => {
  "use strict";
  const data = JSON.parse(document.getElementById("calculation-data").textContent);
  const changes = JSON.parse(document.getElementById("change-data").textContent);
  const slider = document.getElementById("pressure-sample");
  const readout = document.getElementById("sample-readout");
  const format = (value) => value.toFixed(1).replace("-", "−");
  const describe = (row) =>
    `Public cylinder; pressure ${format(row.pressure_mpa)} MPa; ` +
    `hoop demand ${format(row.hoop_mpa)} MPa; ` +
    `axial demand ${format(row.axial_mpa)} MPa; ` +
    `equivalent demand ${format(row.equivalent_mpa)} MPa. ` +
    "Strength limit NOT_EVALUATED; strength acceptance is not evaluated. " +
    "The diameter convention is ASSUMED - to be confirmed.";
  const showSample = () => { readout.textContent = describe(data.series[Number(slider.value)]); };
  slider.addEventListener("input", showSample);
  showSample();

  const panel = document.getElementById("change-panel");
  for (const revision of changes.revisions) {
    const paragraph = document.createElement("p");
    paragraph.textContent = `Revision ${revision.revision}, ${revision.date}: ${revision.changes.join(" ")}`;
    panel.append(paragraph);
  }
  const filename = "structural-module-report.comments.json";
  const storageKey = "structural-module-report-comments-r1";
  const section = document.getElementById("comment-section");
  const status = document.getElementById("comment-status");
  const list = document.getElementById("comment-list");
  const headings = [...document.querySelectorAll("main > section > h2")];
  const sectionIds = headings.map((heading) => heading.parentElement.id);
  for (const heading of headings) {
    const option = document.createElement("option");
    option.value = heading.parentElement.id;
    option.textContent = heading.textContent;
    section.append(option);
  }
  const empty = () => ({document: "structural-module-report", revision: "1", comments: []});
  function validate(value) {
    if (!value || value.document !== "structural-module-report" || value.revision !== "1" ||
        !Array.isArray(value.comments) || value.comments.length > 1000) {
      throw new Error("The comments file does not match this report revision.");
    }
    for (const comment of value.comments) {
      if (!comment || typeof comment.text !== "string" || !comment.text.trim() ||
          comment.text.length > 4000 || !sectionIds.includes(comment.section) ||
          typeof comment.created !== "string" || !Number.isFinite(Date.parse(comment.created))) {
        throw new Error("The comments file contains an invalid comment.");
      }
    }
    return value;
  }
  let comments = empty();
  try {
    const saved = localStorage.getItem(storageKey);
    if (saved) {
      comments = validate(JSON.parse(saved));
      status.textContent = "A browser draft was restored; a file save is still required.";
    }
  } catch (_) { status.textContent = "The browser draft is unavailable; file saving remains available."; }

  function draw() {
    list.replaceChildren();
    for (const comment of comments.comments) {
      const item = document.createElement("li");
      const link = document.createElement("a");
      link.href = `#${comment.section}`;
      link.textContent = headings[sectionIds.indexOf(comment.section)].textContent;
      const text = document.createElement("p");
      text.textContent = `${comment.created}: ${comment.text}`;
      item.append(link, text);
      list.append(item);
    }
  }
  function draft() {
    try {
      localStorage.setItem(storageKey, JSON.stringify(comments));
      status.textContent = "Browser draft updated; save a comments file to retain it beside the report.";
    } catch (_) { status.textContent = "Browser storage is unavailable; save a comments file now."; }
    draw();
  }
  document.getElementById("comment-form").addEventListener("submit", (event) => {
    event.preventDefault();
    const text = document.getElementById("comment-text");
    if (!text.value.trim() || text.value.length > 4000 || comments.comments.length >= 1000) return;
    comments.comments.push({section: section.value, text: text.value.trim(), created: new Date().toISOString()});
    text.value = "";
    draft();
  });
  document.getElementById("save-comments").addEventListener("click", async () => {
    if (!("showDirectoryPicker" in window)) {
      status.textContent = "Folder access is unavailable. Download the comments file and place it beside the report.";
      return;
    }
    try {
      const directory = await window.showDirectoryPicker({mode: "readwrite"});
      const report = await directory.getFileHandle("structural-module-report.html");
      const content = await (await report.getFile()).text();
      if (!content.includes('id="calculation-data"') ||
          !content.includes(data.source_input_digest)) {
        throw new Error("The selected folder does not contain the matching report.");
      }
      const file = await directory.getFileHandle(filename, {create: true});
      const writable = await file.createWritable();
      await writable.write(JSON.stringify(comments, null, 2));
      await writable.close();
      status.textContent = "Comments saved beside the matching report in the selected folder.";
    } catch (error) {
      status.textContent = error.name === "AbortError" ? "File save cancelled; the browser draft remains available." :
        `Comments were not saved: ${error.message}`;
    }
  });
  document.getElementById("download-comments").addEventListener("click", () => {
    const blob = new Blob([JSON.stringify(comments, null, 2)], {type: "application/json"});
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = filename;
    anchor.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    status.textContent = "Comments download requested. Confirm the file is saved beside the report.";
  });
  document.getElementById("load-comments").addEventListener("change", async (event) => {
    const file = event.target.files[0];
    if (!file) return;
    try {
      if (file.size > 5_000_000) throw new Error("The comments file exceeds the size limit.");
      const loaded = validate(JSON.parse(await file.text()));
      if (comments.comments.length && !window.confirm("Replace the current browser draft with the selected comments?")) return;
      comments = loaded;
      draft();
      status.textContent = "Saved comments loaded into the browser draft.";
    } catch (error) { status.textContent = `Comments were not loaded: ${error.message}`; }
    event.target.value = "";
  });
  draw();
})();
