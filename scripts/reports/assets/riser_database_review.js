/* Standalone review controls. All document data and styles remain in the HTML. */
(() => {
  "use strict";
  const model = JSON.parse(document.getElementById("report-data").textContent);
  const changes = JSON.parse(document.getElementById("report-changes").textContent);
  const byId = (id) => document.getElementById(id);
  const filename = "riser-database-analysis.comments.json";
  const storageKey = "riser-database-analysis-review";
  let selected = { text: "", section: "" };
  let folder = null;
  let comments = [];
  const status = (message) => { byId("comment-status").textContent = message; };
  const payload = () => ({
    document: changes.document, revision: changes.revision,
    source_digests: model.source_digests, comments,
  });

  const validComments = (data) => {
    if (data.document !== changes.document || !Array.isArray(data.comments)) {
      throw new Error("The selected file is not a comment file for this document.");
    }
    if (!data.comments.every((item) => item &&
      ["text", "section", "note", "replacement"].every((key) => typeof item[key] === "string"))) {
      throw new Error("The comment file has an invalid record.");
    }
    return data.comments.map((item) => ({
      ...item, revision: item.revision ?? data.revision,
      source_digests: item.source_digests ?? data.source_digests,
    }));
  };

  const canonicalize = (value) => {
    if (Array.isArray(value)) return value.map(canonicalize);
    if (value && typeof value === "object") {
      return Object.fromEntries(Object.keys(value).sort().map((key) => [key, canonicalize(value[key])]));
    }
    return value;
  };
  const fingerprint = (item) => JSON.stringify(canonicalize(item));

  const mergeComments = (incoming) => {
    const fingerprints = new Set(comments.map(fingerprint));
    incoming.forEach((item) => {
      const key = fingerprint(item);
      if (!fingerprints.has(key)) {
        comments.push(item);
        fingerprints.add(key);
      }
    });
  };

  const readFolderComments = async () => {
    try {
      const file = await (await folder.getFileHandle(filename)).getFile();
      mergeComments(validComments(JSON.parse(await file.text())));
    } catch (error) {
      if (error.name !== "NotFoundError") throw error;
    }
  };

  const drawComments = () => {
    byId("comment-list").replaceChildren();
    comments.forEach((comment) => {
      const item = document.createElement("li");
      const quote = document.createElement("blockquote");
      quote.textContent = comment.text;
      const note = document.createElement("p");
      note.textContent = comment.note;
      const replacement = document.createElement("p");
      replacement.textContent = comment.replacement ? "Proposed replacement: " + comment.replacement : "";
      item.append(quote, note, replacement);
      byId("comment-list").append(item);
    });
    try { localStorage.setItem(storageKey, JSON.stringify(payload())); }
    catch { status("Browser storage is unavailable. Export comments before closing this document."); }
  };

  try {
    const saved = localStorage.getItem(storageKey);
    if (saved) {
      const data = JSON.parse(saved);
      comments = validComments(data);
      if (JSON.stringify(data.source_digests) !== JSON.stringify(model.source_digests)) {
        status("Restored comments refer to different source evidence. Check each quoted passage before reuse.");
      }
    }
  } catch { status("Saved browser comments cannot be read. Import a saved comments file to recover them."); }
  drawComments();

  document.addEventListener("selectionchange", () => {
    const selection = window.getSelection();
    const text = selection ? selection.toString().trim() : "";
    if (!text || !selection.anchorNode) return;
    const element = selection.anchorNode.nodeType === Node.ELEMENT_NODE ?
      selection.anchorNode : selection.anchorNode.parentElement;
    if (!element || element.closest("#comments, nav")) return;
    selected = { text, section: element.closest("section[id], .appendix[id]")?.id || "document" };
    byId("selected-text").textContent = text;
  });

  byId("add-comment").addEventListener("click", () => {
    const note = byId("comment-note").value.trim();
    const replacement = byId("comment-replacement").value.trim();
    if (!selected.text || (!note && !replacement)) {
      status("Select document text and enter a comment or replacement first.");
      return;
    }
    comments.push({ ...selected, note, replacement, date: new Date().toISOString(),
      revision: changes.revision, source_digests: model.source_digests });
    byId("comment-note").value = "";
    byId("comment-replacement").value = "";
    drawComments();
    status("Comment added in this browser. Save beside the report or export a comments file.");
  });

  byId("export-comments").addEventListener("click", () => {
    const url = URL.createObjectURL(new Blob([JSON.stringify(payload(), null, 2) + "\n"],
      { type: "application/json" }));
    const link = document.createElement("a");
    link.href = url;
    link.download = filename;
    link.click();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
    status("Comments exported. Place the downloaded comments file beside the report.");
  });

  byId("choose-folder").addEventListener("click", async () => {
    if (!window.showDirectoryPicker) {
      status("Folder saving is unavailable in this browser. Export and place the comments file beside the report.");
      return;
    }
    try {
      folder = await window.showDirectoryPicker({ mode: "readwrite" });
      // Preserve existing comments rather than overwrite an unseen review file.
      await readFolderComments();
      drawComments();
      status("Report folder selected. Save comments beside the report when editing is finished.");
    } catch {
      folder = null;
      status("The report folder was not opened or its comments could not be read. Existing files were preserved.");
    }
  });

  byId("save-comments").addEventListener("click", async () => {
    if (!folder) { status("Choose the report folder first, or export the comments file."); return; }
    try {
      await readFolderComments();
      const handle = await folder.getFileHandle(filename, { create: true });
      const writable = await handle.createWritable();
      await writable.write(JSON.stringify(payload(), null, 2) + "\n");
      await writable.close();
      drawComments();
      status("Comments saved beside the report in the selected folder.");
    } catch { status("Comments were not saved. Export them to preserve this review."); }
  });

  byId("import-comments").addEventListener("change", async (event) => {
    const file = event.target.files[0];
    if (!file) return;
    try {
      const data = JSON.parse(await file.text());
      mergeComments(validComments(data));
      drawComments();
      status("Saved comments imported. Check quoted passages if the report revision has changed.");
    } catch { status("The comments file was not imported. Existing comments were preserved."); }
  });

  const list = document.createElement("ul");
  changes.changes.forEach((change) => {
    const item = document.createElement("li");
    const link = document.createElement("a");
    link.href = "#" + change.section;
    link.textContent = change.description;
    item.append(link);
    list.append(item);
  });
  byId("change-list").append(list);
  document.querySelectorAll("[data-measure]").forEach((point) => {
    const show = () => { byId("chart-readout").textContent = point.querySelector("title").textContent; };
    point.addEventListener("mouseenter", show);
    point.addEventListener("focus", show);
    point.addEventListener("click", show);
  });
})();
