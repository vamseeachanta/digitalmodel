"use strict";
(() => {
  const readout = document.getElementById("chart-readout");
  document.querySelectorAll("[data-point]").forEach(point => {
    const show = () => { readout.textContent = point.getAttribute("aria-label"); };
    point.addEventListener("mouseenter", show);
    point.addEventListener("focus", show);
  });
  const changes = JSON.parse(document.getElementById("changes").textContent);
  changes.changes.forEach(change => {
    const item = document.createElement("li");
    item.textContent = change;
    document.getElementById("change-list").append(item);
  });
  const section = document.getElementById("comment-section");
  document.querySelectorAll("h2").forEach(heading => {
    const option = document.createElement("option");
    option.value = heading.id;
    option.textContent = heading.textContent;
    section.append(option);
  });
  let comments = [];
  const status = document.getElementById("comment-status");
  function render() {
    const list = document.getElementById("comment-list");
    list.replaceChildren();
    comments.forEach(comment => {
      const item = document.createElement("li");
      item.textContent = `${comment.section}: ${comment.text}`;
      list.append(item);
    });
  }
  document.getElementById("add-comment").addEventListener("click", () => {
    const field = document.getElementById("comment-text");
    const text = field.value.trim();
    if (!text) return;
    comments.push({ section: section.value, text });
    field.value = "";
    render();
    status.textContent = "Comments changed; save before closing this document.";
  });
  document.getElementById("save-comments").addEventListener("click", async () => {
    const text = JSON.stringify({ document: "fielddevelopment", comments }, null, 2);
    try {
      if (window.showSaveFilePicker) {
        const handle = await window.showSaveFilePicker({
          suggestedName: "report.comments.json",
          types: [{ description: "Reader comments", accept: { "application/json": [".json"] } }]
        });
        const stream = await handle.createWritable();
        await stream.write(text);
        await stream.close();
        status.textContent = "Comments saved to the selected file.";
      } else {
        const url = URL.createObjectURL(new Blob([text], { type: "application/json" }));
        const link = document.createElement("a");
        link.href = url;
        link.download = "report.comments.json";
        link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        status.textContent = "Download requested; place the comments file beside this report.";
      }
    } catch (error) {
      status.textContent = "Comments were not saved. Keep this document open and retry.";
    }
  });
  document.getElementById("load-comments").addEventListener("change", async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      const data = JSON.parse(await file.text());
      const sections = new Set(Array.from(section.options, item => item.value));
      if (data.document !== "fielddevelopment" || !Array.isArray(data.comments) ||
          !data.comments.every(item => item && sections.has(item.section) &&
            typeof item.text === "string")) throw new Error("Invalid comment data");
      comments = comments.concat(data.comments);
      render();
      status.textContent = "Comments loaded and appended. Save to retain the combined list.";
    } catch (error) {
      status.textContent = "Comments file not loaded; expected the report comments format.";
    }
  });
})();
