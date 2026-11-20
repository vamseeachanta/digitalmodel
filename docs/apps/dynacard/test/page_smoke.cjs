/* Local Chromium smoke test. No packages, server, or remote content. */
"use strict";
const assert = require("node:assert/strict");
const fs = require("node:fs");
const os = require("node:os");
const path = require("node:path");
const {pathToFileURL} = require("node:url");
const {spawn} = require("node:child_process");

function browserPath() {
  const paths = [process.env.BROWSER_BINARY,
    path.join(process.env.ProgramFiles || "", "Google/Chrome/Application/chrome.exe"),
    path.join(process.env["ProgramFiles(x86)"] || "", "Microsoft/Edge/Application/msedge.exe")];
  return paths.find(value => value && fs.existsSync(value));
}

function connection(child) {
  let nextId = 0, buffer = "";
  const pending = new Map(), events = [];
  const rejectPending = error => {
    for (const waiter of pending.values()) waiter.reject(error);
    pending.clear();
  };
  child.once("error", rejectPending);
  child.once("exit", code => rejectPending(new Error(`Browser exited with code ${code}`)));
  child.stdio[4].on("data", chunk => {
    buffer += chunk.toString();
    let end;
    while ((end = buffer.indexOf("\0")) >= 0) {
      const message = JSON.parse(buffer.slice(0, end));
      buffer = buffer.slice(end + 1);
      if (message.id && pending.has(message.id)) {
        const {resolve, reject} = pending.get(message.id);
        pending.delete(message.id);
        if (message.error) reject(new Error(JSON.stringify(message.error)));
        else resolve(message.result);
      } else events.push(message);
    }
  });
  return {events, send(method, params = {}, sessionId) {
    return new Promise((resolve, reject) => {
      const id = ++nextId;
      pending.set(id, {resolve, reject});
      child.stdio[3].write(JSON.stringify({id, method, params, sessionId}) + "\0");
    });
  }};
}

async function browserActions() {
  const form = document.getElementById("well-form");
  const results = document.getElementById("results");
  for (let index = 0; index < 10; index++) {
    document.getElementById("example-select").value = String(index);
    document.getElementById("load-example").click(); form.requestSubmit();
    if (results.hidden) throw Error(`Example ${index + 1} analysis failed`);
    resultChecks(index); await downloadChecks(index);
  }
  if (document.querySelectorAll("svg polyline").length !== 2) throw Error("Plots missing");
  document.getElementById("spm").value = "7";
  document.getElementById("spm").dispatchEvent(new Event("input", {bubbles:true}));
  if (!results.hidden) throw Error("Stale results remained visible");
  document.getElementById("add-rod").click();
  if (document.querySelectorAll("#rod-rows tr").length !== 3) throw Error("Rod add failed");
  document.querySelector("#rod-rows tr:last-child button").click();
  await fileChecks();
  return {status:"PASS", examples:10, plots:2, filePaths:3,
    autoRun:true, workCardBindings:true, plotAnnotations:true,
    modelBlocks:document.querySelectorAll(".diagnosis").length};
}

function resultChecks(index) {
  const example = DynacardExamples.examples[index];
  const result = DynacardCore.analyse(example.card, DynacardExamples.well);
  const rows = [...document.querySelectorAll("#analysis-values tr")];
  for (const [label, key] of [["Polished-rod work per stroke (ft·lbf)", "polished_rod_work_per_stroke"],
    ["Pump-card work per stroke (ft·lbf)", "pump_work_per_stroke"]]) {
    const row = rows.find(item => item.cells[0].textContent === label);
    if (!row || row.cells[1].textContent !== DynacardPresentation.formatValue(result[key], "ft·lbf")) {
      throw Error(`Wrong card work displayed: ${label}`);
    }
  }
  const source = document.getElementById("example-source");
  if (source.hidden || !source.textContent.includes(example.name) ||
      !source.textContent.includes(example.crosswalk_status) ||
      !source.querySelector("a")?.textContent.includes("10.17632/hnfwm9mjs6.1")) throw Error("Example provenance missing");
  const workedLink = source.querySelector("a.worked-example-link");
  if (workedLink?.getAttribute("href") !== `../../domains/artificial_lift/dynacard-examples/README.md#example-${example.index}-${example.slug}`) throw Error("Worked-example link missing");
  const pump = document.getElementById("pump-plot");
  if (pump.dataset.positionOrigin !== "stroke-bottom" || Number(pump.dataset.positionMinimum) !== 0) throw Error("Pump origin not normalized");
  for (const phase of ["upstroke", "downstroke"]) {
    const line = pump.querySelector(`.${phase}-mean`);
    if (!line || Number(line.dataset.load) !== result.fluid_load[`${phase}_load`]) throw Error("Mean-load line mismatch");
  }
  for (const label of ["Pump stroke", "Effective travel", "Fillage", "Fluid load", " in", " lbf"]) {
    if (!pump.textContent.includes(label)) throw Error(`Plot annotation missing: ${label}`);
  }
  const diagnoses = [...document.querySelectorAll(".diagnosis")];
  if (diagnoses.length !== 2 || !diagnoses[0].textContent.includes("Shape-only model (computed pump card)")) throw Error("Diagnosis order/label mismatch");
  diagnoses.forEach(block => {
    if (!block.textContent.includes("HYPOTHESIS") || !block.textContent.includes("Model confidence:")) throw Error("Diagnosis basis missing");
  });
  const top = [...diagnoses[0].querySelectorAll("ol li")].map(item => item.textContent);
  if (top.length !== 3) throw Error("Shape top three missing");
  result.shape_diagnosis.differential.forEach((item, i) => {
    if (!top[i].includes(item.mode.replaceAll("_", " ")) || !top[i].includes(`${(item.probability * 100).toFixed(2)}%`)) throw Error("Shape probabilities mismatch");
  });
}

async function downloadChecks(index) {
  const example = DynacardExamples.examples[index];
  const expected = DynacardCore.analyse(example.card, DynacardExamples.well);
  const blobs = [], names = [], revoked = [];
  const create = URL.createObjectURL, revoke = URL.revokeObjectURL;
  const click = HTMLAnchorElement.prototype.click;
  URL.createObjectURL = blob => {blobs.push(blob); return `blob:test-${blobs.length}`;};
  URL.revokeObjectURL = url => revoked.push(url);
  HTMLAnchorElement.prototype.click = function() {names.push(this.download);};
  try {
    document.getElementById("download-results").click();
    document.getElementById("download-pump").click();
    if (blobs.length !== 2 || names.join() !== "dynacard-results.json,pump-card.csv") throw Error("Download controls failed");
    const actual = JSON.parse(await blobs[0].text());
    if (JSON.stringify(actual) !== JSON.stringify(expected)) throw Error("JSON lost analysis precision");
    const csv = await blobs[1].text();
    if (csv !== DynacardPresentation.pumpCsv(expected.downhole_card)) throw Error("CSV lost pump precision");
    await new Promise(resolve => setTimeout(resolve, 1100));
    if (revoked.length !== 2) throw Error("Download Blob URLs were not released");
  } finally {
    URL.createObjectURL = create; URL.revokeObjectURL = revoke;
    HTMLAnchorElement.prototype.click = click;
  }
}

async function fileChecks() {
  const input = document.getElementById("card-file"), form = document.getElementById("well-form");
  const upload = text => {
    const transfer = new DataTransfer(); transfer.items.add(new File([text], "card.csv", {type:"text/csv"}));
    input.files = transfer.files; input.dispatchEvent(new Event("change"));
  };
  const settled = async () => {
    for (let n = 0; n < 100; n++) {
      if (!document.getElementById("card-status").textContent.startsWith("Reading")) return;
      await new Promise(resolve => setTimeout(resolve, 10));
    }
    throw Error("CSV reading did not settle");
  };
  upload("position_in,load_lbf\nwrong,2"); await settled(); form.requestSubmit();
  if (!document.getElementById("results").hidden) throw Error("Rejected file retained results");
  if (!document.getElementById("error").textContent.includes("Load a valid")) throw Error("Rejected file retained card");
  const card = DynacardExamples.examples[0].card;
  upload("position_in,load_lbf\n" + card.position.map((v,i) => `${v},${card.load[i]}`).join("\n"));
  await settled(); form.requestSubmit();
  if (document.getElementById("results").hidden) throw Error("Valid CSV analysis failed");
  if (!document.getElementById("example-source").hidden) throw Error("CSV retained example provenance");
  const originalText = File.prototype.text;
  File.prototype.text = () => new Promise(resolve => setTimeout(() => resolve("bad"), 40));
  upload("delayed"); document.getElementById("load-example").click();
  await new Promise(resolve => setTimeout(resolve, 80)); File.prototype.text = originalText;
  form.requestSubmit();
  if (document.getElementById("results").hidden) throw Error("Stale file read replaced newer example");
}

async function exercise(client, sessionId) {
  const send = (method, params) => client.send(method, params, sessionId);
  await send("Runtime.enable");
  await send("Page.enable");
  await send("Network.enable");
  await send("Emulation.setDeviceMetricsOverride", {width:1400,height:1000,deviceScaleFactor:1,mobile:false});
  await send("Page.navigate", {url: pathToFileURL(path.resolve(__dirname, "../index.html")).href});
  for (let i = 0; i < 100; i++) {
    const result = await send("Runtime.evaluate", {expression: "document.body?.dataset.appState", returnByValue: true});
    if (result.result.value === "ready") break;
    if (i === 99) throw new Error("Page did not reach ready state");
    await new Promise(resolve => setTimeout(resolve, 100));
  }
  const initial = await send("Runtime.evaluate", {returnByValue:true, expression:
    `${resultChecks.toString()}; (() => {
      if (document.getElementById('results').hidden) throw Error('Initial auto-run missing');
      if (document.getElementById('results').getBoundingClientRect().top >= document.querySelector('.limits').getBoundingClientRect().top) throw Error('Results below limitations');
      resultChecks(0); return true;
    })()`});
  if (initial.exceptionDetails) throw new Error(JSON.stringify(initial.exceptionDetails));
  if (process.env.DYNACARD_SCREENSHOT) {
    const {contentSize} = await send("Page.getLayoutMetrics");
    const {data} = await send("Page.captureScreenshot", {format:"png", captureBeyondViewport:true,
      clip:{x:0,y:0,width:contentSize.width,height:contentSize.height,scale:1}});
    fs.writeFileSync(path.resolve(process.env.DYNACARD_SCREENSHOT), Buffer.from(data,"base64"));
  }
  const result = await send("Runtime.evaluate", {awaitPromise: true, returnByValue: true,
    expression: `${fileChecks.toString()}; ${downloadChecks.toString()}; (${browserActions.toString()})()`});
  if (result.exceptionDetails) throw new Error(JSON.stringify(result.exceptionDetails));
  const failures = client.events.filter(event => event.method === "Runtime.exceptionThrown" ||
    (event.method === "Runtime.consoleAPICalled" && event.params.type === "error"));
  assert.deepEqual(failures, [], "Browser console errors");
  const requests = client.events.filter(event => event.method === "Network.requestWillBeSent")
    .map(event => event.params.request.url).filter(url => !url.startsWith("file:") && !url.startsWith("data:"));
  assert.deepEqual(requests, [], "Page requested a network resource");
  console.log(JSON.stringify(result.result.value));
}

async function main() {
  assert.ok(fs.existsSync(path.resolve(__dirname, "../index.html")), "index.html is required");
  const executable = browserPath();
  if (!executable) throw new Error("No local Chromium browser found; set BROWSER_BINARY");
  const profile = fs.mkdtempSync(path.join(os.tmpdir(), "dynacard-browser-"));
  const child = spawn(executable, ["--headless=new", "--remote-debugging-pipe", "--no-first-run",
    "--no-default-browser-check", "--disable-background-networking", "--disable-component-update",
    "--disable-sync", "--disable-extensions", "--disable-gpu", "--disable-features=MediaRouter",
    "--host-resolver-rules=MAP * ~NOTFOUND", `--user-data-dir=${profile}`, "about:blank"],
  {windowsHide:true, stdio:["ignore", "pipe", "pipe", "pipe", "pipe"]});
  const exited = new Promise(resolve => {child.once("exit", resolve); child.once("error", resolve);});
  child.stderr.resume(); child.stdout.resume();
  const client = connection(child);
  const timeout = setTimeout(() => child.kill(), 30000);
  try {
    const {targetId} = await client.send("Target.createTarget", {url:"about:blank"});
    const {sessionId} = await client.send("Target.attachToTarget", {targetId, flatten:true});
    await exercise(client, sessionId);
  } finally {
    clearTimeout(timeout);
    child.kill();
    let cleanupTimer;
    await Promise.race([exited, new Promise(resolve => {cleanupTimer = setTimeout(resolve, 3000);})]);
    clearTimeout(cleanupTimer);
    const resolved = path.resolve(profile), temporaryRoot = path.resolve(os.tmpdir());
    assert.equal(path.dirname(resolved), temporaryRoot, "Profile cleanup must stay within temp root");
    assert.ok(path.basename(resolved).startsWith("dynacard-browser-"), "Profile cleanup requires owned prefix");
    fs.rmSync(profile, {recursive:true, force:true, maxRetries:5, retryDelay:100});
  }
}
main().catch(error => { console.error(error.message); process.exitCode = 1; });
