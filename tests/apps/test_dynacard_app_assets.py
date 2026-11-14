"""Generated browser model assets: file-size gate, exact content and loading."""

import json
from pathlib import Path
import re
import shutil
import subprocess

import pytest

ROOT = Path(__file__).resolve().parents[2]
APP = ROOT / "docs/apps/dynacard"
NODE = shutil.which("node")
NODE_REQUIRED = pytest.mark.skipif(NODE is None, reason="node is not on PATH")
# check-added-large-files --maxkb=1000 in .pre-commit-config.yaml; the hook
# counts kilobytes of 1024 bytes.
LARGE_FILE_LIMIT_BYTES = 1000 * 1024


def part_paths():
    return sorted(APP.glob("models-part-*.js"),
                  key=lambda path: int(re.search(r"(\d+)\.js$", path.name)[1]))


def embedded_text():
    """Join the part string literals independently of the generator."""
    texts = []
    for path in part_paths():
        script = path.read_text(encoding="utf-8")
        texts.append(json.loads(script.split("const part = ", 1)[1].split(";\n", 1)[0]))
    return "".join(texts)


def source_documents():
    from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
        MODEL_PATH, SHAPE_MODEL_PATH, read_model_bytes,
    )
    return {key: json.loads(read_model_bytes(path))
            for key, path in (("context", MODEL_PATH), ("shape", SHAPE_MODEL_PATH))}


def test_large_file_limit_is_the_pre_commit_limit():
    config = (ROOT / ".pre-commit-config.yaml").read_text(encoding="utf-8")
    assert re.search(r"check-added-large-files[\s\S]{0,200}--maxkb=1000\b", config)


def test_every_app_file_is_below_the_large_file_limit():
    sizes = {path.relative_to(APP).as_posix(): path.stat().st_size
             for path in APP.rglob("*") if path.is_file()}
    assert sizes["models.js"] > 0
    assert {name: size for name, size in sizes.items() if size > LARGE_FILE_LIMIT_BYTES} == {}


def test_model_parts_embed_exact_source_documents():
    parts = part_paths()
    assert len(parts) >= 2
    assert [path.name for path in parts] == [f"models-part-{i}.js" for i in range(1, len(parts) + 1)]
    loader = (APP / "models.js").read_text(encoding="utf-8")
    assert f"const PART_COUNT = {len(parts)};" in loader
    text = embedded_text()
    embedded = json.loads(text)
    assert text == json.dumps(embedded, separators=(",", ":"), ensure_ascii=False)
    assert f"const TEXT_LENGTH = {len(text)};" in loader
    assert embedded == source_documents()


def test_page_loads_every_part_before_the_loader():
    html = (APP / "index.html").read_text(encoding="utf-8")
    scripts = re.findall(r'<script defer src="([^"]+)"></script>', html)
    names = [path.name for path in part_paths()]
    assert scripts[:len(names) + 1] == names + ["models.js"]
    assert scripts.index("models.js") < scripts.index("dynacard-core.js")


BROWSER_LOAD = r"""
const fs = require('node:fs'), path = require('node:path'), vm = require('node:vm');
const [app, skip] = [process.argv[2], Number(process.argv[3])];
const html = fs.readFileSync(path.join(app, 'index.html'), 'utf8');
const names = [...html.matchAll(/<script defer src="(models[^"]*)"><\/script>/g)].map(m => m[1]);
const context = vm.createContext({});
try {
  names.forEach((name, index) => {
    if (index + 1 !== skip) vm.runInContext(fs.readFileSync(path.join(app, name), 'utf8'), context);
  });
  const models = context.DynacardModels;
  process.stdout.write(JSON.stringify({models, leftover: 'DynacardModelParts' in context}));
} catch (error) {
  process.stdout.write(JSON.stringify({error: error.message}));
}
"""


def browser_load(tmp_path, skip=0):
    script = tmp_path / "browser_load.cjs"
    script.write_text(BROWSER_LOAD, encoding="utf-8")
    process = subprocess.run([NODE, str(script), str(APP), str(skip)], capture_output=True,
                             text=True, encoding="utf-8", check=True,
                             creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    return json.loads(process.stdout)


@NODE_REQUIRED
def test_page_script_order_assembles_the_models(tmp_path):
    loaded = browser_load(tmp_path)
    assert loaded == {"models": source_documents(), "leftover": False}


@NODE_REQUIRED
@pytest.mark.parametrize("skip", [1, -1])
def test_loader_rejects_a_missing_part(tmp_path, skip):
    count = len(part_paths())
    loaded = browser_load(tmp_path, skip=count if skip == -1 else skip)
    assert "model part" in loaded["error"]


@NODE_REQUIRED
def test_node_require_assembles_the_models(tmp_path):
    script = tmp_path / "require_models.cjs"
    script.write_text("process.stdout.write(JSON.stringify(require(process.argv[2])));",
                      encoding="utf-8")
    process = subprocess.run([NODE, str(script), str(APP / "models.js")], capture_output=True,
                             text=True, encoding="utf-8", check=True,
                             creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
    assert json.loads(process.stdout) == source_documents()
