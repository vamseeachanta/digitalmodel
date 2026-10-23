# ABOUTME: Tests the issue #1915 wall-thickness 3D explorer renderer.
# ABOUTME: Verifies the public page builder embeds a compact, usable study slice.

import importlib.util
import json
import shutil
import subprocess
import sys
from pathlib import Path

import pytest


REPO_ROOT = Path(__file__).resolve().parents[2]
SCRIPT = (
    REPO_ROOT
    / "scripts"
    / "capabilities"
    / "build_wall_thickness_3d_page.py"
)


def _load_module():
    spec = importlib.util.spec_from_file_location(
        "build_wall_thickness_3d_page",
        SCRIPT,
    )
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def _study():
    walls = [8.0 + 0.5 * index for index in range(7)]
    tensions = [100.0 * index for index in range(7)]
    moments = [10.0 * index for index in range(7)]
    codes = ["DNV-ST-F101", "API-RP-1111"]
    rows = []
    for code in codes:
        for wall_index, wall in enumerate(walls):
            for tension_index, tension in enumerate(tensions):
                for moment_index, moment in enumerate(moments):
                    utilisation = 0.5 + 0.1 * (
                        wall_index + tension_index + moment_index
                    )
                    rows.append(
                        {
                            "code": code,
                            "wall_thickness_mm": wall,
                            "effective_tension_n": tension,
                            "bending_moment_nm": moment,
                            "utilisation": utilisation,
                            "governing_check": (
                                "burst"
                                if (tension_index + moment_index) % 2
                                else "collapse"
                            ),
                        }
                    )
    return {
        "meta": {
            "geometry": {
                "outer_diameter_m": 0.3239,
                "fabrication_tolerance_fraction": 0.125,
            },
            "material": {
                "grade": "X65",
                "smys_pa": 448.0e6,
                "smts_pa": 530.9e6,
            },
            "pressures": {
                "internal_pa": 20.0e6,
                "external_pa": 10.0e6,
            },
            "safety_class": "medium",
            "grid": {
                "wall_thickness_mm": {"values": walls},
                "effective_tension_n": {"values": tensions},
                "bending_moment_nm": {"values": moments},
                "design_codes": codes,
                "row_count": len(rows),
            },
        },
        "rows": rows,
    }


def _embedded_payload(page):
    opening = '<script id="study-data" type="application/json">'
    payload = page.split(opening, 1)[1].split("</script>", 1)[0]
    return payload, json.loads(payload)


def test_build_page_embeds_a_downsampled_slice_for_every_code():
    renderer = _load_module()

    page = renderer.build_page(_study())

    raw_payload, payload = _embedded_payload(page)
    assert payload["m"]["c"] == ["DNV-ST-F101", "API-RP-1111"]
    assert payload["m"]["w"] == [8.0, 9.0, 10.0, 11.0]
    assert payload["m"]["t"] == [0, 300, 600]
    assert payload["m"]["b"] == [0, 30, 60]
    assert payload["m"]["p"] == 2 * 4 * 3 * 3
    assert all(len(slices) == 4 for slices in payload["s"].values())
    assert all(
        len(values) == 9
        for slices in payload["s"].values()
        for values, _governing in slices
    )
    first_values, first_governing = payload["s"]["DNV-ST-F101"][0]
    assert first_values == [
        5000,
        8000,
        11000,
        8000,
        11000,
        14000,
        11000,
        14000,
        17000,
    ]
    assert payload["g"] == ["burst", "collapse"]
    assert first_governing == [1, 0, 1, 0, 1, 0, 1, 0, 1]
    assert payload["m"]["z"] == len(raw_payload.encode("utf-8"))
    assert payload["m"]["z"] < 400_000


def test_page_has_accessible_controls_and_an_automatic_2d_fallback():
    renderer = _load_module()

    page = renderer.build_page(_study())

    assert '<label for="code">' in page
    assert '<select id="code">' in page
    assert '<label for="wall">' in page
    assert '<input id="wall" type="range"' in page
    assert '<canvas id="fallback" role="img"' in page
    assert 'type:"heatmap"' in page
    assert 'type:"contour"' in page
    assert 'getContext("webgl"' in page
    assert "plotly-3.6.0.min.js" in page
    assert "plotly-latest" not in page
    assert "overflow-x:hidden" in page
    assert 'href="../../examples/wall-thickness-load-example.html"' in page


def test_default_view_marks_the_dnv_acceptance_boundary():
    renderer = _load_module()

    page = renderer.build_page(_study())

    assert 'M.c.includes("DNV-ST-F101")' in page
    assert "M.w.indexOf(20)" in page
    assert "z:[1,1,1,1]" in page
    assert "Utilisation = 1.0" in page


def test_fallback_stays_sized_and_plot_updates_are_serialised():
    renderer = _load_module()

    page = renderer.build_page(_study())

    assert 'canvas.parentElement.getBoundingClientRect()' in page
    assert "while(pendingPlot)" in page
    assert 'plot.style.visibility="hidden"' in page
    assert "a.u===level&&b.u===level" in page


def test_build_page_rejects_an_embedded_payload_at_the_size_limit():
    renderer = _load_module()
    study = _study()
    oversized_name = "governing_" + "x" * 400_000
    for row in study["rows"]:
        row["governing_check"] = oversized_name

    with pytest.raises(ValueError, match="embedded JSON"):
        renderer.build_page(study)


def test_main_reads_the_full_study_path_and_writes_the_explorer(
    tmp_path,
    capsys,
):
    renderer = _load_module()
    source = tmp_path / "wall-thickness-3d.json"
    output = tmp_path / "wall-thickness-3d-explorer.html"
    source.write_text(json.dumps(_study()), encoding="utf-8")
    renderer._REPO = tmp_path
    renderer._SOURCE = source
    renderer._OUTPUT = output

    renderer.main()

    assert output.read_text(encoding="utf-8").startswith("<!DOCTYPE html>")
    assert "sidecar JSON bytes" in capsys.readouterr().out


def test_main_writes_a_fetched_bounded_sidecar(tmp_path, capsys):
    renderer = _load_module()
    renderer._REPO = tmp_path
    renderer._SOURCE = tmp_path / "full.json"
    renderer._OUTPUT = tmp_path / "explorer.html"
    renderer._SOURCE.write_text(json.dumps(_study()), encoding="utf-8")
    renderer.main()
    page = renderer._OUTPUT.read_text(encoding="utf-8")
    sidecar = renderer._OUTPUT.with_suffix(".json")
    raw = sidecar.read_bytes()
    data = json.loads(raw)
    assert 'fetch("explorer.json"' in page
    assert '<script id="study-data"' not in page
    assert data["m"]["z"] == len(raw) < 400_000
    assert data["m"]["p"] == 72
    assert data["m"]["qualification"]["status"] == "NOT_EVALUATED"
    assert "embedded JSON bytes" not in capsys.readouterr().out
    assert "400 kB" in page
    assert "Data could not be loaded" in page


@pytest.mark.parametrize("data_url", ["https://example.invalid/a.json", "//example.invalid/a.json", "a</script>.json"])
def test_external_contract_rejects_nonlocal_urls(data_url):
    with pytest.raises(ValueError, match="sidecar"):
        _load_module().build_page(_study(), data_url=data_url)


def test_snapshot_has_an_explicit_row_cap(monkeypatch):
    renderer = _load_module()
    monkeypatch.setattr(renderer, "_MAX_DISPLAY_POINTS", 1)
    def unexpected_packing(*args):
        pytest.fail("point cap must be checked before allocation")
    monkeypatch.setattr(renderer, "_pack_rows", unexpected_packing)
    with pytest.raises(ValueError, match="point cap"):
        renderer.build_page(_study())


@pytest.mark.parametrize("webgl", [False, True])
@pytest.mark.parametrize("external", [False, True])
def test_viewer_executes_at_mobile_width_without_plotly(tmp_path, webgl, external):
    """Run shipped JavaScript with a 375 px canvas and no external library.

    The viewport is the issue's mobile acceptance width. Canvas calls are
    recorded rather than rendered; finite coordinates and working wall/code
    controls are checked without starting a browser or requiring WebGL.
    """
    node = shutil.which("node")
    if node is None:
        pytest.skip("Node is unavailable")
    page = _load_module().build_page(_study())
    raw, _payload = _embedded_payload(page)
    if external:
        page = _load_module().build_page(_study(), data_url="data.json")
    script = page.rsplit("<script>", 1)[1].split("</script>", 1)[0]
    harness = r"""
const assert = require('node:assert/strict');
const calls = [];
const context = new Proxy({}, {get:(_target, name)=>(...args)=>{
  args.filter(x=>typeof x==='number').forEach(x=>assert(Number.isFinite(x)));
  calls.push(name);
}, set:()=>true});
const elements = new Map();
function element(id){if(!elements.has(id))elements.set(id, {
 value:'0',style:{},textContent:'',innerHTML:'',max:0,
 parentElement:{getBoundingClientRect:()=>({width:375,height:390})},
 setAttribute(){},addEventListener(event,fn){this.oninput=fn;},add(option){this.value=option.value;},
 getContext:()=>context
});return elements.get(id);}
global.document={getElementById:element,documentElement:{},createElement:()=>({getContext:()=>WEBGL})};
global.window={WebGLRenderingContext:WEBGL,addEventListener(){}};
global.devicePixelRatio=1;
global.getComputedStyle=()=>({getPropertyValue:()=> '#222'});
global.Option=function(text,value){this.text=text;this.value=value;};
element('study-data').textContent=RAW;
global.fetch=()=>Promise.resolve({ok:true,json:()=>Promise.resolve(JSON.parse(element('study-data').textContent))});
""".replace("WEBGL", "true" if webgl else "false").replace("RAW", json.dumps(raw))
    trailer = r"""
setImmediate(()=>{
assert(calls.includes('fillRect'));
assert.match(element('wall-value').textContent, /mm/);
assert.match(element('summary').innerHTML, /Range/);
assert.match(element('mode').textContent, /2D/);
assert(element('fallback').width<=375);
element('wall').value='0';element('wall').oninput();
assert.equal(element('wall-value').textContent,'8.0 mm');
element('code').value='API-RP-1111';element('code').oninput();
assert.match(element('summary').innerHTML,/Range/);
});
"""
    target = tmp_path / "viewer.cjs"
    target.write_text(harness + script + trailer, encoding="utf-8")
    result = subprocess.run([node, str(target)], capture_output=True, text=True, check=False)
    assert result.returncode == 0, result.stderr


def test_capability_card_links_to_the_delivered_load_explorer():
    cards = json.loads((REPO_ROOT / "docs/capability-map/capability-cards.json").read_text())
    card = next(card for card in cards if card["id"] == "wall-thickness-load-explorer")
    assert card["link"] == "docs/api/structural/wall-thickness-3d-explorer.html"
    assert "tension" in card["summary"].lower()
    assert "assumed" in card["summary"].lower()


def test_worked_example_records_inputs_method_answer_and_limits():
    example = REPO_ROOT / "docs/examples/wall-thickness-load-example.html"
    text = example.read_text(encoding="utf-8")
    for phrase in ("Revision history", "Inputs", "Method", "Answer", "NOT_EVALUATED", "assumed, based on previous experience"):
        assert phrase in text


def test_worked_example_identifies_assumed_strength_conversion_basis():
    text = (REPO_ROOT / "docs/examples/wall-thickness-load-example.html").read_text()
    assert "rounded ksi conversions" in text
    assert "65 ksi" in text and "77 ksi" in text
    assert "not asserted as API 5L tabulated minima" in text
    assert "API 5L X65 PSL1 is assumed" not in text


def test_worked_example_links_stay_inside_the_docs_site():
    from html.parser import HTMLParser

    class Links(HTMLParser):
        def handle_starttag(self, tag, attrs):
            if tag == "a":
                href = dict(attrs).get("href", "")
                assert not href.startswith("../../")

    text = (REPO_ROOT / "docs/examples/wall-thickness-load-example.html").read_text()
    Links().feed(text)
    assert "ApiStd2rdStrategy" in text
    assert "test_wall_thickness_3d.py" in text
