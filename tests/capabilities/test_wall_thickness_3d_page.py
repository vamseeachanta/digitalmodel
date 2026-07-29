# ABOUTME: Tests the issue #1915 wall-thickness 3D explorer renderer.
# ABOUTME: Verifies the public page builder embeds a compact, usable study slice.

import importlib.util
import json
import sys
from pathlib import Path


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
    assert 'href="wall-thickness-3d.json"' in page
