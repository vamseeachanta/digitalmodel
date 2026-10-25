# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Generate a generic hull screening estimate, data and independent tests.

Run: python scripts/capabilities/build_generic_hull_screening.py
Only the standard library and the public U-tube model are required.
"""

from __future__ import annotations

import argparse
from collections.abc import Callable
import hashlib
import importlib.util
import json
import math
from pathlib import Path
import sys
from time import perf_counter
from typing import Any

ROOT = Path(__file__).resolve().parents[2]
MODEL_PATH = "src/digitalmodel/hydrodynamics/utube_tank.py"
PAGE = "docs/api/cfd/generic-hull-roll-screening.html"
DATA = "docs/api/cfd/data/generic_hull_roll_screening.json"
TEST = "tests/hydrodynamics/test_generic_hull_screening.py"
PARENT_PAGE = "docs/api/cfd/dual-tank-sloshing-cfd-study.html"
SCRIPT = "scripts/capabilities/build_generic_hull_screening.py"
MODEL_SPEC = importlib.util.spec_from_file_location("_generic_roll_tank", ROOT / MODEL_PATH)
if MODEL_SPEC is None or MODEL_SPEC.loader is None:
    raise ImportError("Public tank model cannot be loaded")
MODEL = importlib.util.module_from_spec(MODEL_SPEC)
sys.modules[MODEL_SPEC.name] = MODEL
MODEL_SPEC.loader.exec_module(MODEL)

# Public reference tank inputs; no actual hull mass, inertia or restoring is used.
GEOMETRY = MODEL.UTubeGeometry(20.0, 6.0, 5.0, 5.0)
CONDUIT_AREA = 6.8
ALPHAS = (0.02, 0.05, 0.10, 0.15, 0.20)
CRITERIA = "generic-coupled-screening-v1"


def maximize(
    function: Callable[[float], float], periods: list[float],
) -> tuple[float, float, list[float]]:
    """Refine every sampled local maximum, retaining boundary candidates."""
    values = [function(period) for period in periods]
    candidates = list(zip(values, periods))
    golden = (math.sqrt(5) - 1) / 2
    for index in range(1, len(periods) - 1):
        if values[index] < max(values[index - 1], values[index + 1]):
            continue
        low, high = periods[index - 1], periods[index + 1]
        left, right = high - golden * (high - low), low + golden * (high - low)
        fleft, fright = function(left), function(right)
        for _ in range(36):
            if fleft < fright:
                low, left, fleft = left, right, fright
                right = low + golden * (high - low)
                fright = function(right)
            else:
                high, right, fright = right, left, fleft
                left = high - golden * (high - low)
                fleft = function(left)
        candidates.extend(((fleft, left), (fright, right)))
    peak, period = max(candidates)
    if period in (periods[0], periods[-1]):
        raise ValueError("Peak lies at a search boundary; expand the period band")
    return peak, period, values


def screen_case(
    roll_period: float, damping: float, alpha: float, *, samples: int = 121,
) -> dict[str, Any]:
    """Compare separate bare and coupled peaks under identical wave moment."""
    if (not all(math.isfinite(value) for value in (roll_period, damping, alpha))
            or roll_period <= 0 or not 0 < damping < 0.5 or not 0 <= alpha < 1
            or not isinstance(samples, int) or samples < 21):
        raise ValueError("Finite period, positive light damping and 0 <= alpha < 1 required")
    periods = [roll_period * (0.7 + 0.6 * index / (samples - 1))
               for index in range(samples)]
    common = dict(vessel_roll_period=roll_period, hull_damping_ratio=damping,
                  tank_authority=alpha, geometry=GEOMETRY, conduit_area=CONDUIT_AREA)
    wave_moment = 2 * damping * math.radians(5)
    stiffness = MODEL._quasi_static_moment_per_radian(
        GEOMETRY, CONDUIT_AREA, MODEL.DEFAULT_DENSITY, MODEL.DEFAULT_GRAVITY)
    residuals = []

    def response(period: float, enabled: bool = True) -> float:
        value: complex = MODEL.coupled_roll_response(
            forcing_period=period, tank_enabled=enabled, **common)
        if enabled and alpha:
            amplitude = abs(value) * wave_moment
            moment = MODEL.tank_moment(
                geometry=GEOMETRY, conduit_area=CONDUIT_AREA,
                forcing_period=period, roll_amplitude=amplitude)
            impedance = alpha * complex(moment.in_phase, moment.quadrature) / (
                stiffness * amplitude)
            ratio = roll_period / period
            denominator = 1 - ratio**2 + 2j * damping * ratio - impedance
            residual = abs(value * denominator - 1)
            residuals.append(residual)
            if not math.isfinite(residual) or residual >= 1e-5:
                raise ValueError("Coupled response fails the equilibrium residual criterion")
        return abs(value)

    bare_peak, bare_period, bare_values = maximize(
        lambda period: response(period, False), periods)
    tank_peak, tank_period, tank_values = maximize(response, periods)
    long_period = 60.0
    penalty = 100 * (response(long_period) / response(long_period, False) - 1)
    return {
        "roll_period_s": roll_period, "hull_damping_ratio": damping, "alpha": alpha,
        "bare_peak_rao": bare_peak, "bare_peak_period_s": bare_period,
        "tank_peak_rao": tank_peak, "tank_peak_period_s": tank_period,
        "reduction_percent": 100 * (1 - tank_peak / bare_peak),
        "forcing_periods_s": periods, "bare_rao": bare_values, "tank_rao": tank_values,
        "long_forcing_period_s": long_period, "long_period_penalty_percent": penalty,
        "max_relative_residual": max(residuals, default=0.0),
    }


def compute() -> dict[str, Any]:
    """Calculate the declared generic parameter matrix without fitting quotes."""
    cases = [screen_case(period, damping, alpha)
             for period in (18.0, 21.0) for damping in (0.02, 0.05) for alpha in ALPHAS]
    quotes = []
    for damping, quoted in ((0.02, 47.2), (0.05, 23.5)):
        row = next(row for row in cases if row["roll_period_s"] == 18.0
                   and row["hull_damping_ratio"] == damping and row["alpha"] == 0.05)
        calculated = row["reduction_percent"]
        quotes.append({"roll_period_s": 18.0, "hull_damping_ratio": damping,
                       "alpha": 0.05, "quoted_percent": quoted,
                       "computed_percent": calculated,
                       "difference_percentage_points": calculated - quoted})
    return {
        "criteria_revision": CRITERIA,
        "readiness": "screening estimate; engineering review pending",
        "source_classification": "Public source-neutral model; no private payload used",
        "model_revision": "public U-tube equations; source digests identify exact bytes",
        "assumptions": {
            "hull": "Generic normalized hull; no real hull data",
            "tank_geometry_basis": "Public reference tank inputs; calibration transfer assumed",
            "leg_length_m": 20.0, "leg_width_m": 6.0, "fill_depth_m": 5.0,
            "centroid_offset_m": 5.0, "conduit_area_m2": CONDUIT_AREA,
            "effective_conduit_length_m": GEOMETRY.effective_conduit_length,
            "loss_coefficient": MODEL.DEFAULT_LOSS_COEFFICIENT,
            "loss_area_exponent": MODEL.DEFAULT_AREA_EXPONENT,
            "density_kg_m3": MODEL.DEFAULT_DENSITY, "gravity_m_s2": MODEL.DEFAULT_GRAVITY,
            "bare_roll_reference_deg": 5.0,
            "forcing": "Constant within each comparison; proportional to hull damping between groups",
            "normalized_hull_restoring": 1.0,
            "alpha_definition": "Tank quasi-static moment stiffness / hull roll restoring",
            "period_band_over_roll_period": [0.7, 1.3],
            "samples": 121, "peak_refinement_iterations": 36,
            "equilibrium_residual_limit": 1e-5,
            "moment_validated_min_forcing_period_s": 20.0,
        },
        "assumption_units": {
            "leg_length_m": "m", "leg_width_m": "m", "fill_depth_m": "m",
            "centroid_offset_m": "m", "conduit_area_m2": "m2",
            "effective_conduit_length_m": "m", "loss_area_exponent": "1",
            "loss_coefficient": "m^(2*loss_area_exponent-4)",
            "density_kg_m3": "kg/m3", "gravity_m_s2": "m/s2",
            "bare_roll_reference_deg": "deg", "normalized_hull_restoring": "1",
            "period_band_over_roll_period": "1", "samples": "count",
            "peak_refinement_iterations": "count", "equilibrium_residual_limit": "1",
            "moment_validated_min_forcing_period_s": "s",
        },
        "units": {"roll_period_s": "s", "hull_damping_ratio": "1", "alpha": "1",
                  "bare_peak_rao": "1", "tank_peak_rao": "1",
                  "bare_peak_period_s": "s", "tank_peak_period_s": "s",
                  "forcing_periods_s": "s", "bare_rao": "1", "tank_rao": "1",
                  "reduction_percent": "%", "long_forcing_period_s": "s",
                  "long_period_penalty_percent": "%", "max_relative_residual": "1",
                  "difference_percentage_points": "percentage points"},
        "cases": cases, "quote_checks": quotes,
    }


def render(record: dict[str, Any]) -> str:
    """Render short technical prose and the full sensitivity table."""
    rows = []
    for row in record["cases"]:
        rows.append('<tr>' + ''.join(f'<td>{value}</td>' for value in (
            f'{row["roll_period_s"]:.0f}', f'{100 * row["hull_damping_ratio"]:.0f}',
            f'{row["alpha"]:.2f}', f'{row["reduction_percent"]:.1f}',
            f'{row["tank_peak_period_s"]:.1f}')) + '</tr>')
    quote_rows = []
    for row in record["quote_checks"]:
        quote_rows.append('<tr>' + ''.join(f'<td>{value}</td>' for value in (
            f'{100 * row["hull_damping_ratio"]:.0f}', f'{row["quoted_percent"]:.1f}',
            f'{row["computed_percent"]:.1f}',
            f'{row["difference_percentage_points"]:+.1f}')) + '</tr>')
    low, high = (row["computed_percent"] for row in record["quote_checks"])
    ratio = high / low
    return f'''<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Generic hull coupled roll screening estimate</title>
<link rel="stylesheet" href="../_assets/brand.css">
<style>body{{font-family:system-ui,sans-serif;line-height:1.6;max-width:960px;margin:auto;padding:24px;color:#16324f}}
table{{border-collapse:collapse;width:100%;font-variant-numeric:tabular-nums}}
th,td{{text-align:right;padding:6px;border-bottom:1px solid #ccd4dd}}
caption{{caption-side:bottom;text-align:left;padding:12px 0}}pre{{white-space:pre-wrap}}
.notice{{padding:16px;background:#fff0d0}}h2{{margin-top:2em}}</style></head>
<body><header><h1>Generic hull coupled roll screening estimate</h1>
<p class="subtitle">Generic hull roll-plus-tank screening estimate from a public coupled model.</p></header>
<h2>Summary</h2>
<p>The coupled model predicts a screening estimate for generic hulls. No result represents a real hull.</p>
<p class="notice">At 18 s, alpha 0.05 with no units, and A_c = 6.8 m², the screening estimates are {low:.1f} % and {high:.1f} %.
They replace proposed quotes of 47.2 % and 23.5 %, respectively. The quote table gives the differences.</p>
<h2>Revision history</h2>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead>
<tbody><tr><td>Initial</td><td>2026-10-24</td><td>Generic hull screening estimate added to distinguish model predictions from real-hull evidence.</td></tr></tbody></table>
<h2>Coupled method: screening estimate</h2>
<p>The model links hull roll to water transfer between connected tank legs. Tank moments feed back into the hull roll equation.
Alpha is the tank's quasi-static free-surface moment stiffness divided by the hull's roll restoring. Alpha has no units.</p>
<p>The public model includes vertical-centroid and local free-surface terms in that quasi-static stiffness.
Its denominator also includes water redistribution between the legs. Alpha therefore follows the complete public moment convention.</p>
<p>The normalized bare-hull impedance is D = 1 − (T₀/T)² + 2iζ(T₀/T).
The coupled impedance is D − αH/Kₛ. H is tank moment per roll angle; Kₛ is its quasi-static limit.</p>
<p>The hull equation uses normalized restoring of 1, with no units. The model sets forcing to give 5° bare roll at the nominal resonance.
Forcing stays fixed within each tank-on and tank-off comparison. Forcing changes between hull-damping groups because tank loss depends on amplitude.</p>
<p>The calculation finds each response peak separately over 0.7 to 1.3 times the hull roll period, with no units.
It refines all sampled local peaks. Reduction equals 100 % times one minus the coupled peak divided by the bare peak.</p>
<h2>Tank basis: screening estimate</h2>
<p>The calculation uses public reference tank inputs, with no actual hull geometry.
Each leg measures 20.0 m by 6.0 m. Fill depth is 5.0 m; centroid offset is 5.0 m.
Conduit area A_c is 6.8 m². The model supplies effective length, fluid properties and calibrated loss parameters.</p>
<p>The data record lists every coefficient and its units. Calibration transfer remains an assumption.
These screening estimates use the public reference tank, which differs from the illustrative tank on the linked study page.</p>
<h2>Peak roll reduction: screening estimate</h2>
<table><thead><tr><th>Hull roll period (s)</th><th>Hull damping (%)</th><th>Alpha (1)</th>
<th>Peak reduction (%)</th><th>Coupled peak forcing period (s)</th></tr></thead><tbody>
{chr(10).join(rows)}
</tbody><caption>Screening estimate sensitivity matrix. Tool: public U-tube model and the linked generator; exact source digests appear in the data record.</caption></table>
<p>At 18 s and alpha 0.05, with no units, increasing hull damping from 2 % to 5 % roughly halves the screening benefit.
The remaining benefit is {ratio:.1f} times the lower-damping benefit, with no units.</p>
<h2>Quote comparison: screening estimate</h2>
<p>Both comparisons use an 18 s generic hull period, alpha 0.05 with no units, and conduit area A_c of 6.8 m².</p>
<table><thead><tr><th>Hull damping (%)</th><th>Proposed quote (%)</th><th>Generated screening estimate (%)</th>
<th>Difference (percentage points)</th></tr></thead><tbody>
{chr(10).join(quote_rows)}
</tbody><caption>Screening estimate minus proposed quote, computed before rounding. No coefficient was fitted to either quote.</caption></table>
<h2>Long-period penalty: screening estimate</h2>
<p>Slow water transfer reduces the effective hull restoring. In the slow-forcing limit, the roll ratio approaches 1/(1−α), with no units.
At alpha 0.05, with no units, that limit gives 5.3 % more roll. A near-resonance reduction does not remove this penalty.</p>
<h2>Hand checks: screening estimate</h2>
<p>Alpha = 0, with no units, gives zero reduction, or 0 %.
An independent undamped box calculation checks the coupled denominator and both natural frequencies.</p>
<p>For that mathematical check, each leg measures 2 m by 1 m, with 1 m depth and 1 m offset.
The conduit has 1 m² area and 1 m effective length. Fluid density is 1 kg/m³; gravity is 1 m/s².</p>
<p>The hand calculation gives Kₛ = 53/6 N m/rad and H = 359/24 N m/rad at 0.5 rad/s.
At alpha 0.1, with no units, the response is 2120/1231 times normalized forcing, with no units.</p>
<h2>Validity limits: screening estimate</h2>
<p>The 18 s cases mildly extrapolate below the moment model's validated band of 20 s and longer.
Every sampled forcing period below 20 s lies outside that band, including parts of the 21 s sweep.
The published moment fit does not validate coupled hull reduction.</p>
<p>No tank-on and tank-off real-vessel comparison establishes these reductions. No full-scale roll reduction has been calculated here.
These screening estimates exclude sea-state weighting, structural loads and design qualification.</p>
<h2>Reproduce the screening estimate</h2>
<pre>python scripts/capabilities/build_generic_hull_screening.py
python -m pytest tests/hydrodynamics/test_generic_hull_screening.py</pre>
<p>The generator writes this page, the data record and the independent-oracle test from one script.
The test checks limiting cases, equilibrium residuals, frequency refinement and exact regeneration.</p>
<p><a href="data/generic_hull_roll_screening.json">Screening estimate data, arrays, units and source digests</a> ·
<a href="../../../scripts/capabilities/build_generic_hull_screening.py">Screening estimate generator and test source</a> ·
<a href="../../../src/digitalmodel/hydrodynamics/utube_tank.py">Public coupled model</a> ·
<a href="dual-tank-sloshing-cfd-study.html">Connected-tank study</a></p>
<h2>Left</h2>
<p>AI generated the script, calculations, tests and draft text. Engineering checks and acceptance remain pending.
P.E. sign-off remains pending; no engineer signature is asserted.</p>
<p>Technical-reviewer, claims, cold-read and Codex review passes remain pending for Mon 26 Oct 2026 18:00Z.</p>
<footer>Screening estimate only. Generated with Codex.</footer></body></html>
'''


def build(
    output_root: Path = ROOT, *, record: dict[str, Any] | None = None,
) -> None:
    """Write and read back the page, data and fixed independent-oracle tests."""
    output_root = Path(output_root).resolve()
    record = compute() if record is None else record
    page = render(record).encode("utf-8")
    tests = TEST_SOURCE.encode("utf-8")
    record = {**record, "provenance": {
        "generator_command": f"python {SCRIPT}",
        "input_sha256": {relative: hashlib.sha256((ROOT / relative).read_bytes()).hexdigest()
                         for relative in (SCRIPT, MODEL_PATH)},
        "output_sha256": {PAGE: hashlib.sha256(page).hexdigest(),
                          TEST: hashlib.sha256(tests).hexdigest()},
        "data_digest_scope": "Data bytes are checked by deterministic regeneration; no self digest",
    }}
    payloads = {PAGE: page, TEST: tests,
                DATA: (json.dumps(record, indent=2, allow_nan=False) + "\n").encode("utf-8")}
    for relative, payload in payloads.items():
        target = output_root / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(payload)
        if target.read_bytes() != payload:
            raise OSError("Generated artifact readback failed")


def main() -> None:
    """Generate the three declared artifacts in the selected output tree."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-root", type=Path, default=ROOT)
    args = parser.parse_args()
    start = perf_counter()
    build(args.output_root)
    print(f"Generated three screening artifacts in {perf_counter() - start:.2f} s")


TEST_SOURCE = r'''# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent checks generated by the generic hull screening script."""

import hashlib
import importlib.util
import json
import math
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts/capabilities/build_generic_hull_screening.py"


@pytest.fixture(scope="module")
def builder():
    spec = importlib.util.spec_from_file_location("generic_screening", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


@pytest.fixture(scope="module")
def record(builder):
    return builder.compute()


def test_zero_authority_has_zero_reduction(builder):
    result = builder.screen_case(18.0, 0.02, 0.0)
    assert result["reduction_percent"] == pytest.approx(0.0, abs=1e-9)
    assert result["tank_peak_period_s"] == pytest.approx(result["bare_peak_period_s"])


def test_undamped_two_degree_hand_check(builder):
    """Unit-density/gravity box: Kfixed=29/6, Ktransfer=4, J=19/2.

    At omega=1/2 rad/s and hull omega=1 rad/s, H=359/24.
    Kstatic=53/6; alpha=1/10 gives roll/moment=2120/1231.
    The characteristic polynomial is 946*x*x-1529*x+477.
    These rational values come from prismatic integrals, not model output.
    """
    model = builder.MODEL
    geometry = model.UTubeGeometry(2.0, 1.0, 1.0, 1.0, 1.0)
    moment = model.tank_moment(
        geometry=geometry, conduit_area=1.0, forcing_period=4 * math.pi,
        roll_amplitude=1e-7, density=1.0, gravity=1.0,
        loss_coefficient=1e-20,
    )
    assert moment.in_phase / 1e-7 == pytest.approx(359 / 24, rel=1e-10)
    assert abs(moment.quadrature / 1e-7) < 1e-10
    response = 1 / (0.75 - 0.1 * (moment.in_phase / 1e-7) / (53 / 6))
    assert response == pytest.approx(2120 / 1231, rel=1e-10)
    for sign in (-1, 1):
        x = (1529 + sign * math.sqrt(532873)) / 1892
        period = 2 * math.pi / math.sqrt(x)
        h = model.tank_moment(
            geometry=geometry, conduit_area=1.0, forcing_period=period,
            roll_amplitude=1e-7, density=1.0, gravity=1.0,
            loss_coefficient=1e-20,
        ).in_phase / 1e-7
        assert 1 - x - 0.1 * h / (53 / 6) == pytest.approx(0, abs=1e-10)


def test_long_period_free_surface_limit(builder):
    args = dict(vessel_roll_period=18.0, hull_damping_ratio=0.02,
                tank_authority=0.05, geometry=builder.GEOMETRY,
                conduit_area=builder.CONDUIT_AREA)
    bare = abs(builder.MODEL.coupled_roll_response(
        forcing_period=1e6, tank_enabled=False, **args))
    tank = abs(builder.MODEL.coupled_roll_response(forcing_period=1e6, **args))
    assert tank / bare == pytest.approx(1 / 0.95, rel=1e-5)


@pytest.mark.parametrize("period,damping,alpha", [
    (18.0, 0.02, 0.02), (18.0, 0.02, 0.05), (18.0, 0.02, 0.20),
    (21.0, 0.05, 0.02), (21.0, 0.05, 0.05), (21.0, 0.05, 0.20),
])
def test_peak_refinement_and_bare_closed_form(builder, period, damping, alpha):
    coarse = builder.screen_case(period, damping, alpha)
    fine = builder.screen_case(period, damping, alpha, samples=241)
    assert coarse["bare_peak_rao"] == pytest.approx(
        1 / (2 * damping * math.sqrt(1 - damping**2)), rel=1e-9)
    assert coarse["bare_peak_period_s"] == pytest.approx(
        period / math.sqrt(1 - 2 * damping**2), rel=1e-7)
    assert abs(coarse["reduction_percent"] - fine["reduction_percent"]) < 0.1
    assert abs(coarse["tank_peak_period_s"] - fine["tank_peak_period_s"]) < 0.01


def test_unconverged_response_is_rejected(builder, monkeypatch):
    original = builder.MODEL.coupled_roll_response

    def perturbed(**kwargs):
        return 1.01 * original(**kwargs)

    monkeypatch.setattr(builder.MODEL, "coupled_roll_response", perturbed)
    with pytest.raises(ValueError, match="residual"):
        builder.screen_case(18, .02, .05)


def test_table_basis_and_quote_comparison(record):
    assert len(record["cases"]) == 20
    assert {row["roll_period_s"] for row in record["cases"]} == {18.0, 21.0}
    assert {row["hull_damping_ratio"] for row in record["cases"]} == {0.02, 0.05}
    assert {row["alpha"] for row in record["cases"]} == {0.02, 0.05, 0.1, 0.15, 0.2}
    for row in record["cases"]:
        expected = 100 * (1 - row["tank_peak_rao"] / row["bare_peak_rao"])
        assert row["reduction_percent"] == pytest.approx(expected)
        assert row["max_relative_residual"] < 1e-5
        assert len(row["forcing_periods_s"]) == len(row["tank_rao"]) == 121
    for quote in record["quote_checks"]:
        assert quote["difference_percentage_points"] == pytest.approx(
            quote["computed_percent"] - quote["quoted_percent"])


def test_invalid_screen_inputs_are_rejected(builder):
    for values in [(0, .02, .05), (18, 0, .05), (18, .02, -1),
                   (math.nan, .02, .05), (18, math.inf, .05), (18, .02, 1)]:
        with pytest.raises(ValueError):
            builder.screen_case(*values)
    with pytest.raises(ValueError):
        builder.screen_case(18, .02, .05, samples=2)


def test_generator_reproduces_all_three_outputs(builder, record, tmp_path):
    builder.build(tmp_path, record=record)
    for relative in (builder.PAGE, builder.DATA, builder.TEST):
        assert (tmp_path / relative).read_bytes() == (ROOT / relative).read_bytes()
    saved = json.loads((tmp_path / builder.DATA).read_text())
    for relative, digest in saved["provenance"]["output_sha256"].items():
        assert hashlib.sha256((tmp_path / relative).read_bytes()).hexdigest() == digest
    for relative, digest in saved["provenance"]["input_sha256"].items():
        assert hashlib.sha256((ROOT / relative).read_bytes()).hexdigest() == digest


def test_page_wording_links_and_preservation(builder, record):
    from html.parser import HTMLParser
    from urllib.parse import urlsplit

    class Links(HTMLParser):
        def __init__(self):
            super().__init__()
            self.links = []

        def handle_starttag(self, tag, attrs):
            self.links.extend(value for key, value in attrs if key == "href")

    text = builder.render(record)
    assert '<title>Generic hull coupled roll screening estimate</title>' in text
    assert 'Generic hull roll-plus-tank screening estimate' in text
    assert '18 s cases mildly extrapolate' in text
    assert 'No tank-on and tank-off real-vessel comparison' in text
    assert 'P.E. sign-off remains pending' in text
    assert 'AI generated' in text
    assert 'Revision history' in text
    assert 'Quote comparison' in text
    assert '\u2014' not in text
    for folder in ("home", "mnt"):
        assert "/" + folder + "/" not in text
    links = Links()
    links.feed(text)
    page = ROOT / builder.PAGE
    for href in links.links:
        url = urlsplit(href)
        if not url.scheme:
            assert (page.parent / url.path).is_file(), href
    parent = (ROOT / builder.PARENT_PAGE).read_text()
    assert f'href="{Path(builder.PAGE).name}"' in parent
'''


if __name__ == "__main__":
    main()
