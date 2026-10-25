# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Research audit tests; synthetic fixtures are not qualification evidence."""

import builtins
import copy
import hashlib
import importlib.util
import json
import math
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
sys.path.insert(0, str(ROOT / "scripts/studies"))
SPEC = importlib.util.spec_from_file_location(
    "surface_crack_verdict", ROOT / "scripts/studies/surface_crack_verdict.py"
)
verdict = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(verdict)

RESULTS = ROOT / "docs/domains/asset-integrity/surface-crack-calculix-results.json"
# Study comparator, not a code acceptance limit: existing research receipt.
LIMIT = json.loads(RESULTS.read_text())["comparison_tolerance"]
REGENERATE = (
    "python scripts/studies/surface_crack_verdict.py "
    "--results docs/domains/asset-integrity/surface-crack-calculix-results.json "
    "--relative-limit 0.05 --sources <source-receipts.json> "
    "--receipt-date 2026-10-11 --report-date 2026-10-24 "
    "--output-json docs/domains/asset-integrity/surface-crack-solver-audit.json "
    "--output-html docs/domains/asset-integrity/surface-crack-solver-analysis.html"
)


def test_reference_against_independent_hand_calculation():
    """NASA TM-83200 (1981), pp. 4, 7, 12, Eqs. 2a, 10-11, 16-21.

    Synthetic study inputs: a=c=5 mm, t=10 mm, b=25 mm, S=100 MPa.
    At a/c=1: Q=2.464, M1=1.04, M2=-0.54+0.89/1.2,
    M3=0.5-1/1.65, f_phi=1. At depth, g=1; at surface,
    g=1+0.1+0.35*(0.5)^2=1.1875. Hence K_surface/K_depth=1.1875.
    K_depth = 100*(1.04+M2/4+M3/16)*sqrt(sec(pi*sqrt(0.5)/10))
              *sqrt(pi*0.005/2.464) = 8.7616605877 MPa sqrt(m).
    This arithmetic is independent of the implementation being checked.
    """
    m2 = -0.54 + 0.89 / 1.2
    m3 = 0.5 - 1 / 1.65
    width_factor = math.sqrt(1 / math.cos(math.pi * math.sqrt(0.5) / 10))
    deep = 100 * (1.04 + m2 / 4 + m3 / 16) * width_factor
    deep *= math.sqrt(math.pi * 0.005 / 2.464)
    assert verdict.reference_k(90) == pytest.approx(deep)
    assert verdict.reference_k(0) == pytest.approx(deep * 1.1875)


def test_historical_receipts_are_recomputed_and_never_qualified():
    """Signed difference is defined as (candidate-reference)/reference.

    Finest recorded depth values: 7.718447812807262 and 8.76166058773269.
    Independent division gives -0.11906564565923079, or -11.9 percent.
    This checks receipt arithmetic, not the accuracy of the original solve.
    """
    data = json.loads(RESULTS.read_text())
    # Corrupt derived values: the audit must use actual front samples instead.
    data["runs"][-1]["deepest_difference"] = 0
    data["runs"][-1]["max_abs_front_difference"] = 0
    for row in data["runs"][-1]["front"]:
        row["relative_difference"] = 0
        row["reference_k"] = 1
    result = verdict.audit(data, relative_limit=LIMIT)
    assert result["runs"][-1]["deepest_difference"] == pytest.approx(
        (7.718447812807262 - 8.76166058773269) / 8.76166058773269
    )
    assert result["runs"][-1]["comparison"] == "EXCEEDS_RESEARCH_COMPARATOR"
    assert result["solver_selection"] == "NOT_EVALUATED"
    assert result["qualified_results"] == 0
    assert result["runs"][-1]["native_outputs"] == "NOT_REVALIDATED"
    assert result["code_aster"]["comparison"] == "NOT_EVALUATED"
    assert result["licensed_comparison"] == "NOT_EVALUATED"


def test_exact_reference_agreement_cannot_qualify_a_solver():
    """A constructed exact K fixture supplies no native J or contour evidence."""
    data = json.loads(RESULTS.read_text())
    for run in data["runs"]:
        for row in run["front"]:
            row["k_mpa_sqrt_m"] = verdict.reference_k(row["phi_deg"])
    result = verdict.audit(data, relative_limit=LIMIT)
    assert all(r["comparison"] == "WITHIN_RESEARCH_COMPARATOR"
               for r in result["runs"])
    assert result["qualified_results"] == 0
    assert result["solver_selection"] == "NOT_EVALUATED"
    assert all(r["native_j"] == "NOT_EVALUATED" for r in result["runs"])


@pytest.mark.parametrize("limit", [0, -1, math.nan, math.inf, True])
def test_rejects_invalid_comparator(limit):
    with pytest.raises(ValueError, match="relative_limit"):
        verdict.audit(json.loads(RESULTS.read_text()), relative_limit=limit)


@pytest.mark.parametrize("field,value", [
    ("k_mpa_sqrt_m", math.nan), ("k_mpa_sqrt_m", -1),
    ("phi_deg", math.inf), ("phi_deg", -1), ("phi_deg", 91),
])
def test_rejects_invalid_front_samples(field, value):
    data = json.loads(RESULTS.read_text())
    data["runs"][0]["front"][0][field] = value
    with pytest.raises(ValueError):
        verdict.audit(data, relative_limit=LIMIT)


@pytest.mark.parametrize("damage", ["empty", "missing_endpoint", "duplicate"])
def test_rejects_incomplete_or_ambiguous_front(damage):
    data = json.loads(RESULTS.read_text())
    front = data["runs"][0]["front"]
    if damage == "empty":
        front.clear()
    elif damage == "missing_endpoint":
        front.pop()
    else:
        front.append(copy.deepcopy(front[0]))
    with pytest.raises(ValueError):
        verdict.audit(data, relative_limit=LIMIT)


def test_missing_or_failed_runs_do_not_become_zero_error_results():
    data = json.loads(RESULTS.read_text())
    data["runs"][0]["returncode"] = "timeout"
    data["runs"][0].pop("front")
    result = verdict.audit(data, relative_limit=LIMIT)
    assert result["runs"][0]["comparison"] == "NOT_EVALUATED"
    assert result["runs"][0]["max_abs_front_difference"] is None
    data["runs"].clear()
    with pytest.raises(ValueError, match="runs"):
        verdict.audit(data, relative_limit=LIMIT)


def test_rejects_geometry_substitution():
    data = json.loads(RESULTS.read_text())
    data["benchmark"]["a_mm"] *= 2
    with pytest.raises(ValueError, match="benchmark"):
        verdict.audit(data, relative_limit=LIMIT)


@pytest.mark.parametrize("field,value", [
    ("nodes", 0), ("elements", -1), ("wall_seconds", math.nan),
    ("peak_rss_kib", -1), ("refinement", 0),
])
def test_rejects_invalid_cost_or_mesh_receipts(field, value):
    data = json.loads(RESULTS.read_text())
    data["runs"][0][field] = value
    with pytest.raises(ValueError):
        verdict.audit(data, relative_limit=LIMIT)


def test_saved_audit_binds_input_bytes_and_reproduces_report():
    base = ROOT / "docs/domains/asset-integrity"
    saved = json.loads((base / "surface-crack-solver-audit.json").read_text())
    assert saved["input_sha256"] == hashlib.sha256(RESULTS.read_bytes()).hexdigest()
    reference = saved["reference_implementation"]
    assert reference["sha256"] == hashlib.sha256(
        (ROOT / reference["path"]).read_bytes()).hexdigest(), (
            f"Reference implementation changed. Regenerate with: {REGENERATE}")
    actual = verdict.audit(json.loads(RESULTS.read_text()), relative_limit=LIMIT)
    assert saved["assessment"] == actual
    assert (base / "surface-crack-solver-analysis.html").read_text() == verdict.render(saved)


def test_failed_last_run_is_rendered_as_not_evaluated():
    data = json.loads(RESULTS.read_text())
    data["runs"][-1]["returncode"] = "timeout"
    report = verdict.render({
        "assessment": verdict.audit(data, relative_limit=LIMIT),
        "input_sha256": "synthetic", "sources": [],
        "receipt_date": "2026-10-11", "report_date": "2026-10-24",
    })
    assert "finest recorded deepest K is NOT_EVALUATED" in report
    assert "comparison status is NOT_EVALUATED" in report


@pytest.mark.parametrize("collision", ["results", "sources", "outputs"])
def test_cli_preserves_inputs_and_requires_distinct_outputs(tmp_path, monkeypatch, collision):
    """Output path collisions must be rejected before any input is overwritten."""
    inputs = tmp_path / "results.json"
    sources = tmp_path / "sources.json"
    inputs.write_bytes(RESULTS.read_bytes())
    sources.write_text("[]")
    before = {p: p.read_bytes() for p in (inputs, sources)}
    output_json = tmp_path / "audit.json"
    output_html = tmp_path / "report.html"
    if collision == "results":
        output_json = inputs
    elif collision == "sources":
        output_html = sources
    else:
        output_html = output_json
    monkeypatch.setattr(sys, "argv", [
        "surface_crack_verdict.py", "--results", str(inputs),
        "--sources", str(sources), "--relative-limit", str(LIMIT),
        "--output-json", str(output_json), "--output-html", str(output_html),
        "--receipt-date", "2026-10-11", "--report-date", "2026-10-24",
    ])
    with pytest.raises(ValueError, match="distinct"):
        verdict.main()
    assert {p: p.read_bytes() for p in before} == before


def test_offline_import_does_not_require_posix_resource(monkeypatch):
    original_import = builtins.__import__

    def without_resource(name, *args, **kwargs):
        if name == "resource":
            raise ImportError("POSIX resource module unavailable")
        return original_import(name, *args, **kwargs)

    monkeypatch.delitem(sys.modules, "surface_crack_post", raising=False)
    monkeypatch.setattr(builtins, "__import__", without_resource)
    portable = importlib.util.module_from_spec(SPEC)
    SPEC.loader.exec_module(portable)
    assert portable.reference_k(90) == pytest.approx(verdict.reference_k(90))


@pytest.mark.parametrize("returncode", [0, "timeout"])
def test_missing_rss_preserves_run_disposition(returncode):
    data = json.loads(RESULTS.read_text())
    data["runs"][-1].update(returncode=returncode, peak_rss_kib=None)
    result = verdict.audit(data, relative_limit=LIMIT)
    run = result["runs"][-1]
    assert run["peak_rss_kib"] is None
    assert run["cost"] == "NOT_EVALUATED"
    assert run["comparison"] == (
        "EXCEEDS_RESEARCH_COMPARATOR" if returncode == 0 else "NOT_EVALUATED")
    report = verdict.render({
        "assessment": result, "input_sha256": "synthetic", "sources": [],
        "receipt_date": "2026-10-11", "report_date": "2026-10-24",
    })
    assert "<td>NOT_EVALUATED</td>" in report
    assert "<td>19384</td>" in report
    assert "<td>19384.0</td>" not in report


@pytest.mark.parametrize("interior_count", [0, 1, 2])
def test_front_requires_at_least_two_interior_stations(interior_count):
    data = json.loads(RESULTS.read_text())
    front = data["runs"][0]["front"]
    data["runs"][0]["front"] = [front[0], *front[1:1+interior_count], front[-1]]
    if interior_count < 2:
        with pytest.raises(ValueError, match="interior"):
            verdict.audit(data, relative_limit=LIMIT)
    else:
        result = verdict.audit(data, relative_limit=LIMIT)
        assert result["runs"][0]["front_points"] == 4
        assert result["qualified_results"] == 0


def cli_outputs(tmp_path, monkeypatch, sources):
    """Prepare an offline CLI invocation with explicit evidence dates."""
    source_path = tmp_path / "sources.json"
    source_path.write_text(json.dumps(sources), encoding="utf-8")
    output_json, output_html = tmp_path / "audit.json", tmp_path / "report.html"
    monkeypatch.setattr(sys, "argv", [
        "surface_crack_verdict.py", "--results", str(RESULTS),
        "--sources", str(source_path), "--relative-limit", str(LIMIT),
        "--output-json", str(output_json), "--output-html", str(output_html),
        "--receipt-date", "2025-03-04", "--report-date", "2026-11-02",
    ])
    return output_json, output_html


def test_cli_uses_explicit_dates_and_utf8(tmp_path, monkeypatch):
    source = {
        "url": "https://example.org/reference", "title": "Caf\u00e9",
        "edition": "synthetic", "retrieved_utc": "2026-10-24T00:00:00Z",
        "http_status": "<script>untrusted</script>", "reuse": "synthetic",
    }
    output_json, output_html = cli_outputs(tmp_path, monkeypatch, [source])
    read_text, write_text = Path.read_text, Path.write_text

    def utf8_read(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        return read_text(path, *args, **kwargs)

    def utf8_write(path, *args, **kwargs):
        assert kwargs.get("encoding") == "utf-8"
        return write_text(path, *args, **kwargs)

    with monkeypatch.context() as encoding_patch:
        encoding_patch.setattr(Path, "read_text", utf8_read)
        encoding_patch.setattr(Path, "write_text", utf8_write)
        verdict.main()
    saved = json.loads(output_json.read_bytes())
    report = output_html.read_bytes().decode("utf-8")
    assert saved["receipt_date"] == "2025-03-04"
    assert saved["report_date"] == "2026-11-02"
    assert "costs from the 2025-03-04 receipt" in report
    assert "<td>2</td><td>2026-11-02</td>" in report
    assert "<td>1</td><td>2026-10-20</td>" in report
    assert "Caf\u00e9" in report
    assert "HTTP &lt;script&gt;untrusted&lt;/script&gt;" in report
    assert "<script>untrusted</script>" not in report
    assert "g = 1.1875" in report
    assert "K_surf = 10.4 MPa sqrt(m)" in report
    assert "reference fit scatter" in saved["assessment"]["limit_basis"]
    assert "reference fit scatter" in report


@pytest.mark.parametrize("existing", [False, True])
def test_cli_render_failure_preserves_both_outputs(tmp_path, monkeypatch, existing):
    outputs = cli_outputs(tmp_path, monkeypatch, [{}])
    if existing:
        for path in outputs:
            path.write_text("retained output", encoding="utf-8")
    before = {p: p.read_bytes() if p.exists() else None for p in outputs}
    with pytest.raises(KeyError):
        verdict.main()
    assert {p: p.read_bytes() if p.exists() else None for p in outputs} == before


@pytest.mark.parametrize("option,value", [
    ("--receipt-date", "2025-02-30"), ("--report-date", "not-a-date"),
    ("--report-date", "20261024"),
])
def test_cli_rejects_invalid_dates_before_writing(tmp_path, monkeypatch, option, value):
    outputs = cli_outputs(tmp_path, monkeypatch, [])
    sys.argv[sys.argv.index(option)+1] = value
    with pytest.raises(SystemExit):
        verdict.main()
    assert not any(path.exists() for path in outputs)
