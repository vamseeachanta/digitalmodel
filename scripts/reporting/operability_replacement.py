"""Rebuild synthetic operability software checks; no engineering acceptance.

The entry point uses the standard library and imports the actual calculation
file. That file requires NumPy and Pydantic; no solver package is imported.
"""

import argparse
import hashlib
import html
import importlib.util
import json
import math
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path
from types import ModuleType


ROOT = Path(__file__).resolve().parents[2]
CODE_PATHS = (
    "src/digitalmodel/orcaflex/weather_window.py",
    "tests/orcaflex/test_weather_window.py",
    "scripts/reporting/operability_replacement.py",
)
CRITERIA = {"hs_limit": 2.0, "tp_limit": 8.0,
            "wind_limit_mps": 10.0, "current_limit_mps": 1.0}
SOURCE_RECEIPTS = [
    {"key": "N", "source_id": "noaa-ndbc",
     "url": "https://www.ndbc.noaa.gov/faq/measdes.shtml",
     "retrieved_utc": "2026-10-25T06:41:31Z", "http_status": 200, "bytes": 51265,
     "sha256": "e16d1ed2d813778892915a6c83ea41b3db3161bd5a1227281e4b75b6c77206ba",
     "basis": "Realtime MM missing; UTC; WSPD m/s; wind from true north; DPD remains dominant period."},
    {"key": "A", "source_id": "coops",
     "url": "https://api.tidesandcurrents.noaa.gov/api/prod/",
     "retrieved_utc": "2026-10-25T06:41:42.682296Z", "http_status": 200, "bytes": 42037,
     "sha256": "2f35c273fdb3eeca2aa98111d0ed62998500c2ed9f4c507b0543d919dd6b4da5",
     "basis": "Explicit metric/gmt request; current cm/s divided by 100; requested bin is an identifier."},
    {"key": "R", "source_id": "coops",
     "url": "https://api.tidesandcurrents.noaa.gov/api/prod/responseHelp.html",
     "retrieved_utc": "2026-10-25T06:41:42.938084Z", "http_status": 200, "bytes": 23254,
     "sha256": "c934a7921f56b80bbd5b05701795357a97cb5cb3c692931c3d96e8194976ba69",
     "basis": "Core current fields t, s, d, b; no depth inferred from bin."},
    {"key": "P", "source_id": "noaa-ndbc",
     "url": "https://www.weather.gov/disclaimer",
     "retrieved_utc": "2026-10-25T06:41:43.338442Z", "http_status": 200, "bytes": 41679,
     "sha256": "8919bb160c331dbee30b25a03f3061b9f934bf05bb684c62fd1680e44ae43b8f",
     "basis": "NWS public-domain material subject to stated exceptions; identify source; no endorsement or altered official data."},
    {"key": "Q", "source_id": "coops",
     "url": "https://tidesandcurrents.noaa.gov/disclaimers.html",
     "retrieved_utc": "2026-10-25T06:41:43.790530Z", "http_status": 200, "bytes": 24039,
     "sha256": "4af265156e195fda2a8c4457af5bd2f73b74d70cd9219f3a4ae94d2d2543363c",
     "basis": "NOS attribution requested; altered content is independent; third-party exclusions remain."},
]
GAPS = {
    "joint_history": {"status": "UNVERIFIED", "disposition":
        "No synchronized environmental history is supplied; only synthetic co-timed software samples are admitted."},
    "site_operating_limits": {"status": "UNVERIFIED", "disposition":
        "Approved engineering limits are absent; synthetic-v1 thresholds are software fixtures only."},
    "current_deployment_depth": {"status": "UNVERIFIED", "disposition":
        "Deployment depth is absent; a bin identifier does not qualify current engineering input."},
    "standards_derived_acceptance": {"status": "UNVERIFIED", "disposition":
        "Standards-derived criteria and fixtures are excluded; no standards compliance is evaluated."},
}


def calculation_module() -> ModuleType:
    """Load the production file without executing optional solver-package imports."""
    name = "operability_calculation"
    spec = importlib.util.spec_from_file_location(name, ROOT / CODE_PATHS[0])
    if spec is None or spec.loader is None:
        raise ValueError("calculation module cannot be loaded")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def synthetic_inputs() -> list[dict]:
    """Return the six versioned, duration-explicit acceptance rows."""
    values = [(1.0, 7.0, 8.0, 0.5), (2.0, 8.0, 10.0, 1.0),
              (1.0, 7.0, 10.1, 0.5), (1.0, 7.0, 8.0, 1.1),
              (2.1, 7.0, 8.0, 0.5), (1.0, 8.1, 8.0, 0.5)]
    rows = []
    for index, (hs, tp, wind, current) in enumerate(values):
        rows.append({
            "case": chr(ord("A") + index),
            "start_utc": (datetime(2000, 1, 1, tzinfo=timezone.utc)
                          + timedelta(hours=index)).isoformat().replace("+00:00", "Z"),
            "duration_hours": 1.0, "hs_m": hs, "tp_s": tp,
            "wind_mps": wind, "current_mps": current,
            "units": {"duration_hours": "h", "hs_m": "m", "tp_s": "s",
                      "wind_mps": "m/s", "current_mps": "m/s"},
            "provenance": {key: {"source_id": "synthetic", "record_id": chr(ord("A") + index),
                                 "quality": "synthetic"}
                           for key in ("hs_m", "tp_s", "wind_mps", "current_mps")},
        })
    return rows


def result_summary(result: dict) -> dict:
    """Preserve durations and fraction; round percentages only for presentation."""
    keys = ("passing_duration_hours", "observed_duration_hours", "missing_coverage_hours",
            "sample_passes", "window_durations_hours", "engineering_readiness")
    summary = {key: result[key] for key in keys}
    summary["passing_fraction"] = {"numerator_hours": result["passing_duration_hours"],
                                   "denominator_hours": result["observed_duration_hours"]}
    summary["operability_pct_display"] = f'{result["operability_pct"]:.1f}'
    summary["non_operability_pct_display"] = f'{result["non_operability_pct"]:.1f}'
    return summary


def build_audit() -> dict:
    """Check unrounded production results before emitting the audit record."""
    module = calculation_module()
    rows = synthetic_inputs()
    samples = [{key: value for key, value in row.items() if key != "case"} for row in rows]
    joint = module.OperabilityInput(samples=samples, **CRITERIA).calculate_operability()
    wave = module.OperabilityInput(samples=samples, hs_limit=2.0, tp_limit=8.0).calculate_operability()
    for result, expected in ((joint, 2.0), (wave, 4.0)):
        if (result["passing_duration_hours"] != expected
                or result["observed_duration_hours"] != 6.0
                or not math.isclose(result["operability_pct"], 100 * expected / 6)):
            raise ValueError("synthetic duration acceptance failed")
    if joint["sample_passes"] != [True, True, False, False, False, False]:
        raise ValueError("synthetic joint mask acceptance failed")
    current = module.parse_coops_currents(
        b'{"data":[{"t":"2000-01-01 00:00","s":"100.0","d":"90.0","b":"1"}]}',
        provenance={"source_id": "coops", "record_id": "synthetic-conversion", "quality": "synthetic"},
        units="metric", time_zone="gmt", expected_bin=1)
    if current[0]["current_mps"] != 1.0:
        raise ValueError("synthetic current conversion acceptance failed")
    encoded_inputs = json.dumps(rows, sort_keys=True, separators=(",", ":")).encode()
    return {
        "title": "Synchronized operability software analysis", "software_check": "PASS",
        "summary": "Synthetic joint limits pass 2 h of 6 h (33.3%); wave-only limits pass 4 h of 6 h (66.7%).",
        "revision_history": [{"revision": "0", "date": "2026-10-25",
                              "change": "Joint wind/current software checks are introduced to prevent ignored limits."}],
        "criteria_revision": "synthetic-v1", "criteria": CRITERIA,
        "criteria_units": {"hs_limit": "m", "tp_limit": "s", "wind_limit_mps": "m/s", "current_limit_mps": "m/s"},
        "inputs": rows, "input_sha256": hashlib.sha256(encoded_inputs).hexdigest(),
        "joint": result_summary(joint), "wave_only": result_summary(wave),
        "parser_check": {"kind": "synthetic supplied bytes", "input_cm_per_s": 100.0,
                         "output_m_per_s": current[0]["current_mps"], "status": "PASS"},
        "parser_source_acceptance": {
            "ndbc_realtime_fields_units_missing": {"status": "CONFIRMED", "receipts": ["N"]},
            "coops_fields_units_bin": {"status": "CONFIRMED", "receipts": ["A", "R"]},
            "coops_missing_values": {"status": "BLOCKED-IF-MISSING", "reason":
                "The retrieved API and response guides do not specify missing-value tokens. Empty/null preservation is a defensive software policy; source-specific missing-value acceptance requires authoritative evidence."},
        },
        "calculation_basis": "Each enabled upper limit passes at equality. Passing hours are summed from the joint sample mask; marginals are never multiplied.",
        "coverage_basis": "Observed duration excludes interior gaps; windows break at gaps and failures. Coverage outside the first-start/last-end span is unknown.",
        "engineering_readiness": "UNVERIFIED", "gaps": GAPS,
        "parser_limits": ["NDBC realtime standard-met only; MM stays missing; historical numeric sentinels are rejected.",
                          "NDBC gust is not WSPD; dominant period is not promoted to peak period; no height or averaging correction is applied.",
                          "CO-OPS core data rows t/s/d/b only; metric/gmt and one explicit positive bin are required; empty/null stays missing.",
                          "Parser output is not a qualified synchronized sample or a measured interval. Provenance and raw-input digest are retained."],
        "source_receipts": SOURCE_RECEIPTS,
        "source_reuse": "NOAA NDBC and CO-OPS documentation supplies field semantics only. All numerical fixtures are independently synthetic, not official NOAA observations. No endorsement is implied; no third-party material or raw observed series is reproduced.",
        "freshness": "Receipts record documentation retrieved on the stated dates; no live endpoint behavior is guaranteed.",
        "code_sha256": {path: hashlib.sha256((ROOT / path).read_bytes()).hexdigest() for path in CODE_PATHS},
    }


def render_html(audit: dict) -> str:
    """Render the saved synthetic rows and the same qualification dispositions."""
    def escape(value: object) -> str:
        return html.escape(str(value), quote=True)
    parts = ['<!doctype html><html lang="en"><head><meta charset="utf-8">',
             '<meta name="viewport" content="width=device-width, initial-scale=1">',
             f'<title>{escape(audit["title"])}</title>',
             '<style>body{font:16px/1.5 system-ui;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#172b3a}table{border-collapse:collapse;width:100%;margin-top:1rem}th,td{border:1px solid #bccbd4;padding:.4rem;text-align:left}code{overflow-wrap:anywhere}a{color:#075b80}</style>',
             '</head><body>', f'<h1>{escape(audit["title"])}</h1>',
             f'<h2>Summary</h2><p>{escape(audit["summary"])} Engineering readiness remains UNVERIFIED.</p>',
             '<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>']
    for row in audit["revision_history"]:
        parts.append('<tr>' + ''.join(f'<td>{escape(row[key])}</td>' for key in ("revision", "date", "change")) + '</tr>')
    parts.extend(['</table><p>Table 1. Issued revisions.</p>',
                  '<h2>Synthetic results</h2><table><tr><th>Criteria</th><th>Passing duration (h)</th><th>Observed duration (h)</th><th>Passing (%)</th></tr>'])
    for label, key in (("Joint", "joint"), ("Wave only", "wave_only")):
        row = audit[key]
        parts.append(f'<tr><td>{label}</td><td>{row["passing_duration_hours"]:.1f}</td><td>{row["observed_duration_hours"]:.1f}</td><td>{row["operability_pct_display"]}</td></tr>')
    parts.extend(['</table><p>Table 2. Software fixtures, with unrounded acceptance arithmetic.</p>',
                  '<h2>Input rows</h2><table><tr><th>Case</th><th>Start (UTC)</th><th>Duration (h)</th><th>Hs (m)</th><th>Tp (s)</th><th>Wind (m/s)</th><th>Current (m/s)</th><th>Joint pass</th></tr>'])
    for row, passed in zip(audit["inputs"], audit["joint"]["sample_passes"]):
        cells = [row["case"], row["start_utc"], *(f'{row[key]:.1f}' for key in ("duration_hours", "hs_m", "tp_s", "wind_mps", "current_mps")), str(passed)]
        parts.append('<tr>' + ''.join(f'<td>{escape(cell)}</td>' for cell in cells) + '</tr>')
    parts.extend(['</table><p>Table 3. Synthetic co-timed input; provenance is synthetic for every variable.</p>',
                  '<h2>Criteria and calculation basis</h2>',
                  '<p>synthetic-v1: Hs ≤ 2.0 m; Tp ≤ 8.0 s; wind ≤ 10.0 m/s; current ≤ 1.0 m/s. These thresholds are software fixtures.</p>',
                  f'<p>{escape(audit["calculation_basis"])}</p><p>{escape(audit["coverage_basis"])}</p>',
                  '<p>Synthetic current parser check: 100.0 cm/s converts to 1.0 m/s.</p>',
                  '<h2>Engineering prerequisites</h2>'])
    for key, gap in audit["gaps"].items():
        parts.append(f'<p><strong>{escape(key)}: {gap["status"]}</strong> — {escape(gap["disposition"])}</p>')
    parts.append('<h2>Parser scope</h2><ul>')
    parts.extend(f'<li>{escape(item)}</li>' for item in audit["parser_limits"])
    parts.append('</ul><h2>Source acceptance</h2>')
    for key, acceptance in audit["parser_source_acceptance"].items():
        parts.append(f'<p><strong>{escape(key)}: {acceptance["status"]}</strong> {escape(acceptance.get("reason", "Documentation fields and units are confirmed by the recorded receipts."))}</p>')
    parts.extend(['<h2>Source attribution and receipts</h2>', f'<p>{escape(audit["source_reuse"])}</p>'])
    for receipt in audit["source_receipts"]:
        parts.append(f'<p><a href="{escape(receipt["url"])}">{receipt["key"]}: {escape(receipt["source_id"])}</a>; {escape(receipt["retrieved_utc"])}; HTTP {receipt["http_status"]}; {receipt["bytes"]} bytes.<br><code>{receipt["sha256"]}</code><br>{escape(receipt["basis"])}</p>')
    parts.extend([f'<p>{escape(audit["freshness"])}</p>', '<h2>Reproduction digests</h2>'])
    for path, digest in audit["code_sha256"].items():
        parts.append(f'<p>{escape(path)}<br><code>{digest}</code></p>')
    parts.extend([f'<p>Canonical synthetic input SHA-256: <code>{audit["input_sha256"]}</code>.</p>',
                  '<p>Provide approved limits, synchronized history and deployment-depth evidence before an engineering assessment. Standards-derived acceptance is outside this software check.</p>',
                  '</body></html>'])
    return '\n'.join(parts) + '\n'


def generate(out_dir: Path) -> None:
    """Write deterministic JSON/HTML from committed synthetic inputs and code."""
    audit = build_audit()
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    (out_dir / "operability-replacement.json").write_text(
        json.dumps(audit, indent=2, sort_keys=True, allow_nan=False) + "\n", encoding="utf-8")
    (out_dir / "operability-replacement.html").write_text(render_html(audit), encoding="utf-8")


def main() -> None:
    """Require explicit synthetic mode and an output directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--synthetic", action="store_true", required=True)
    parser.add_argument("--out-dir", type=Path, required=True)
    arguments = parser.parse_args()
    generate(arguments.out_dir)


if __name__ == "__main__":
    main()
