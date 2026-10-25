# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the offline seismic module report from its documented public example.

Use the package source on PYTHONPATH and run this script from any directory.
Only the report and its change-panel record are written. No solver is imported.
"""

from __future__ import annotations

import argparse
import ast
import hashlib
import importlib.metadata
import json
import re
from pathlib import Path, PurePath

import numpy as np
import yaml
from jinja2 import Environment, FileSystemLoader, StrictUndefined
from markupsafe import Markup
from scipy.integrate import trapezoid

from digitalmodel.seismic import read_ascii_accelerogram
from digitalmodel.seismic.readers.ascii import G0_M_S2
from digitalmodel.seismic.response_spectrum.calculations import compute_kinematics
from digitalmodel.signal_processing.signal_analysis.filters.frequency import (
    FrequencyFilter,
)


ROOT = Path(__file__).resolve().parents[2]
TEMPLATES = Path(__file__).with_name("templates")
EXAMPLE = Path("examples/workflows/response-spectrum")
OUTPUT = Path("docs/reports/modules/seismic")
# Source: the committed example's data/README.md, pinned fixture statistics.
# The unchanged record is pinned after CRLF-to-LF normalisation.
EXPECTED_RECORD_DIGEST = "de18e82358f66e7ed1d585a0086a815b2ea32c49d2cdf5c4e8ab8104b8f620f3"
EXEMPLAR = Path("docs/api/cfd/turbulent-flat-plate-verification.html")
BRAND = Path("docs/api/_assets/brand.css")
EXISTING_TESTS = (
    "tests/seismic/test_baseline.py",
    "tests/seismic/test_readers.py",
    "tests/seismic/test_response_spectrum_workflow.py",
)
SOURCE_FILES = (
    "src/digitalmodel/seismic/readers/ascii.py",
    "src/digitalmodel/seismic/baseline.py",
    "src/digitalmodel/seismic/response_spectrum/calculations.py",
    "src/digitalmodel/seismic/response_spectrum/workflow.py",
    "src/digitalmodel/seismic/response_spectrum/render.py",
    "src/digitalmodel/signal_processing/signal_analysis/filters/frequency.py",
)
SECTIONS = (
    "Executive Summary", "Introduction", "Design Data and Criteria",
    "Methodology", "Worked Example", "Validation Evidence",
    "Key Findings and Limits of Use", "Recommendations", "References",
    "Appendices",
)


def digest(path: Path) -> str:
    """Hash text sources after normalising CRLF line endings to LF."""
    return hashlib.sha256(path.read_bytes().replace(b"\r\n", b"\n")).hexdigest()


def source_record(path: str | PurePath) -> dict:
    """Retain portable evidence locators and LF-normalised content digests."""
    locator = path.as_posix() if isinstance(path, PurePath) else Path(path).as_posix()
    return {"path": locator, "sha256": digest(ROOT / locator)}


def reproduction_versions() -> dict[str, str]:
    """Record the installed numerical and template releases without gating."""
    names = ("numpy", "scipy", "pandas", "jinja2")
    return {name: importlib.metadata.version(name) for name in names}


def numeric_evidence() -> dict:
    """Call the documented module with the unchanged example settings."""
    config = yaml.safe_load((ROOT / EXAMPLE / "input.yml").read_text(encoding="utf-8"))
    settings = config["response_spectrum"]
    record_path = EXAMPLE / settings["record"]["file"]
    if digest(ROOT / record_path) != EXPECTED_RECORD_DIGEST:
        raise ValueError("Public example record digest mismatch; review the data basis")
    record = read_ascii_accelerogram(
        ROOT / record_path, units=settings["record"]["units"]
    )
    result = compute_kinematics(
        record, baseline=settings["baseline"], frequency_filter=settings["filter"]
    )
    metrics = dict(result.summary)
    metrics.update(
        baseline_samples=result.processing_chain[0]["samples"],
        baseline_offset_m_s2=result.processing_chain[0]["offset_m_s2"],
        low_hz=result.processing_chain[1]["band_hz"][0],
        high_hz=result.processing_chain[1]["band_hz"][1],
        requested_high_hz=settings["filter"]["band_hz"][1],
        filter_order=result.processing_chain[1]["order"],
        # SciPy 1.15.3 signal.butter, Wn: Nyquist = half the sample rate.
        nyquist_hz=1 / (2 * record.dt_s),
        filtered_peak_m_s2=float(result.timeseries.filtered_acceleration_m_s2.abs().max()),
        peak_velocity_m_s=float(result.timeseries.velocity_m_s.abs().max()),
        peak_displacement_m=float(result.timeseries.displacement_m.abs().max()),
        end_velocity_m_s=float(result.timeseries.velocity_m_s.iloc[-1]),
        end_displacement_m=float(result.timeseries.displacement_m.iloc[-1]),
        unfiltered_end_velocity_m_s=float(trapezoid(
            result.timeseries.corrected_acceleration_m_s2, result.timeseries.time_s
        )),
    )
    frequency_filter = FrequencyFilter(order=metrics["filter_order"])
    frequency_filter.design_filter(
        (metrics["low_hz"], metrics["high_hz"]), 1 / record.dt_s, btype="bandpass"
    )
    frequency, response = frequency_filter.get_frequency_response(fs=1 / record.dt_s)
    return {
        "case": "El Centro north–south public digitization",
        "method": result.method,
        "metrics": metrics,
        "processing_chain": result.processing_chain,
        "timeseries": {name: values.tolist() for name, values in result.timeseries.items()},
        # SciPy 1.15.3 filtfilt, Notes: forward/backward application squares gain.
        "filter_response": {"frequency_hz": frequency.tolist(),
                            "gain": np.round(np.abs(response) ** 2, 12).tolist()},
        "sources": [source_record(path) for path in (
            record_path, EXAMPLE / "input.yml", EXAMPLE / "data/README.md", *SOURCE_FILES,
            *EXISTING_TESTS,
        )],
        "versions": reproduction_versions(),
    }


def test_inventory() -> list[dict]:
    """Count existing checks without importing the general engine or its adapters."""
    rows = []
    for path in EXISTING_TESTS:
        tree = ast.parse((ROOT / path).read_text(encoding="utf-8"))
        names = [node.name for node in ast.walk(tree)
                 if isinstance(node, ast.FunctionDef) and node.name.startswith("test_")]
        rows.append({"path": path, "count": len(names), "tests": names})
    return rows


def inline_json(value: object) -> str:
    """Keep data inside a script element even when a string contains HTML."""
    return json.dumps(value, ensure_ascii=False, allow_nan=False, separators=(",", ":")).replace(
        "<", "\\u003c"
    ).replace(">", "\\u003e").replace("&", "\\u0026")


def styles() -> str:
    """Inline the existing house page and canonical brand stylesheet unchanged."""
    exemplar = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    house = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    brand = (ROOT / BRAND).read_text(encoding="utf-8")
    return f"<style>{brand}</style>\n<style>{house}</style>"


def build(output_dir: Path = ROOT / OUTPUT) -> None:
    """Render both deliverables after the source digest has been checked."""
    evidence = numeric_evidence()
    inventory = test_inventory()
    changes = {
        "document": "Seismic acceleration-record preprocessing analysis",
        "revisions": [{"revision": "00", "date": "2026-10-21", "changes": [
            "First issue records the public accelerogram calculation and its numerical basis.",
            "The distinction between acceleration preprocessing and oscillator response spectra is established.",
            "The building-floor measurement condition and missing design acceptance basis are stated.",
        ]}, {"revision": "01", "date": "2026-10-22", "changes": [
            "Text digests, source locators and output line endings are made portable for rebuilds.",
            "Installed dependency versions are retained as metadata without lockfile gating.",
            "The initial-window mean and unfiltered end velocity are stated as baseline evidence.",
        ]}, {"revision": "02", "date": "2026-10-24", "changes": [
            "Cross-runtime numerical comparison budgets are separated from exact same-runtime rebuilds.",
            "Review notes added during export remain unsaved, and intervening edits prevent import replacement.",
            "The current revision is added to the visible history, and integration remains available with newer numerical libraries.",
        ]}],
    }
    environment = Environment(
        loader=FileSystemLoader(TEMPLATES), autoescape=True, undefined=StrictUndefined,
        keep_trailing_newline=True,
    )

    def metric(name: str, digits: int = 3) -> Markup:
        value = evidence["metrics"][name]
        shown = f"{value:.{digits}f}".replace("-", "−")
        return Markup(f'<span data-metric="{name}" data-digits="{digits}">{shown}</span>')

    html = environment.get_template("seismic_module_report.html").render(
        metric=metric, evidence=evidence, inventory=inventory, revisions=changes["revisions"],
        existing_count=sum(row["count"] for row in inventory),
        g0=f"{G0_M_S2:.5f}", sections=SECTIONS,
        styles=Markup(styles()), evidence_json=Markup(inline_json(evidence)),
        changes_json=Markup(inline_json(changes)),
        interactions=Markup((TEMPLATES / "seismic_module_interactions.js").read_text(encoding="utf-8")),
        example_config=(ROOT / EXAMPLE / "input.yml").read_text(encoding="utf-8"),
        exemplar=EXEMPLAR.as_posix(), brand=BRAND.as_posix(),
    )
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "seismic-analysis.html").write_text(html, encoding="utf-8", newline="\n")
    (output_dir / "changes.json").write_text(
        json.dumps(changes, indent=2, ensure_ascii=False) + "\n", encoding="utf-8", newline="\n"
    )


def main() -> None:
    """Accept a disposable destination for rebuild checks."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=ROOT / OUTPUT)
    args = parser.parse_args()
    build(args.output_dir)
    print("Seismic report and change record rebuilt from the public example.")


if __name__ == "__main__":
    main()
