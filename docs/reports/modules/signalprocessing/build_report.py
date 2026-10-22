# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Rebuild the self-contained signal-processing analysis from public inputs.

Use report-requirements.txt beside the report tests for a byte-exact rebuild.
Numerical calculations also run with compatible installed library versions.
The --check option compares bytes without writing outputs.
"""

import argparse
import hashlib
import importlib.metadata
import json
import math
import re
from pathlib import Path

import numpy as np

from digitalmodel.signal_processing.signal_analysis import SpectralAnalyzer, TimeSeriesProcessor

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
STYLE_SOURCE = "docs/reports/pipeline-ffs-assessment.html"
NUMERICAL_LIBRARIES = ("numpy", "pandas", "scipy")
REBUILD = "PYTHONPATH=src python docs/reports/modules/signalprocessing/build_report.py"


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def read_basis():
    return json.loads((HERE / "public-example.json").read_text(encoding="utf-8"))


def make_signal(basis):
    """Reproduce the cited sine-plus-white-noise example with a declared seed."""
    data = basis["inputs"]
    fs = data["sampling_rate_hz"]
    time = np.arange(data["samples"]) / fs
    rng = np.random.Generator(np.random.PCG64(basis["adaptation"]["random_seed"]))
    amplitude = data["sine_rms_v"] * np.sqrt(2)
    noise_std = np.sqrt(data["noise_density_v2_per_hz"] * fs / 2)
    return amplitude * np.sin(2 * np.pi * data["sine_frequency_hz"] * time) + rng.normal(
        scale=noise_std, size=time.shape,
    )


def calculate(basis):
    versions = {name: importlib.metadata.version(name) for name in NUMERICAL_LIBRARIES}
    data = basis["inputs"]
    fs, size = data["sampling_rate_hz"], data["segment_samples"]
    signal = make_signal(basis)
    analyzer = SpectralAnalyzer(fs, "welch")
    frame = analyzer.compute_spectrum(signal, nperseg=size, noverlap=size // 2, window="hann")
    peak = analyzer.find_peaks(frame, n_peaks=1).iloc[0]
    stats = TimeSeriesProcessor().calculate_statistics(signal)
    spacing = float(frame.frequency.iloc[1] - frame.frequency.iloc[0])
    noise = float(frame.power.iloc[size // 4:].mean())
    metrics = {
        **data, "overlap_samples": size // 2,
        "random_seed": basis["adaptation"]["random_seed"],
        "duration_s": stats["length"] / fs,
        "rms_v": float(stats["rms"]), "variance_v2": float(stats["var"]),
        "bins": len(frame), "segments": 1 + (len(signal) - size) // (size - size // 2),
        "spacing_hz": spacing, "nyquist_hz": float(frame.frequency.iloc[-1]),
        "peak_hz": float(peak.frequency), "peak_density_v2_per_hz": float(peak.power),
        "peak_error_hz": abs(float(peak.frequency) - data["sine_frequency_hz"]),
        "peak_limit_hz": spacing / 2, "noise_density_estimate_v2_per_hz": noise,
        "noise_error_percent": abs(noise / data["noise_density_v2_per_hz"] - 1) * 100,
        "integrated_density_v2": float(frame.power.sum() * spacing),
        "noise_band_start_hz": float(frame.frequency.iloc[size // 4]),
    }
    return {
        "metrics": metrics, "versions": versions,
        "criteria_met": {
            "peak": metrics["peak_error_hz"] <= metrics["peak_limit_hz"],
        },
        "spectrum": {
            "frequency_hz": frame.frequency.tolist(),
            "density_v2_per_hz": frame.power.tolist(),
        },
        "signal_sha256": hashlib.sha256(signal.astype("<f8").tobytes()).hexdigest(),
        "basis_sha256": digest(HERE / "public-example.json"),
    }


def verify_evidence(evidence):
    if evidence["failed"] or evidence["errors"] or not evidence["passed"]:
        raise ValueError("Validation evidence contains a failure or no passing tests")
    for name, expected in evidence["sha256"].items():
        path = ROOT / name
        if path.resolve().is_relative_to(ROOT) is False or digest(path) != expected:
            raise ValueError(f"Evidence digest mismatch: {Path(name).as_posix()}; refresh the evidence before {REBUILD}")


def format_metric(key, value):
    if key in ("peak_hz", "peak_error_hz", "peak_limit_hz", "spacing_hz"):
        return f"{value:,.3f}"
    if key == "noise_error_percent":
        # Upward rounding preserves a statement of the form 'within x percent'.
        return f"{math.ceil(value * 10) / 10:.1f}"
    if "density" in key:
        return f"{value:.3e}" if abs(value) < 0.01 else f"{value:.3g}"
    if isinstance(value, int):
        return f"{value:,}"
    return f"{value:,.1f}"


def render():
    result = calculate(read_basis())
    if not all(result["criteria_met"].values()):
        raise ValueError("The demonstration criteria are not met; revise the analysis text")
    evidence = json.loads((HERE / "validation.json").read_text(encoding="utf-8"))
    verify_evidence(evidence)
    changes = json.loads((HERE / "changes.json").read_text(encoding="utf-8"))
    house_style = re.search(r"<style>.*?</style>", (ROOT / STYLE_SOURCE).read_text(encoding="utf-8"), re.S)[0]
    tokens = {
        "house_style": house_style,
        "document_binding": hashlib.sha256(b"".join(
            (HERE / name).read_bytes() for name in (
                "report.template.html", "report_ui.js", "public-example.json", "validation.json", "changes.json",
            )
        ) + house_style.encode() + json.dumps(result, sort_keys=True).encode()).hexdigest(),
        "calculation_data": json.dumps(result, sort_keys=True, allow_nan=False),
        "validation_passed": str(evidence["passed"]),
        "validation_groups": "".join(
            f'<tr><td>{group["description"]}</td><td>{group["passed"]}</td><td>{group["qualification"]}</td></tr>'
            for group in evidence["groups"]
        ),
        "change_panel": "".join(
            f"<li>{sentence}</li>" for revision in changes["revisions"] for sentence in revision["changes"]
        ),
        "ui_script": (HERE / "report_ui.js").read_text(encoding="utf-8"),
    }
    tokens.update({key: f'<span data-metric="{key}">{format_metric(key, value)}</span>'
                   for key, value in result["metrics"].items()})
    template = (HERE / "report.template.html").read_text(encoding="utf-8")
    page = re.sub(r"@@([a-z0-9_]+)@@", lambda match: tokens[match[1]], template)
    if "@@" in page:
        raise ValueError("Unresolved template field")
    return {
        "report.html": page,
        "results.json": json.dumps(result, indent=2, sort_keys=True, allow_nan=False) + "\n",
        "changes.json": json.dumps(changes, indent=2) + "\n",
    }


def build(output_dir=HERE, check=False):
    outputs = render()
    output_dir = Path(output_dir)
    if check:
        for name, content in outputs.items():
            target = output_dir / name
            if not target.exists() or target.read_bytes() != content.encode("utf-8"):
                raise ValueError(f"Rebuild differs: {name}; install report-requirements.txt and run {REBUILD}")
        return
    output_dir.mkdir(parents=True, exist_ok=True)
    for name, content in outputs.items():
        (output_dir / name).write_text(content, encoding="utf-8", newline="\n")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=HERE)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    build(args.output_dir, args.check)
