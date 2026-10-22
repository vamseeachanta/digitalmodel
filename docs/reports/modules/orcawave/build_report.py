"""Build the offline report using public, unlicensed calculation functions.

Run with PYTHONPATH=src and numpy/pydantic installed. No solver is imported.
The example reproduces test_significant_response in the public test suite.
"""
import hashlib
import inspect
import json
import re
from pathlib import Path

import numpy as np

from digitalmodel.orcawave.motion_statistics import (
    compute_response_spectrum,
    short_term_statistics,
)
from digitalmodel.orcawave.wave_spectrum import pierson_moskowitz

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
EXEMPLAR = "docs/api/cfd/wave-excited-body-rao-verification.html"
SOURCES = [
    "tests/orcawave/test_motion_statistics.py",
    "tests/orcawave/test_wave_spectrum.py",
    "src/digitalmodel/orcawave/motion_statistics.py",
    "src/digitalmodel/orcawave/wave_spectrum.py",
    "src/digitalmodel/orcawave/docs/engineering-citations.md",
    EXEMPLAR,
    "docs/api/_assets/brand.css",
]
CHANGES = {
    "revision": "A",
    "changes": [
        "The spectral response example is introduced with separate single and double amplitudes.",
        "Finite-frequency variance is compared with the full-frequency analytic reference.",
        "Physical response qualification and operating limits remain not evaluated.",
    ],
}


def calculate():
    """Return inputs, complete module outputs, arrays and derived comparisons."""
    inputs = dict(hs=3.0, tp=10.0, rao=0.8, omega_min=0.1,
                  omega_max=2.0, points=200)
    inputs["duration"] = inspect.signature(short_term_statistics).parameters[
        "duration_s"].default
    omega = np.linspace(inputs["omega_min"], inputs["omega_max"], inputs["points"])
    wave = pierson_moskowitz(omega, inputs["hs"], inputs["tp"])
    response = compute_response_spectrum(omega, np.full_like(omega, inputs["rao"]), wave)
    statistics = short_term_statistics(response, duration_s=inputs["duration"])
    reference = (inputs["rao"] * inputs["hs"] / 4) ** 2
    return {
        "inputs": inputs,
        "results": {**statistics.model_dump(), "m0": response.m0,
                    "m2": response.m2, "m4": response.m4},
        "derived": {"reference_variance": reference,
                    "variance_difference_percent": 100 * (response.m0 / reference - 1),
                    "cycles": inputs["duration"] / statistics.mean_zero_crossing_period},
        "omega": omega.tolist(), "wave": wave.tolist(),
        "response": response.spectral_density,
        "sources": {path: hashlib.sha256((ROOT / path).read_text(
            encoding="utf-8").encode("utf-8")).hexdigest()
                    for path in SOURCES},
    }


def number(value):
    """Use four significant figures without rounding nonzero values to zero."""
    return f"{value:.4g}".replace("-", "\N{MINUS SIGN}")


def render():
    data = calculate()
    values = {**data["inputs"], **data["results"], **data["derived"]}
    template = (HERE / "report.html.in").read_text(encoding="utf-8")
    for key, value in values.items():
        template = template.replace("{{" + key + "}}",
                                    f'<span data-value="{key}">{number(value)}</span>')
    css = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    css = re.sub(r"/\*.*?\*/", "", css, flags=re.S)
    exemplar = (ROOT / EXEMPLAR).read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    template = template.replace("{{style}}", css + style)
    template = template.replace("{{data}}", json.dumps(data, allow_nan=False))
    template = template.replace("{{changes}}", json.dumps(CHANGES))
    if re.search(r"\{\{\w+\}\}", template):
        raise ValueError("Unresolved report field")
    return template


def build(destination=HERE):
    destination = Path(destination)
    destination.mkdir(parents=True, exist_ok=True)
    (destination / "index.html").write_text(render(), encoding="utf-8", newline="\n")
    (destination / "changes.json").write_text(
        json.dumps(CHANGES, indent=2) + "\n", encoding="utf-8", newline="\n")


if __name__ == "__main__":
    build()
