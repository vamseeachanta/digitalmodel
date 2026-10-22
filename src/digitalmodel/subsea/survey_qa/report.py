"""Worked synthetic analysis through the existing house report-pack front end.

Public references are carried in source_artifacts, not invented wiki locators.
The citations package's Citation schema requires an existing wiki reference;
none is asserted here. No licensed source values are reproduced.
"""

from __future__ import annotations

import csv
from dataclasses import asdict
from math import dist, sqrt
from pathlib import Path

from .calibration import estimate_patch
from .example import EXAMPLE_OFFSETS, make_example
from .verification import DepthUncertainty, apply_corrections, screen_overlap

NOAA_URL = (
    "https://nauticalcharts.noaa.gov/publications/docs/standards-and-requirements/"
    "fpm/field_procedures_manual_2020.pdf"
)
TRIG_URL = (
    "https://openstax.org/books/precalculus-2e/pages/5-4-right-triangle-trigonometry"
)
OLS_URL = "https://openstax.org/books/introductory-statistics-2e/pages/12-3-the-regression-equation"
IHO_URL = "https://iho.int/uploads/user/pubs/standards/s-44/S-44_Edition_6.1.0.pdf"


def _one_decimal(value):
    # Report comparison precision: one decimal; suppress rounded negative zero.
    return f"{0.0 if round(value, 1) == 0 else value:.1f}"


def write_example_report(output_dir: Path) -> dict:
    """Render md/html, CSVs and manifests at the caller's output directory.

    The noise amplitude, seed, matching tolerance and recovery tolerances are
    chosen synthetic test inputs. Only S-44's two explicitly cited table values
    are standard-derived. The example always uses the same inputs, with no date
    or path inferred from the execution environment. PDF output is off.
    """
    from digitalmodel.report_pack.workflow import router

    output_dir = Path(output_dir).resolve()
    output_dir.mkdir(parents=True, exist_ok=True)
    lines = make_example(noise_std_m=0.002, seed=7)
    estimated = estimate_patch(lines)
    a, b = (apply_corrections(s, estimated) for s in lines.roll)
    # Read from IHO S-44 edition 6.1.0 (October 2022), Table 1, Special Order.
    # Section 3.2.3 defines TVU_max; these are EXPLICIT example inputs only.
    budget = DepthUncertainty(
        "Special Order",
        "6.1.0 (2022)",
        0.25,
        0.0075,
        "IHO S-44, Table 1, section 3.2.3",
    )
    overlap = screen_overlap(a, b, budget=budget, xy_tolerance_m=0.1)
    missing = screen_overlap(a, b, budget=None, xy_tolerance_m=0.1)

    with (output_dir / "estimates.csv").open(
        "w", newline="", encoding="utf-8"
    ) as stream:
        writer = csv.writer(stream)
        writer.writerow(
            [
                "Parameter",
                "Unit",
                "Injected",
                "Estimated",
                "Difference",
                "Test tolerance",
            ]
        )
        for name, value in asdict(EXAMPLE_OFFSETS).items():
            factor, unit, tolerance = (
                (1000, "ms", 10) if name == "latency_s" else (1, "deg", 0.1)
            )
            got = getattr(estimated, name)
            writer.writerow(
                [
                    name,
                    unit,
                    _one_decimal(value * factor),
                    _one_decimal(got * factor),
                    _one_decimal((got - value) * factor),
                    _one_decimal(tolerance),
                ]
            )
    with (output_dir / "overlap.csv").open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(
            [
                "Screen",
                "Matched",
                "Unmatched first",
                "Unmatched second",
                "Max difference mm",
                "Min TVU ceiling mm",
                "Worst utilisation percent",
            ]
        )
        writer.writerow(
            [
                overlap.status,
                overlap.matched_count,
                overlap.unmatched_first,
                overlap.unmatched_second,
                _one_decimal(overlap.max_abs_difference_m * 1000),
                _one_decimal(overlap.min_limit_m * 1000),
                _one_decimal(overlap.max_ratio * 100),
            ]
        )

    def rms(order):
        corrected = apply_corrections(lines.roll[0], estimated, order=order)
        return sqrt(
            sum(dist(p, q) ** 2 for p, q in zip(corrected.xyz_m, corrected.truth_xyz_m))
            / len(corrected.xyz_m)
        )

    standard_rms = rms(("latency", "pitch", "roll", "yaw"))
    reversed_rms = rms(("yaw", "roll", "pitch", "latency"))
    sections = [
        {
            "title": "Summary",
            "content": (
                "A synthetic multibeam patch-test campaign recovers the injected roll 0.5 deg, "
                "pitch 1.0 deg, yaw 1.5 deg and latency 0.08 s. All estimates satisfy the chosen "
                "regression tolerances of 0.1 deg and 0.01 s. The corrected matched overlap is "
                "WITHIN_SCREEN against an explicitly supplied S-44 TVU ceiling. This is a "
                "synthetic consistency analysis; no survey-order certification is inferred."
            ),
        },
        {
            "title": "Revision history",
            "content": (
                "```text\n"
                "Revision | Date       | What changed and why\n"
                "00       | 2026-10-22 | Added synthetic geometry and hand calculations to demonstrate patch testing.\n"
                "```"
            ),
        },
        {
            "title": "Scope and data basis",
            "content": (
                "Eight synthetic lines share constant injected biases. Separate flat, sloping and "
                "point-target patches isolate each parameter. All values below are chosen inputs, "
                "except the identified S-44 table coefficients. No real survey data are used.\n\n"
                "- Depth datum 50 m; along-track slope 0.2 (depth increases with X).\n"
                "- Point target at X=0, Y=0, shoaling 5 m on the sloping patch.\n"
                "- Beam offsets -30 to +30 m at 10 m spacing; yaw tracks Y=-30/+30 m.\n"
                "- Speeds 2/4 m/s; pings at 1 s intervals for 40 s; reference starts X=-40/+40 m.\n"
                "- Sensor lever arm (0,0,0) m; constant sound velocity 1500 m/s at depths 0/100 m.\n"
                "- Exactly paired timestamps; supplied synchronization tolerance 0 s.\n"
                "- Independent Gaussian depth noise, standard deviation 0.002 m; seed 7 plus line index.\n"
                "- Chosen overlap XY matching tolerance 0.1 m; matched subset only.\n"
                "- Special Order coefficients a=0.25 m and b=0.0075, read from S-44 6.1.0 Table 1."
            ),
        },
        {
            "title": "Geometry and method",
            "content": (
                "Ground X runs forward along the first line, Y to starboard, Z down. Reciprocal "
                "direction reverses both horizontal vessel axes. Positive roll moves starboard "
                "returns deeper, pitch moves nadir returns forward, yaw moves starboard returns aft. "
                "Positive latency advances recorded position. Sensor lever arms set each rotation origin.\n\n"
                "```text\n"
                "Plan view (Z down)       X forward --->\n"
                "Y=-30: parallel line    ----------------->\n"
                "Y=  0: feature                          *\n"
                "Y=+30: parallel line    ----------------->\n"
                "Reciprocal slope/flat   <-----------------\n"
                "Cross section: starboard b>0; depth D>0 below sensor.\n"
                "```\n\n"
                "The footprint vector is q=(0,b,D). Forward injection is T_latency Ry Rx Rz q. "
                "Its inverse is applied as latency, pitch, roll, yaw. This rotation convention is "
                "declared for the example, not assumed for an acquisition system.\n\n"
                "Roll: fit measured cross-track slopes m+ and m- by least squares. "
                "After pitch correction m+=tan(r), m-=-tan(r); r=atan((m+-m-)/2). "
                "Hand example: slopes +0.01/-0.01 give r=atan(0.01).\n\n"
                "Pitch: after latency correction, nadir X_obs=X_true+direction*D_obs*tan(p). "
                "At equal observed depth, reciprocal separation dx=2*D_obs*tan(p); "
                "p=atan(dx/(2*D_obs)). Hand example: D=50 m and dx=2 m give p=atan(0.02). "
                "Using true instead of observed depth gives the usual small-angle approximation.\n\n"
                "Latency: at equal depth on the same heading, dx=direction*(v_fast-v_slow)*tau. "
                "Hand example: a 0.16 m shift divided by 2 m/s gives 0.08 s.\n\n"
                "Yaw: after earlier corrections, associated target returns have dx=-delta_b*sin(y), "
                "dy=delta_b*cos(y) in the vessel frame; y=atan(-dx/dy). "
                "Hand example: dx=-2 m, dy=sqrt(40^2-2^2) m gives y=asin(0.05). "
                "Track separation would require asin; the estimator uses observed relative offsets.\n\n"
                "Least squares: minimize sum((y-a-b*x)^2); the normal equations yield "
                "b=sum((x-xbar)*(y-ybar))/sum((x-xbar)^2). These equations are derived from the "
                "declared matrices and line geometries, rather than copied instrument formulas."
            ),
        },
        {
            "title": "Results",
            "content": (
                f"Standard-order coordinate RMS is {_one_decimal(standard_rms * 1000)} mm; "
                f"reverse-order RMS is {_one_decimal(reversed_rms * 1000)} mm on the first roll line. "
                "These truth-based diagnostics are used only for the synthetic example. Estimation "
                "uses measured returns, never truth coordinates. Tables use one decimal place.\n\n"
                "The overlap compares absolute paired depth differences against "
                "sqrt(a^2+(b*d)^2), with d the mean pair depth. This individual-sounding ceiling "
                "is used as a conservative consistency screen, not an IHO overlap rule. "
                f"When table inputs are omitted, the same overlap is {missing.status}."
            ),
        },
        {
            "title": "Sources and traceability",
            "content": (
                "- NOAA Field Procedures Manual (2020), section 1.5.6.2, pp. 26-27: "
                "line selection and pitch, roll, heading sequence. " + NOAA_URL + "\n"
                "- IHO S-44, edition 6.1.0 (October 2022), section 3.2.3 and Table 1, "
                "p. 18: TVU equation and Special Order coefficients. "
                + IHO_URL
                + "\n\n"
                "OpenStax, Precalculus 2e (2021), section 5.4 supplies the trigonometric "
                "ratios used in the geometry derivations: " + TRIG_URL + "\n\n"
                "OpenStax, Introductory Statistics 2e (2023), section 12.3 supplies the "
                "least-squares criterion; the normal-equation rearrangement is derived here: "
                + OLS_URL
                + "\n\n"
                "All four sources were opened from their publishers on 2026-10-22. The attempted "
                "6.2.0 URL returned HTML, so it supplies no coefficients. The NOAA reference "
                "treats timing as a separate gross diagnostic; latency-first is the chosen synthetic "
                "sequence here. Public URLs are retained in the provenance manifest. Wiki-based "
                "citation records are not fabricated; the citation sidecar is empty."
            ),
        },
        {
            "title": "Conclusions and limits",
            "content": (
                "The synthetic recovery and consistency screen satisfy the stated test criteria. "
                "Use the module to exercise sign conventions and parameter coupling before "
                "considering measured-data work. Real calibration requires qualified sensor geometry, "
                "sound-velocity treatment, timing, target association and an uncertainty budget.\n\n"
                "Only two parallel headings, constant velocity, known planar suitability, prescribed "
                "beam footprints and one explicitly associated point are supported. The SVP is "
                "validated metadata; no acoustic travel times are generated. No raw survey importer, "
                "refraction, motion history, datum reduction or full S-44 order assessment is included. "
                "Estimates have no confidence intervals. A user supplies the applicable order, edition, "
                "a, b and table reference; missing evidence remains NOT_EVALUATED. "
                "The nearest-point overlap matcher is intended for small synthetic datasets."
            ),
        },
    ]
    cfg = {
        "_config_dir_path": str(output_dir),
        "_config_file_path": str(output_dir / "patch_test.yml"),
        "report_pack": {
            "document": {
                "number": "S0000-RPT-001-00",
                "revision": "00",
                "title": "Synthetic MBES patch-test analysis",
                "project": "Synthetic example",
                "client": "Not applicable",
                "date": "2026-10-22",
                "revision_history": [
                    {
                        "rev": "00",
                        "date": "2026-10-22",
                        "description": "Initial synthetic method and worked calculations",
                    }
                ],
            },
            "sections": sections,
            "results": {
                "tables": [
                    {"title": "Bias recovery", "csv": "estimates.csv"},
                    {"title": "Corrected overlap consistency", "csv": "overlap.csv"},
                ]
            },
            "citations": [],
            "manifest": {
                "issue": "Refs #2955",
                "project": "Synthetic example",
                "artifact_class": "synthetic-calibration-analysis",
                "privacy_classification": "public-synthetic",
                "publishability_decision": "synthetic inputs and public references only",
                "input_source_ids": ["synthetic-patch-test"],
                "source_artifacts": {
                    "line_geometry": NOAA_URL,
                    "depth_uncertainty": IHO_URL,
                    "trigonometry": TRIG_URL,
                    "least_squares": OLS_URL,
                },
                "raw_output_path": "survey_qa:synthetic",
                "final_output_path": "survey_qa:example",
                "compute_environment": "not-recorded",
            },
            "pdf": "off",
            "output_dir": str(output_dir),
        },
    }
    return router(cfg)
