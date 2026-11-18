# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Report issue #2748 acceptance evidence without starting a native solver.

Run the complete tests/orcawave suite with -rA, save its output outside the
checkout, then pass that log here. --expected-ref additionally requires the
audited implementation and tests to equal that ref. OPEN includes unavailable
qualification evidence; passing arithmetic does not qualify a licensed clause.
"""

import argparse
import json
from pathlib import Path
import subprocess
import warnings

import numpy as np


def _nodes(module, **cases):
    """Expand explicit parameter IDs into complete pytest identities."""
    return tuple(
        f"tests/orcawave/test_{module}.py::{name}{suffix}"
        for name, parameters in cases.items()
        for suffix in (tuple(f"[{value}]" for value in parameters) or ("",))
    )


# Every independent acceptance case is required, including its file and parameters.
NODES = {
    1: _nodes(
        "audit_p1",
        test_01_volume_waterline=("True-False", "False-False", "False-True"),
        test_01_inward_flagged=(),
    ),
    2: _nodes(
        "audit_p1",
        test_02_single_cell_cycles=(),
        test_02_two_cell_cycle_mixture=(),
        test_02_probability_sum=(),
    ),
    3: _nodes("audit_p1", test_03_annex_d_weighted_dose=(), test_03_explicit_inputs=()),
    4: _nodes("audit_p1", test_04_rayleigh_maxima=()),
    5: _nodes("audit_p1", test_05_significant_amplitudes=()),
    6: _nodes("audit_p1", test_06_symmetry_defaults_and_domain=()),
    7: _nodes("audit_p1", test_07_explicit_two_peak=()),
    8: _nodes("audit_p1", test_08_native_fixture_and_wamit_refusal=()),
    9: _nodes(
        "audit_p1",
        test_09_vertex_stream=(),
        test_09_bad_count_or_nonfinite=("0 0 -2 " * 20, "0 0 -2 " * 28, "nan " * 72),
        test_09_negative_npan=(),
    ),
    10: _nodes(
        "audit_p2",
        test_10_gamma_validation=("0", "-1", "nan", "inf"),
        test_10_unusual_gamma_warning=(),
        test_10_finite_results=(),
        test_10_result_rejects_nan=(),
    ),
    11: _nodes(
        "audit_p2",
        test_11_disjoint_raos=("period", "heading"),
        test_11_invalid_quantile=("0-1", "-1-1", "1.1-1", "0.1-0", "0.1--1", "nan-1"),
        test_11_invalid_moments=tuple(
            f"{value}-{moment}" for value in (0, -1) for moment in ("m0", "m2", "m4")
        ),
        test_11_bad_grid=("omega0", "omega1", "omega2", "omega3"),
        test_11_generation_grid=("kwargs0", "kwargs1", "kwargs2"),
    ),
    12: _nodes(
        "audit_p2", test_12_jonswap_published_ordinates=(), test_12_issc_period_ratio=()
    ),
    13: _nodes(
        "audit_p1",
        test_04_rayleigh_maxima=(),
        test_05_significant_amplitudes=(),
        test_02_single_cell_cycles=(),
        test_03_annex_d_weighted_dose=(),
    ),
    14: _nodes("audit_p2", test_14_beam_wind_current_values=()),
    15: (
        *_nodes("audit_p2", test_15_hydrostatic_values_and_assumptions=()),
        *_nodes(
            "pack_gates",
            test_hydrodynamics_zero_diagonal_cannot_hide_asymmetry=(
                "added_mass",
                "radiation_damping",
            ),
        ),
    ),
    16: _nodes(
        "pack_gates",
        **{
            "test_" + name.replace("-", "_"): ()
            for name in (
                "C0-CSV",
                "C0-GDF",
                "C0-COEFF",
                "C0-MAP",
                "C1-OW",
                "C1-AQWA",
                "C4-AREA",
                "C4-ASPECT",
                "C4-SKEW",
                "C4-VOLUME",
                "C4-BALANCE",
                "C4-FRAME",
                "C4-MESH",
                "C4-WATERLINE",
                "C4-NORMALS",
                "C4-IRREGULAR",
                "C4-HYDRO",
                "C4-WIND",
                "C4-HULL",
                "C5-TRACE",
            )
        },
    ),
    17: _nodes(
        "audit_p2",
        test_17_database_consistency=("entry1", "entry2", "entry3", "entry4", "entry5"),
        test_17_rotation_at_25_seconds=(
            "FPSO-0.05-0.08",
            "SemiSubmersible-0.03-0.12",
            "Drillship-0.04-0.08",
            "Barge-0.06-0.1",
            "LNGC-0.04-0.08",
        ),
    ),
    18: _nodes("audit_p2", test_18_heuristic_explicit=("8", "16")),
    19: _nodes(
        "audit_p2",
        test_19_head_wind_frame=(),
        test_19_heading_domain=("-1", "360", "nan"),
        test_19_imported_frame_required=(),
    ),
}


def audit(log):
    """Combine collected acceptance outcomes with explicit qualification limits."""
    from digitalmodel.orcawave.vessel_database import (
        get_representative_raos,
        get_vessel,
        list_vessels,
    )
    from digitalmodel.orcawave.wave_spectrum import torsethaugen

    outcomes = []
    for line in log.splitlines():
        if line.startswith(
            ("PASSED ", "FAILED ", "SKIPPED ", "XFAIL ", "XPASS ", "ERROR ")
        ):
            status, node = line.split(maxsplit=1)
            outcomes.append((status, node))
    result = {}
    for section, nodes in NODES.items():
        passed = all(
            any(status == "PASSED" and name == node for status, name in outcomes)
            and not any(
                status != "PASSED" and (name == node or name.startswith(node + " - "))
                for status, name in outcomes
            )
            for node in nodes
        )
        result[section] = {
            "status": "FIXED" if passed else "OPEN",
            "evidence": "acceptance nodes pass"
            if passed
            else "acceptance evidence missing or failing",
        }

    for section in (3, 13):
        result[section] = {
            "status": "OPEN",
            "evidence": "arithmetic covered; licensed Annex D clause NOT_EVALUATED",
        }

    # #2748 section 17 requests pitch within 10% of deep-water slope at 25 s.
    # k=(2*pi/T)^2/g, degrees per metre amplitude=k*180/pi. This is not
    # universally a quasi-static period; never cap a resonant response to pass.
    slope = (2 * np.pi / 25) ** 2 / 9.81 * 180 / np.pi
    outside = 0
    unmarked = 0
    for key in list_vessels():
        entry = get_vessel(key)
        unmarked += int(not getattr(entry, "synthetic", False))
        response = get_representative_raos(entry.vessel_type)
        amplitude = response.pitch_rao[0][response.periods.index(25)]
        outside += int(abs(amplitude / slope - 1) > 0.1)
    if outside or unmarked:
        result[17] = {
            "status": "OPEN",
            "evidence": f"{outside} entries outside the 25 s slope band; {unmarked} missing synthetic flags",
        }

    # A renamed heuristic must also retain the requested deprecated callable.
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always", DeprecationWarning)
        try:
            torsethaugen(np.linspace(0.01, 20, 2000), 4, 10)
            compatible = any(issubclass(w.category, DeprecationWarning) for w in caught)
        except ValueError:
            compatible = False
    result[18] = {
        "status": "FIXED" if compatible and result[18]["status"] == "FIXED" else "OPEN",
        "evidence": (
            result[18]["evidence"]
            + "; renamed heuristic and deprecated callable; published model NOT_EVALUATED"
            if compatible
            else "heuristic renamed, but deprecated callable raises"
        ),
    }
    return result


def main():
    """Print one state per section; persist the same evidence on request."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pytest-log", type=Path, required=True)
    parser.add_argument("--expected-ref")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    if args.expected_ref:
        subprocess.run(
            [
                "git",
                "diff",
                "--exit-code",
                args.expected_ref,
                "--",
                "src/digitalmodel/orcawave",
                "tests/orcawave",
            ],
            check=True,
            capture_output=True,
        )
    result = audit(args.pytest_log.read_text(encoding="utf-8"))
    for section, row in result.items():
        print(f"{section:02d} {row['status']}: {row['evidence']}")
    if args.output:
        args.output.write_text(
            json.dumps(result, indent=2) + "\n", encoding="utf-8", newline="\n"
        )


if __name__ == "__main__":
    main()
