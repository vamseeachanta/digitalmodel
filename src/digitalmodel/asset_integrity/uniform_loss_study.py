# ABOUTME: Bounded four-geometry uniform local-loss request preparation.
# ABOUTME: Physical grids and FCA requests never qualify assessment results.
"""Prepare reproducible requests; never execute or invert an FFS method.

Basis: issue 2287 (2026), Parametric basis and Output and inversion contract;
existing source audit, Tables 1/3. All defaults are synthetic study assumptions,
not standard coefficients or acceptance limits. Pressures are required inputs.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from pathlib import Path

from .uniform_loss_envelope import (
    MAX_WALL_TOLERANCE_IN,
    SOURCE_AUDIT,
    prepare_envelope_study,
)

# Issue 2287 (2026), Parametric basis: four OD/wall pairs (in), seven axial
# lengths (in), four circumferential fractions, three FCA sensitivity values
# (in), 20% nominal lower study bound and 16-by-8 physical cell-center grid.
# These are experimental choices, never normative API 579 limits.
GEOMETRIES_IN = ((12.75, 0.375), (16.0, 0.625), (20.0, 0.5), (30.0, 0.375))
LENGTHS_IN = (0.5, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0)
WIDTH_FRACTIONS = (0.02, 0.10, 0.25, 0.50)
FCA_PAIRS_IN = ((0.0, 0.0), (0.020, 0.020), (0.050, 0.050))
LOWER_WALL_FRACTION = 0.20
GRID_SHAPE = (16, 8)
SPEC = "docs/plans/2026-10-06-issue-2287-uniform-local-loss-screening.md"


def _nonnegative(value):
    try:
        return type(value) in (int, float) and math.isfinite(value) and value >= 0
    except OverflowError:
        return False


def _axis(extent, count):
    # Equal-cell geometry: x_j=(j+1/2)*extent/N, j=0..N-1.
    # The half-cell offset defines a cell center (issue 2287, physical grid).
    return [(index + 0.5) * (extent / count) for index in range(count)]


def _sha256(value):
    return hashlib.sha256(value).hexdigest()


def _canonical(value):
    return json.dumps(
        value, sort_keys=True, separators=(",", ":"), allow_nan=False
    ).encode()


def prepare_study(*, pressures_psi, fca_pairs_in=FCA_PAIRS_IN):
    """Build at most 336 requests with both levels permanently unevaluated.

    Supply one pressure per pipe in GEOMETRIES_IN order. One to three distinct
    (sound FCA, flaw FCA) pairs are permitted; the cap bounds this study's size.
    FCA subtraction is reported as requested arithmetic only, never labelled Tc
    or tmm, never used to alter current geometry or qualify an inversion.
    Nonpositive reduced search bounds are rejected before a result is returned.
    """
    pressures = list(pressures_psi)
    if len(pressures) != len(GEOMETRIES_IN) or any(
        not _nonnegative(value) or value == 0 for value in pressures
    ):
        raise ValueError("Supply four finite positive pressures in geometry order")
    pairs = [list(pair) for pair in fca_pairs_in]
    if not 1 <= len(pairs) <= len(FCA_PAIRS_IN):
        raise ValueError("Supply one to three distinct FCA pairs")
    if any(len(pair) != 2 or not all(_nonnegative(v) for v in pair) for pair in pairs):
        raise ValueError("Each FCA pair requires finite nonnegative sound/flaw values")
    if len({tuple(pair) for pair in pairs}) != len(pairs):
        raise ValueError("Duplicate FCA pairs are not permitted")
    if any(
        sound >= wall or flaw >= LOWER_WALL_FRACTION * wall
        for _, wall in GEOMETRIES_IN
        for sound, flaw in pairs
    ):
        raise ValueError("Requested FCA exhausts sound wall or lower study wall")

    requests = [
        dict(
            od_in=od,
            nominal_wall_in=wall,
            sound_wall_in=wall,
            axial_length_in=length,
            width_fraction=width,
            pressure_psi=pressure,
            lower_wall_in=LOWER_WALL_FRACTION * wall,
            upper_wall_in=wall,
            wall_tolerance_in=MAX_WALL_TOLERANCE_IN,
        )
        for (od, wall), pressure in zip(GEOMETRIES_IN, pressures)
        for length in LENGTHS_IN
        for width in WIDTH_FRACTIONS
        for _ in pairs
    ]
    result = prepare_envelope_study(requests)
    for index, cell in enumerate(result["cells"]):
        sound, flaw = pairs[index % len(pairs)]
        inputs, geometry = cell["inputs"], cell["geometry"]
        cell["case_id"] = f"cell-{index + 1:04d}"
        cell["fca_request"] = dict(
            sound_fca_in=sound,
            flaw_fca_in=flaw,
            status="NOT_EVALUATED",
            sound_wall_after_subtraction_in=inputs["sound_wall_in"] - sound,
            lower_wall_after_subtraction_in=inputs["lower_wall_in"] - flaw,
            upper_wall_after_subtraction_in=inputs["upper_wall_in"] - flaw,
            reason="Arithmetic request only; Part 5 FCA definitions not qualified",
        )
        cell["physical_grid"] = dict(
            shape=list(GRID_SHAPE),
            coordinate_convention="cell centers",
            axial_edges_in=[0.0, inputs["axial_length_in"]],
            circumferential_edges_in=[0.0, geometry["width_arc_in"]],
            axial_centers_in=_axis(inputs["axial_length_in"], GRID_SHAPE[0]),
            circumferential_centers_in=_axis(geometry["width_arc_in"], GRID_SHAPE[1]),
        )
    configuration = dict(
        pressures_psi=pressures,
        fca_pairs_in=pairs,
        geometries_in=[list(pair) for pair in GEOMETRIES_IN],
        lengths_in=list(LENGTHS_IN),
        width_fractions=list(WIDTH_FRACTIONS),
        grid_shape=list(GRID_SHAPE),
        lower_wall_fraction=LOWER_WALL_FRACTION,
        wall_tolerance_in=MAX_WALL_TOLERANCE_IN,
    )
    root = Path(__file__).resolve().parents[3]
    sources = [
        Path(__file__).relative_to(root).as_posix(),
        "src/digitalmodel/asset_integrity/uniform_loss_envelope.py",
        SOURCE_AUDIT,
        SPEC,
    ]
    result["configuration"] = configuration
    result["meta"].update(
        study_config_sha256=_sha256(_canonical(configuration)),
        source_sha256={name: _sha256((root / name).read_bytes()) for name in sources},
        source_hash_scope="request preparation modules, source audit and study specification; exact bytes",
        study_config_hash_scope="canonical complete study configuration; includes FCA and grid shape",
        requested_cells=len(result["cells"]),
        evaluated_levels=0,
        numerical_results=False,
    )
    return result


def main(argv=None):
    """Write strict deterministic JSON; reuse identical files, refuse replacement."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--config", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        config = json.loads(args.config.read_text(encoding="utf-8"))
        result = prepare_study(**config)
        payload = (json.dumps(result, indent=2, allow_nan=False) + "\n").encode("utf-8")
        if args.output.exists():
            if args.output.read_bytes() != payload:
                parser.error("Existing output differs; choose a new output file")
            action = "reused"
        else:
            with args.output.open("xb") as stream:
                stream.write(payload)
            action = "wrote"
    except (ValueError, TypeError, OSError, OverflowError) as error:
        # Do not echo caller file paths or configuration values in diagnostics.
        parser.error(f"Request preparation failed ({type(error).__name__})")
    print(f"{action} {len(result['cells'])} requests; 0 evaluated levels")


if __name__ == "__main__":
    main()
