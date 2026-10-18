# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build OrcaWave diffraction decks from vessel-database records.

Bridges vessel_db -> hull_library -> diffraction (#622): for a vessel record it
takes mass/inertia from ``vessel_db.mass_properties``, generates a parametric
hull mesh from the principal particulars (per hull form), exports a WAMIT GDF,
and assembles a ``DiffractionSpec`` -> OrcaWave ``.yml`` via ``orcawave_backend``.

Honesty: the parametric mesh is a wall-sided, plan-form-tapered approximation
for first-pass screening — NOT a class-approved hull. The mesh path is a single
swappable field (``hull.gdf``); drop in a real mesh and re-run.
Drilling semi-submersibles use an assumed four-column, twin-pontoon mesh.
Crane semi-submersibles, spars and TLPs reference a mesh to supply
(``PROVIDE_HULL_MESH.gdf``).

The solver run itself needs a licensed OrcaWave/AQWA machine; this module only
*prepares* the decks.
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import numpy as np

from digitalmodel.hydrodynamics.diffraction.input_schemas import (
    DiffractionSpec,
    EnvironmentSpec,
    FrequencyInputType,
    FrequencySpec,
    VesselGeometry,
    VesselInertia,
    VesselSpec,
    WaveHeadingSpec,
)
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
from digitalmodel.hydrodynamics.hull_library.column_pontoon_form import generate_column_pontoon
from digitalmodel.hydrodynamics.hull_library.generic_semi import generic_semi_parameters
from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import export_gdf
from digitalmodel.hydrodynamics.hull_library.mesh_generator import (
    HullMeshGenerator,
    MeshGeneratorConfig,
)
from digitalmodel.hydrodynamics.hull_library.profile_schema import (
    HullProfile,
    HullStation,
    HullType,
)
from digitalmodel.marine_ops.vessel_db.gyradii import estimate_gyradii
from digitalmodel.marine_ops.vessel_db.loader import Record
from digitalmodel.marine_ops.vessel_db.mass_properties import from_record

PLACEHOLDER_MESH = "PROVIDE_HULL_MESH.gdf"
MESH_FILE = "hull.gdf"
ASSUMED_SEMI_NAME = "Generic semi-submersible - assumed proportions"

# Plan-form fineness per form: 1.0 = pure box; lower = finer (more tapered) ends.
_FORM_TAPER = {"box": 0.95, "full": 0.86, "ship": 0.74}
_FORM_HULLTYPE = {
    "box": HullType.BARGE,
    "full": HullType.TANKER,
    "ship": HullType.SHIP,
}

# Default analysis grid (first-pass): wave periods (s) and headings (deg).
DEFAULT_PERIODS = [4.0, 5.0, 6.0, 7.0, 8.0, 10.0, 12.0, 14.0, 16.0, 18.0, 20.0, 25.0]
DEFAULT_HEADINGS = [0.0, 45.0, 90.0, 135.0, 180.0]


def clean_vessel_name(name: str) -> str:
    """Return a filesystem and deck name without unsafe characters.

    Parenthetical suffixes are removed from the backend's YAML filename.
    """
    base = name.split("(")[0].strip() or name
    base = re.sub(r"[^A-Za-z0-9 _\-]+", "", base).strip()
    return base or "vessel"


def select_form(record: Record) -> str:
    """Pick a parametric hull form for a vessel record.

    Returns one of: ``box`` (FPSO/FSO/barge), ``full`` (tanker/gas carrier),
    ``ship`` (drillship / monohull construction / default), ``column_pontoon``
    (drilling semi-submersible), or ``placeholder`` (crane semi / spar / TLP).
    Column-and-pontoon outputs are labelled "assumed proportions".
    """
    vt = (record.vessel_type or "").lower()
    if any(k in vt for k in ("spar", "tlp")):
        return "placeholder"
    if any(k in vt for k in ("semi", "column", "pontoon")):
        # Crane and SSCV wording occurs in the curated particulars records;
        # heavy-lift occurs in the installation records. Space form is equivalent.
        if any(k in vt for k in ("crane", "sscv", "heavy-lift", "heavy lift")):
            return "placeholder"
        return "column_pontoon"
    if any(k in vt for k in ("tanker", "vlcc", "crude", "lng", "lpg", "gas carrier")):
        return "full"
    if any(k in vt for k in ("fpso", "fso", "barge")):
        return "box"
    return "ship"


def parametric_hull_profile(
    name: str,
    loa: float,
    beam: float,
    draft: float,
    depth: float,
    form: str = "ship",
    n_stations: int = 15,
) -> HullProfile:
    """Wall-sided, plan-form-tapered HullProfile from principal particulars.

    Sections are rectangular (flat bottom, vertical sides) at a half-beam that
    tapers toward the ends per ``form`` — a first-pass screening hull, not a
    faired design.
    """
    half = beam / 2.0
    taper = _FORM_TAPER.get(form, _FORM_TAPER["ship"])
    end_frac = (1.0 - taper) / 2.0  # fraction of length over which ends taper in
    xs = np.linspace(0.0, loa, n_stations)
    stations = []
    for x in xs:
        f = x / loa if loa else 0.0
        if end_frac <= 0:
            pf = 1.0
        else:
            pf = min(1.0, min(f, 1.0 - f) / end_frac)
        pf = max(pf, 0.06)  # keep a small non-zero breadth at the very ends
        y = round(half * pf, 4)
        stations.append(
            HullStation(
                x_position=round(float(x), 4),
                waterline_offsets=[(0.0, float(y)), (float(depth), float(y))],
            )
        )
    return HullProfile(
        name=name,
        hull_type=_FORM_HULLTYPE.get(form, HullType.SHIP),
        stations=stations,
        length_bp=float(loa),
        beam=float(beam),
        draft=float(draft),
        depth=float(depth),
        source="parametric (vessel_db particulars; first-pass screening hull)",
    )


@dataclass
class DeckResult:
    """Paths, mesh classification and mass basis of a prepared deck."""

    vessel: str
    form: str
    deck_path: Path  # OrcaWave-native deck (.yml)
    spec_path: Path  # canonical DiffractionSpec (spec.yml) — batch-runner input
    mesh_path: Optional[Path]
    mesh_kind: str  # "parametric" | "placeholder"
    n_panels: Optional[int]
    mass_basis: str  # "explicit" | "free_floating"
    notes: list[str] = field(default_factory=list)


def _inertia_for(record: Record, dims: dict) -> tuple[VesselInertia, str, list[str]]:
    """Build explicit inertia when displacement is known, otherwise free-floating.

    In free-floating mode, displaced volume supplies the solver's mass.
    """
    notes: list[str] = []
    mp = from_record(record)
    if mp is not None and mp.mass_te and mp.mass_te > 0:
        rog = [
            mp.gyradii_m.get("kxx"),
            mp.gyradii_m.get("kyy"),
            mp.gyradii_m.get("kzz"),
        ]
        if any(v is None for v in rog):
            # fall back to estimates for any missing axis
            est = estimate_gyradii(
                dims.get("beam"), dims.get("lbp") or dims.get("loa"), record.vessel_type
            )
            rog = [
                mp.gyradii_m.get("kxx")
                or (est.get("kxx_roll").value if est.get("kxx_roll") else 0.0),
                mp.gyradii_m.get("kyy")
                or (est.get("kyy_pitch").value if est.get("kyy_pitch") else 0.0),
                mp.gyradii_m.get("kzz")
                or (est.get("kzz_yaw").value if est.get("kzz_yaw") else 0.0),
            ]
        if "vcg" in mp.gaps:
            notes.append(
                "VCG unknown (CoG z defaulted to keel datum) — set before running"
            )
        # mass_properties.from_record cog_m datum: x=0 amidships (+fwd),
        # y=0 centreline, z=0 keel. The parametric mesh/deck frame has x=0 at
        # the aft perpendicular (mesh x spans 0..LOA) and z=0 at the waterline
        # (mesh z spans -draft..0; see hull_library.mesh_generator), so convert
        # before the backend emits BodyCentreOfMass verbatim.
        loa = dims.get("loa") or dims.get("lbp") or 0.0
        draft = dims.get("draft") or 0.0
        cx, cy, cz = (float(c) for c in mp.cog_m)
        return (
            VesselInertia(
                mode="explicit",
                mass=mp.mass_te * 1000.0,  # tonnes -> kg
                centre_of_gravity=[loa / 2.0 + cx, cy, cz - draft],
                radii_of_gyration=[float(v) for v in rog],
            ),
            "explicit",
            notes,
        )
    # No displacement: let the solver compute mass from displaced volume.
    est = estimate_gyradii(
        dims.get("beam"), dims.get("lbp") or dims.get("loa"), record.vessel_type
    )
    rog = [
        (
            est.get("kxx_roll").value
            if est.get("kxx_roll")
            else 0.35 * (dims.get("beam") or 1.0)
        ),
        (
            est.get("kyy_pitch").value
            if est.get("kyy_pitch")
            else 0.25 * (dims.get("loa") or 1.0)
        ),
        (
            est.get("kzz_yaw").value
            if est.get("kzz_yaw")
            else 0.26 * (dims.get("loa") or 1.0)
        ),
    ]
    notes.append("displacement unknown -> free_floating mode (mass from mesh volume)")
    return (
        VesselInertia(
            mode="free_floating",
            mass=1.0,
            centre_of_gravity=[0.0, 0.0, 0.0],
            radii_of_gyration=[float(v) for v in rog],
            cog_z=0.0,
        ),
        "free_floating",
        notes,
    )


def build_deck(
    record: Record,
    out_dir: Path,
    *,
    periods: Optional[list[float]] = None,
    headings: Optional[list[float]] = None,
    water_depth: float | str = "infinite",
    target_panels: int = 1000,
) -> Optional[DeckResult]:
    """Prepare one OrcaWave diffraction deck for a vessel record.

    Returns None if the record lacks the principal dimensions needed to build a
    mesh/spec. Writes ``<out_dir>/<vessel_id>/{hull.gdf, spec.yml, <name>.yml,
    manifest.json}`` (``spec.yml`` = canonical DiffractionSpec, ``<name>.yml`` =
    OrcaWave-native deck).
    """
    dims = record.canonical_dimensions()
    loa = dims.get("loa") or dims.get("lbp")
    beam = dims.get("beam")
    draft = dims.get("draft")
    if not (loa and beam and draft):
        return None
    depth = dims.get("depth") or round(draft * 1.6, 2)

    form = select_form(record)
    vid = record.vessel_id()
    deck_dir = Path(out_dir) / vid
    deck_dir.mkdir(parents=True, exist_ok=True)

    notes: list[str] = []
    mesh_path: Optional[Path] = None
    n_panels: Optional[int] = None
    column_mesh = None
    if form == "column_pontoon":
        try:
            # The generic screening geometry uses the specified coarse 4.0 m
            # target size; target_panels controls the single-surface generator.
            parameters = generic_semi_parameters(loa, beam, draft, 4.0)
        except ValueError:
            form = "placeholder"
            notes.append("invalid column-and-pontoon parameters; supply a measured mesh")
        else:
            column_mesh, _ = generate_column_pontoon(parameters)
    if form == "placeholder":
        mesh_file = PLACEHOLDER_MESH
        mesh_kind = "placeholder"
        notes.append(
            f"'{record.vessel_type}' is not representable as a single hull surface "
            f"(columns/pontoons); supply a real mesh as '{PLACEHOLDER_MESH}' and re-run."
        )
    elif column_mesh is not None:
        column_mesh.name = ASSUMED_SEMI_NAME
        mesh_path = export_gdf(column_mesh, deck_dir / MESH_FILE)
        n_panels = len(column_mesh.panels)
        mesh_file, mesh_kind = MESH_FILE, "parametric"
        notes.append("assumed proportions: based on previous experience")
        notes.append("no braces, column rotation or pontoon overhangs; geometric lid disabled")
    else:
        profile = parametric_hull_profile(record.name, loa, beam, draft, depth, form)
        mesh = HullMeshGenerator().generate(
            profile, MeshGeneratorConfig(target_panels=target_panels)
        )
        mesh_path = export_gdf(mesh, deck_dir / MESH_FILE)
        n_panels = len(mesh.panels)
        mesh_file = MESH_FILE
        mesh_kind = "parametric"
        notes.append(
            "parametric wall-sided mesh (first-pass screening; swap in a faired hull for design)"
        )

    inertia, mass_basis, inertia_notes = _inertia_for(record, dims)
    if column_mesh is not None and mass_basis == "explicit":
        # Column-and-pontoon x=0 is amidships; the single-surface x=0 is aft.
        inertia.centre_of_gravity[0] -= loa / 2.0
    notes.extend(inertia_notes)
    deck_name = ASSUMED_SEMI_NAME if column_mesh is not None else clean_vessel_name(record.name)
    deck_type = (
        "generic drilling semi-submersible (assumed proportions)"
        if column_mesh is not None else record.vessel_type or form
    )

    spec = DiffractionSpec(
        vessel=VesselSpec(
            name=deck_name,
            type=deck_type,
            geometry=VesselGeometry(mesh_file=mesh_file),
            inertia=inertia,
        ),
        environment=EnvironmentSpec(water_depth=water_depth),
        frequencies=FrequencySpec(
            input_type=FrequencyInputType.PERIOD,
            values=periods or DEFAULT_PERIODS,
        ),
        wave_headings=WaveHeadingSpec(values=headings or DEFAULT_HEADINGS),
    )
    # Canonical spec on disk: this is what the batch runner / licensed-run lane
    # consumes (DiffractionSpec.from_yaml); the OrcaWave-native deck alongside
    # it is for direct opening in OrcaWave.
    spec_path = spec.to_yaml(deck_dir / "spec.yml")
    deck_path = OrcaWaveBackend().generate_single(spec, deck_dir)

    manifest = {
        "vessel": deck_name if column_mesh is not None else record.name,
        "vessel_id": vid,
        "vessel_type": record.vessel_type,
        "form": form,
        "mesh": {
            "kind": mesh_kind,
            "file": mesh_file,
            "n_panels": n_panels,
            "swap": f"replace {mesh_file} with a real mesh and re-run; no other change needed",
        },
        "particulars": {"loa": loa, "beam": beam, "draft": draft, "depth": depth},
        "mass_basis": mass_basis,
        "water_depth": water_depth,
        "periods_s": periods or DEFAULT_PERIODS,
        "headings_deg": headings or DEFAULT_HEADINGS,
        "deck": deck_path.name,
        "spec": spec_path.name,
        "notes": notes,
        "provenance": "parametric prep from vessel_db; run on a licensed OrcaWave/AQWA machine",
    }
    if column_mesh is not None:
        manifest["source_record_id"] = manifest.pop("vessel_id")
        manifest["geometry_basis"] = "assumed proportions"
    (deck_dir / "manifest.json").write_text(json.dumps(manifest, indent=2))

    return DeckResult(
        vessel=deck_name if column_mesh is not None else record.name,
        form=form,
        deck_path=deck_path,
        spec_path=spec_path,
        mesh_path=mesh_path,
        mesh_kind=mesh_kind,
        n_panels=n_panels,
        mass_basis=mass_basis,
        notes=notes,
    )
