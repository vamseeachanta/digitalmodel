"""Offline sizing-to-coupled-input mapping; never loads a solver binding.

Sources for the new mapping (no empirical engineering constants are added):
* OpenStax, University Physics vol. 1 (2016), §9.6: mass first moments.
* OpenStax, University Physics vol. 1 (2016), §10.5: I = sum(m r²).
* BIPM, SI Brochure, 9th ed. (2019), Table 8: 1 tonne = 1000 kg.
* Native property names: repository L02 YAML, VesselTypes/Draughts and Turbines.

The screening code's existing coefficients remain screening assumptions. Hull
radii are required rather than copying its pitch estimate into every axis.
The supplied template must represent RNA and tower separately from the hull;
its mass inventory and controller compatibility still need independent review.
"""

from __future__ import annotations

import copy
import hashlib
import json
import math
import re
import stat
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Literal

import yaml
from pydantic import BaseModel, ConfigDict, Field, model_validator

from digitalmodel.floating_wind import floaters
from digitalmodel.floating_wind.floaters import (
    G_STANDARD,
    RHO_SEAWATER,
    FloaterArchetype,
    TurbineTopside,
    build_floater,
)
from digitalmodel.floating_wind.screening import (
    LoadCase,
    ScreeningCriteria,
    VariantScreening,
    screen_concept,
)
from digitalmodel.floating_wind.solver_tier import FrequencyGrid, build_orcawave_spec
from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator
from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

# Exact SI conversion, BIPM SI Brochure 9th ed. (2019), Table 8.
KG_PER_TONNE = 1000.0
SOURCES = (
    "OpenStax, University Physics vol. 1 (2016), section 9.6: mass first moments; "
    "https://openstax.org/books/university-physics-volume-1/pages/9-6-center-of-mass",
    "OpenStax, University Physics vol. 1 (2016), section 10.5: moments of inertia; "
    "https://openstax.org/books/university-physics-volume-1/pages/10-5-calculating-moments-of-inertia",
    "BIPM, SI Brochure, 9th ed. (2019), Table 8: tonne to kilogram; "
    "https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf",
    "BIPM, 3rd CGPM (1901), Declaration 2, conventional value of standard gravity; "
    "https://www.bipm.org/en/committees/cg/cgpm/3-1901/resolution-2",
    "NOAA, World Ocean Atlas 2013 FAQ, question 8: assumed seawater density; "
    "https://www.nodc.noaa.gov/OC5/wod-woa-faqs.html",
)
NOT_EVALUATED = {
    "diffraction_schema": "Diffraction package initialization imports solver components; validation deferred.",
    "mesh_quality": "Supply a wetted hull mesh consistent with the sized geometry; no mesh is inferred.",
    "diffraction_import": "Import new radiation, added-mass, damping and load-RAO data for this geometry.",
    "topside_inventory": "Confirm separate RNA/tower mass, CG and inertia in the supplied template.",
    "turbine_controller": "The reviewed turbine/controller arrangement from #2423 remains a prerequisite.",
    "mooring": "Confirm mooring topology and properties against the sized hull and site.",
    "native_load": "Repository schema/reference checks do not prove native solver loadability.",
    "solver_comparison": "No diffraction or coupled time-domain solve is performed by this workflow.",
    "screening_basis": "Inherited screening coefficients and acceptance limits require a documented design basis.",
}


class ChainInput(BaseModel):
    """One explicit input for screening and both solver-input stages.

    Frames are aligned, origin at still water on the hull centreline. Hull and
    topside CGs are on that centreline; hull radii are about the hull-only CG.
    The template supplies the complete turbine/tower/mooring/controller topology.
    """

    model_config = ConfigDict(extra="forbid", allow_inf_nan=False)

    case_id: str = Field(pattern=r"^[a-zA-Z0-9][a-zA-Z0-9_-]*$")
    basis: str = Field(min_length=1)
    archetype: FloaterArchetype
    params: dict[str, Any]
    topside: TurbineTopside
    criteria: ScreeningCriteria
    load_cases: list[LoadCase] = Field(min_length=1)
    water_depth_m: float = Field(gt=0)
    water_density_kg_m3: float = Field(gt=0)
    ballast_vcg_m: float = Field(ge=0, description="Ballast CG above keel (m)")
    frequencies_rad_s: list[float] = Field(min_length=1)
    headings_deg: list[float] = Field(min_length=1)
    hull_radii_m: tuple[float, float, float]
    mesh_file: str = Field(min_length=1)
    floater_name: str = Field(min_length=1)
    turbine_name: str = Field(min_length=1)
    template: dict[str, Any]

    @model_validator(mode="before")
    @classmethod
    def _explicit_basis(cls, data: Any) -> Any:
        if not isinstance(data, dict):
            return data
        # No silently inherited screening thresholds or empirical coefficients.
        for name, model in (
            ("topside", TurbineTopside),
            ("criteria", ScreeningCriteria),
        ):
            value = data.get(name, {})
            keys = (
                value.model_fields_set if isinstance(value, BaseModel) else value.keys()
            )
            missing = set(model.model_fields) - set(keys)
            unknown = set(keys) - set(model.model_fields)
            if missing or unknown:
                raise ValueError(f"explicit {name} inputs required: {sorted(missing)}")
        return data

    @model_validator(mode="after")
    def _check_inputs(self) -> ChainInput:
        floater = build_floater(self.archetype, **self.params)
        missing = set(type(floater).model_fields) - set(self.params)
        unknown = set(self.params) - set(type(floater).model_fields)
        if missing or unknown:
            raise ValueError(
                f"explicit floater inputs required; missing={sorted(missing)}, unknown={sorted(unknown)}"
            )
        if self.water_depth_m <= floater.draft:
            raise ValueError("water depth must exceed hull draft")
        # The inherited screen cannot vary density or ballast position. Require
        # an explicit compatible basis rather than silently screen another hull.
        if self.water_density_kg_m3 != RHO_SEAWATER:
            raise ValueError("unsupported screening water density")
        # This is an inherited screening assumption, not a design coefficient.
        if not math.isclose(
            self.ballast_vcg_m,
            floaters.BALLAST_VCG_DRAFT_FRACTION * floater.draft,
            abs_tol=1e-12,
        ):
            raise ValueError("unsupported screening ballast VCG")
        if any(r <= 0 for r in self.hull_radii_m):
            raise ValueError("hull radii must be positive")
        if any(f <= 0 or not math.isfinite(f) for f in self.frequencies_rad_s):
            raise ValueError("frequencies must be finite and positive")
        if self.frequencies_rad_s != sorted(set(self.frequencies_rad_s)):
            raise ValueError("frequencies must be unique and increasing")
        if any(not math.isfinite(h) for h in self.headings_deg):
            raise ValueError("headings must be finite")
        if len(set(self.headings_deg)) != len(self.headings_deg):
            raise ValueError("headings must be unique")
        if not re.fullmatch(r"[a-zA-Z0-9_-]+\.gdf", self.mesh_file):
            raise ValueError("mesh_file must be a portable GDF basename")
        names = [lc.name for lc in self.load_cases]
        if len(set(names)) != len(names) or any(
            not re.fullmatch(r"[a-zA-Z0-9][a-zA-Z0-9_-]*", n) for n in names
        ):
            raise ValueError("load-case names must be unique portable identifiers")
        if any(n.casefold() == "diffraction" for n in names):
            raise ValueError("load-case name is reserved for workflow output")
        # Nested legacy models permit non-finite floats; reject them here too.
        _finite_tree(self.model_dump(mode="python"))
        if self.criteria.mooring_stiffness_kN_per_m is not None:
            if self.criteria.mooring_stiffness_kN_per_m <= 0:
                raise ValueError("mooring stiffness must be positive")
        stiffness = self.criteria.mooring_stiffness_kN_per_m
        offset = self.criteria.max_offset_m
        if (stiffness is None) != (offset is None):
            raise ValueError("mooring stiffness and offset must be supplied together")
        if offset is not None and offset <= 0:
            raise ValueError("allowable offset must be positive")
        low, high = self.criteria.modal_band_s
        if not 0 < low < high:
            raise ValueError("modal period band must be positive and increasing")
        return self


def _finite_tree(value: Any) -> None:
    if isinstance(value, float) and not math.isfinite(value):
        raise ValueError("inputs must be finite")
    if isinstance(value, dict):
        for item in value.values():
            _finite_tree(item)
    if isinstance(value, (list, tuple)):
        for item in value:
            _finite_tree(item)


def _digest(data: Any) -> str:
    text = json.dumps(data, sort_keys=True, separators=(",", ":"), allow_nan=False)
    return hashlib.sha256(text.encode()).hexdigest()


@dataclass
class ChainBundle:
    """Prepared inputs plus screening and provenance; no solver results implied."""

    input_digest: str
    variant: VariantScreening
    diffraction: dict[str, Any]
    coupled_specs: dict[str, dict[str, Any]]
    manifest: dict[str, Any]


def _connected_to(generic: Any, turbine: Any, floater_name: str) -> bool:
    """Follow rigid Connection references; unsupported flexible paths fail closed."""
    targets = {}
    for field in ("vessels", "buoys_6d", "buoys_3d", "turbines"):
        for obj in getattr(generic, field, []) or []:
            connection = getattr(obj, "connection", None)
            if connection is not None or "connection" in obj.model_fields_set:
                targets[obj.name] = connection
            else:
                targets[obj.name] = obj.properties.get("Connection")
    current = turbine.name
    visited = set()
    while current not in visited:
        if current == floater_name:
            return True
        visited.add(current)
        current = targets.get(current)
    return False


def _is_link(path: Path) -> bool:
    """Junctions are links even when Windows reports is_symlink() as False."""
    if path.is_symlink() or getattr(path, "is_junction", lambda: False)():
        return True
    try:
        attributes = path.lstat()
    except FileNotFoundError:
        return False  # A new output directory has no link metadata yet.
    return bool(
        getattr(attributes, "st_file_attributes", 0) & stat.FILE_ATTRIBUTE_REPARSE_POINT
    )


def _generated_files(root: Path) -> list[Path]:
    """Check containment and reject links before descending into directories."""
    files = []
    pending = [root]
    resolved_root = root.resolve()
    while pending:
        for path in pending.pop().iterdir():
            if _is_link(path) or not path.resolve().is_relative_to(resolved_root):
                raise ValueError(
                    "generated output must remain inside its directory without links"
                )
            if path.is_dir():
                pending.append(path)
            elif path.is_file():
                files.append(path)
    return sorted(files)


def prepare_chain(inputs: ChainInput, out_dir: str | Path) -> ChainBundle:
    """Screen one sized floater and generate validated coupling inputs per sea state.

    Existing hydrodynamics are removed from the target draught rather than reused
    on a changed hull. Diffraction inertia describes the hull only; displacement
    RAOs for that bare hull must not be substituted for the coupled response.
    No mesh file, controller file or result is silently created or substituted.
    """
    out = Path(out_dir)
    if _is_link(out):
        raise ValueError("output directory must not be a link or junction")
    if out.exists() and any(out.iterdir()):
        raise ValueError("output directory must be empty; preserve existing data")
    out = out.resolve()
    shared_input = inputs.model_dump(mode="json")
    digest = _digest(shared_input)
    template = ProjectInputSpec.model_validate(copy.deepcopy(inputs.template))
    if template.generic is None:
        raise ValueError("a generic coupled template is required")
    generic = template.generic
    unit_properties = generic.general_properties
    if unit_properties.get("UnitsSystem", "SI") != "SI" or any(
        key in unit_properties
        for key in ("LengthUnits", "MassUnits", "ForceUnits", "g")
    ):
        raise ValueError(
            "coupled template must use SI units (m, t, kN) without unit overrides"
        )
    if template.environment.raw_properties.get("WaterSurfaceZ", 0) != 0:
        raise ValueError("water surface must use the zero SWL datum")
    if template.environment.water.density != inputs.water_density_kg_m3 / KG_PER_TONNE:
        raise ValueError(
            "template water density mismatch with explicit screening density"
        )
    if any(lc.steady_load_kN != 0 for lc in inputs.load_cases):
        raise ValueError("nonzero steady load has no native applied-force mapping")
    vessels = [v for v in generic.vessels if v.name == inputs.floater_name]
    turbines = [t for t in generic.turbines if t.name == inputs.turbine_name]
    if len(vessels) != 1 or len(turbines) != 1:
        raise ValueError("exactly one named floater and turbine are required")
    vessel = vessels[0]
    if vessel.initial_position != [0.0, 0.0, 0.0]:
        raise ValueError("target floater must use the centreline SWL datum")
    if vessel.connection != "Free":
        raise ValueError("target floater must be free at the SWL datum")
    if not _connected_to(generic, turbines[0], vessel.name):
        raise ValueError("turbine must be rigidly connected to the sized floater")
    types = [t for t in generic.vessel_types if t.name == vessel.vessel_type]
    if len(types) != 1:
        raise ValueError("exactly one target vessel type is required")
    if sum(v.vessel_type == vessel.vessel_type for v in generic.vessels) != 1:
        raise ValueError("target vessel type must not be shared with another vessel")
    if vessel.properties.get("VesselType", vessel.vessel_type) != vessel.vessel_type:
        raise ValueError("conflicting vessel type override")
    # The screened dimensions and mass frame are already physical: never scale
    # the target geometry a second time in the native vessel length mechanism.
    if vessel.length is not None or "Length" in vessel.properties:
        raise ValueError("remove target vessel length scaling before mapping")

    screened = screen_concept(
        inputs.archetype,
        inputs.params,
        sweeps=[],
        topside=inputs.topside,
        load_cases=inputs.load_cases,
        criteria=inputs.criteria,
    )
    variant = screened.variants[0].model_copy(update={"case_id": inputs.case_id})
    p = variant.properties
    if not p.feasible:
        raise ValueError("infeasible mass balance at the requested draft")
    # OpenStax (2016), §9.6: M*z = sum(m_i*z_i), using explicit ballast VCG.
    mass_t = p.total_mass_t - inputs.topside.total_mass_t
    hull_z = (
        p.steel_mass_t * inputs.params["steel_vcg_frac"] * p.draft_m
        + p.ballast_mass_t * inputs.ballast_vcg_m
    ) / mass_t - p.draft_m
    cog = [0.0, 0.0, hull_z]  # Centreline/SWL datum, defined by ChainInput.
    # OpenStax (2016), §10.5: I_axis = m*k_axis² about the hull-only CG.
    inertias = [mass_t * r**2 for r in inputs.hull_radii_m]
    tensor = [[inertias[0], 0.0, 0.0], [0.0, inertias[1], 0.0], [0.0, 0.0, inertias[2]]]
    draught = {
        "Name": "SizedDraught",
        "Mass": mass_t,
        "CentreOfMass": cog,
        "MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ": tensor,
    }
    types[0].properties["Draughts"] = [draught]
    vessel.properties["Draught"] = "SizedDraught"

    diffraction = build_orcawave_spec(
        variant,
        mesh_file=inputs.mesh_file,
        grid=FrequencyGrid(tuple(inputs.frequencies_rad_s), tuple(inputs.headings_deg)),
        water_depth=inputs.water_depth_m,
        water_density=inputs.water_density_kg_m3,
    )
    diffraction["vessel"]["inertia"] = {
        "mass": mass_t * KG_PER_TONNE,
        "centre_of_gravity": cog,
        "radii_of_gyration": list(inputs.hull_radii_m),
    }
    # BIPM, 3rd CGPM (1901), Declaration 2: conventional standard gravity.
    diffraction["environment"]["gravity"] = G_STANDARD
    diffraction["metadata"] = {
        "description": "Hull-only diffraction input; coupled dynamics pending"
    }
    coupled = {}
    for lc in inputs.load_cases:
        spec = template.model_copy(deep=True)
        spec.metadata.name = f"{inputs.case_id}-{lc.name}"
        spec.environment.water.depth = inputs.water_depth_m
        if len(spec.environment.waves.trains) != 1:
            raise ValueError(
                "single wave train required for a shared screening sea state"
            )
        train = spec.environment.waves.trains[0]
        spectral = train.type in {
            "jonswap",
            "JONSWAP",
            "pierson_moskowitz",
            "Pierson-Moskowitz",
        }
        if not spectral:
            raise ValueError("spectral wave type required for shared Hs and Tp inputs")
        train.height = lc.hs_m
        train.period = lc.tp_s
        # The repository builder selects Tp only from a Tp-only raw train.
        # Remove inherited period keys so the shared sea state is authoritative.
        raw = spec.environment.raw_properties
        raw_trains = raw.get("WaveTrains", [])
        raw_train = (
            dict(raw_trains[0])
            if isinstance(raw_trains, list)
            and raw_trains
            and isinstance(raw_trains[0], dict)
            else {}
        )
        raw_train.setdefault("Name", train.name)
        for key in ("WaveTp", "WaveTz", "WavePeriod"):
            raw_train.pop(key, None)
        if spectral:
            raw_train["WaveTp"] = lc.tp_s
        if spectral or raw_trains:
            raw["WaveTrains"] = [raw_train]
        coupled[lc.name] = spec.model_dump(mode="json", exclude_none=True)

    equilibrium = {
        "status": "NOT_EVALUATED",
        "displacement_t": p.displacement_t,
        "body_mass_t": mass_t,
        "missing_topside_mass_t": inputs.topside.total_mass_t,
        "criterion": "Body mass must balance displaced mass at the specified draft before static or RAO evaluation.",
        "basis": "OpenStax (2016), section 9.6; separate topside weight is absent from the diffraction body.",
    }
    not_evaluated = dict(NOT_EVALUATED)
    not_evaluated["hull_only_equilibrium"] = (
        f"Body mass {mass_t:.3f} t against displaced mass {p.displacement_t:.3f} t; "
        f"separate topside mass {inputs.topside.total_mass_t:.3f} t is absent. "
        + equilibrium["criterion"]
        + " "
        + equilibrium["basis"]
    )
    manifest = {
        "input_digest": digest,
        "case_id": inputs.case_id,
        "basis": inputs.basis,
        "screening_criteria": inputs.criteria.model_dump(mode="json"),
        "screening_passed": variant.passed,
        "solver_ready": False,
        "status": "PREPARED_OFFLINE",
        "validation_findings": [],
        "hull_only_equilibrium": equilibrium,
        "not_evaluated": not_evaluated,
        "sources": list(SOURCES),
    }
    out.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix=".chain-", dir=out.parent) as temporary:
        # Keep the staging parent private; publish a normally created child.
        # An existing empty output directory keeps its original permissions.
        stage = Path(temporary) / "output"
        stage.mkdir()
        if out.exists():
            stage.chmod(stat.S_IMODE(out.stat().st_mode))
        (stage / "shared-input.json").write_text(
            json.dumps(shared_input, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
            newline="\n",
        )
        (stage / "diffraction.yml").write_text(
            yaml.safe_dump(diffraction, sort_keys=False), encoding="utf-8", newline="\n"
        )
        for name, spec in coupled.items():
            spec_path = stage / f"{name}.yml"
            spec_path.write_text(
                yaml.safe_dump(spec, sort_keys=False), encoding="utf-8", newline="\n"
            )
            # Mandatory repository generation and post-validation; no error bypass.
            ModularModelGenerator(spec_path).generate(stage / name)
            _generated_files(stage)
            from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
                PostGenerationValidator,
            )

            for finding in PostGenerationValidator().validate_directory(
                stage / name / "includes"
            ):
                if finding.level == "error":
                    raise ValueError("post-generation validation failed")
                manifest["validation_findings"].append(
                    {
                        "case": name,
                        "level": finding.level,
                        "category": finding.category,
                        "message": finding.message,
                    }
                )
        manifest["files"] = {
            file.relative_to(stage).as_posix(): hashlib.sha256(
                file.read_bytes()
            ).hexdigest()
            for file in _generated_files(stage)
        }
        (stage / "manifest.json").write_text(
            json.dumps(manifest, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
            newline="\n",
        )
        # Removing an empty destination also supports Windows directory rename.
        # If another writer populated it meanwhile, rmdir fails and preserves it.
        if out.exists():
            out.rmdir()
        stage.rename(out)
    return ChainBundle(digest, variant, diffraction, coupled, manifest)


class ResultRecord(BaseModel):
    """Normalised stored evidence; synthetic fixtures never attest solver success."""

    model_config = ConfigDict(extra="forbid", allow_inf_nan=False)
    input_digest: str = Field(pattern=r"^[a-f0-9]{64}$")
    case_name: str = Field(min_length=1)
    # These metrics refer to the coupled assembly, not the bare diffraction hull.
    solver: Literal["OrcaFlex"]
    version: str = Field(min_length=1)
    evidence_kind: Literal["recorded", "synthetic"]
    source: str = Field(min_length=1)
    metric: Literal["heave_natural_period_s", "pitch_natural_period_s"]
    value: float = Field(gt=0)
    unit: Literal["s"]


def compare_results(
    bundle: ChainBundle, records: list[ResultRecord]
) -> list[dict[str, Any]]:
    """Compare like-unit periods only; no acceptance limit is invented.

    Relative difference = (solver - screen)/screen * 100, the mathematical
    definition of percent (BIPM SI Brochure, 9th ed., §5.4.7). The upstream DAF
    proxy is deliberately not compared to a dimensional motion amplitude.
    """
    versions = {}
    seen = set()
    rows = []
    for record in records:
        if record.input_digest != bundle.input_digest:
            raise ValueError("result input digest mismatch")
        if record.case_name not in bundle.coupled_specs:
            raise ValueError("unknown result case")
        if record.solver in versions and versions[record.solver] != record.version:
            raise ValueError("mixed solver versions are not comparable")
        versions[record.solver] = record.version
        key = (record.case_name, record.solver, record.metric)
        if key in seen:
            raise ValueError("duplicate result metric")
        seen.add(key)
        estimate = getattr(bundle.variant.properties, record.metric)
        established = math.isfinite(estimate) and estimate > 0
        rows.append(
            {
                **record.model_dump(),
                "screening": estimate if established else None,
                "difference": record.value - estimate if established else None,
                "difference_percent": (record.value / estimate - 1.0) * 100.0
                if established
                else None,
                "status": (
                    "SYNTHETIC" if record.evidence_kind == "synthetic" else "RECORDED"
                )
                if established
                else "NOT_EVALUATED",
                "explanation": "Screening uses uncoupled linear proxies; coupled differences require review.",
            }
        )
    return rows


def build_chain_report(
    bundle: ChainBundle, *, document: Any, records: list[ResultRecord] | None = None
) -> Any:
    """Build the standard report spec with evidence gaps and a way forward."""
    from digitalmodel.reporting import (
        Provenance,
        ReportSpec,
        Section,
        TableBlock,
        TextBlock,
    )

    rows = compare_results(bundle, records or [])
    p = bundle.variant.properties
    modal_low, modal_high = bundle.manifest["screening_criteria"]["modal_band_s"]

    def text(key: str, title: str, body: str) -> Any:
        return Section(key=key, title=title, blocks=[TextBlock(markdown=body)])

    sections = [
        text(
            "summary",
            "Summary",
            "Offline mapping prepared. Solver response and controller compatibility remain NOT_EVALUATED. Screening verdict: "
            + ("PASS" if bundle.variant.passed else "FAIL")
            + f"; governing check {bundle.variant.governing_check}, "
            + f"margin {bundle.variant.governing_margin:.3g} against zero. "
            + "The check values, limits and basis are listed below.",
        ),
        Section(
            key="revision-history",
            title="Revision history",
            blocks=[
                TableBlock(
                    title="Revision record",
                    columns=["Revision", "Date", "What changed and why"],
                    rows=[
                        [r.rev, r.date, r.description]
                        for r in document.revision_history
                    ],
                )
            ],
        ),
        text(
            "scope",
            "Introduction and scope",
            "One sized floater drives screening, hull-only diffraction inputs and coupled turbine template inputs for every supplied sea state. No licensed solver is imported or run.",
        ),
        text("references", "References and traceability", "\n\n".join(SOURCES)),
        text("basis", "Design data and basis", bundle.manifest["basis"]),
        text(
            "method",
            "Methodology and assumptions",
            "Hull mass = total mass - RNA mass - tower mass. Hull CG above SWL = (steel mass × steel VCG above keel + ballast mass × explicit ballast VCG above keel) / hull mass - draft. Hull principal inertia = hull mass × supplied radius squared. Mass first moments follow OpenStax (2016), section 9.6; inertia follows section 10.5. Aligned axes and centreline CGs are assumed. The template must carry RNA and tower separately. No stale diffraction data is retained in the target draught.",
        ),
        Section(
            key="results",
            title="Results and case coverage",
            blocks=[
                TableBlock(
                    title="Mass and CG mapping",
                    columns=["Quantity", "Screening value", "Unit"],
                    rows=[
                        ["Displacement", f"{p.displacement_t:.1f}", "t"],
                        ["Ballast", f"{p.ballast_mass_t:.1f}", "t"],
                        [
                            "Hull mass",
                            f"{bundle.diffraction['vessel']['inertia']['mass'] / KG_PER_TONNE:.1f}",
                            "t",
                        ],
                        [
                            "Hull CG above SWL",
                            f"{bundle.diffraction['vessel']['inertia']['centre_of_gravity'][2]:.1f}",
                            "m",
                        ],
                    ],
                    source="shared screening input and first-moment balance",
                ),
                TextBlock(markdown="Cases: " + ", ".join(bundle.coupled_specs)),
                TableBlock(
                    title="Screening criteria and basis",
                    columns=["Check", "Value", "Limit", "Margin", "Unit", "Basis"],
                    rows=[
                        [
                            check.name,
                            f"{check.value:.3g}",
                            f"{modal_low:.3g}\u2013{modal_high:.3g} (excluded)"
                            if check.name == "modal"
                            else f"{check.limit:.3g}",
                            f"{check.margin:.3g}",
                            {
                                "stability": "m",
                                "modal": "s",
                                "motion": "1",
                                "mooring": "m",
                            }[check.name],
                            bundle.manifest["basis"],
                        ]
                        for check in bundle.variant.checks
                    ],
                    source="Explicit shared-input screening criteria. Modal periods must lie outside the excluded band; the signed margin uses the nearer edge. Other checks pass when their margin is nonnegative.",
                ),
            ],
        ),
    ]
    if rows:
        sections.append(
            Section(
                key="comparison",
                title="Screening and stored evidence",
                blocks=[
                    TableBlock(
                        title="Period differences",
                        columns=[
                            "Case",
                            "Metric",
                            "Solver/version",
                            "Status",
                            "Screen (s)",
                            "Evidence (s)",
                            "Difference (s)",
                            "Difference (%)",
                        ],
                        rows=[
                            [
                                r["case_name"],
                                r["metric"],
                                r["solver"] + "/" + r["version"],
                                r["status"],
                                *[
                                    f"{r[k]:.1f}"
                                    if r[k] is not None
                                    else "NOT_EVALUATED"
                                    for k in (
                                        "screening",
                                        "value",
                                        "difference",
                                        "difference_percent",
                                    )
                                ],
                            ]
                            for r in rows
                        ],
                        source="; ".join(r["source"] for r in rows),
                    ),
                    TextBlock(
                        markdown="Differences are descriptive; no acceptance limit is supplied. Screening uses uncoupled proxies; explanations are not established by the stored values alone."
                    ),
                ],
            )
        )
    else:
        sections.append(
            text(
                "comparison",
                "Screening and stored evidence",
                "No solver results supplied. Comparisons: NOT_EVALUATED.",
            )
        )
    sections.extend(
        [
            Section(
                key="limits",
                title="Conclusions and limitations",
                blocks=[
                    TableBlock(
                        title="Evidence gaps",
                        columns=["Check", "Status", "Way forward"],
                        rows=[
                            [k, "NOT_EVALUATED", v]
                            for k, v in bundle.manifest["not_evaluated"].items()
                        ],
                    )
                ],
            ),
            text(
                "next",
                "Way forward",
                "Review the mass inventory and turbine/controller arrangement, supply and check the wetted mesh, import new diffraction coefficients, then validate native loading and run static, decay, operating and parked cases on recorded solver versions. Retain the input digest with every stored result. No comparison here establishes design acceptance.",
            ),
        ]
    )
    provenance = Provenance().add(
        "input",
        "shared-input",
        digest="sha256:" + bundle.input_digest,
        description="Canonical sizing and template input",
    )
    return ReportSpec(
        document=document,
        sections=sections,
        provenance=provenance,
        input_echo={
            "case": bundle.variant.case_id,
            "input_digest": bundle.input_digest,
            "screening_input": "shared-input.json",
        },
    )
