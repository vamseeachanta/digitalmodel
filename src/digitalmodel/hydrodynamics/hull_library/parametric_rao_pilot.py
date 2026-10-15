# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Resumable open-source hull-family pilot using the existing RAO database."""

from __future__ import annotations

import hashlib
import json
import re
from datetime import datetime, timezone
from itertools import product
from pathlib import Path

import numpy as np

from .rao_database import RAODatabase, RAODatabaseEntry

DOFS = ["Surge", "Sway", "Heave", "Roll", "Pitch", "Yaw"]


def digest(path):
    """Return the digest of saved bytes."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def write_json(path, value):
    """Write deterministic finite JSON with one final newline."""
    Path(path).write_text(json.dumps(value, sort_keys=True, indent=2, allow_nan=False) + "\n", encoding="utf-8")


def case_table(basis):
    """Validate and enumerate the complete Cartesian family before solving."""
    keys = ("length_m", "beam_m", "draft_m")
    dimensions = basis["dimensions"]
    for key in keys:
        values = np.asarray(dimensions[key], float)
        if len(values) == 0 or not np.isfinite(values).all() or (values <= 0).any() or len(set(values)) != len(values):
            raise ValueError("Dimensions must be unique finite positive values")
    for key in ("omega_rad_s", "heading_deg"):
        values = np.asarray(basis[key], float)
        if not len(values) or not np.isfinite(values).all() or len(set(values)) != len(values):
            raise ValueError("Axes must be finite, nonempty and unique")
    if min(basis["omega_rad_s"]) <= 0:
        raise ValueError("Frequencies must be positive")
    cases = []
    for form in ("box", "analytic_ship"):
        for index, values in enumerate(product(*(dimensions[k] for k in keys))):
            cases.append({"case_id": f"{form}_{index:02d}", "form": form, **dict(zip(keys, values)),
                          "box_resolution": basis["box_resolution"], "ship_target_panels": basis["ship_target_panels"]})
    return cases


def geometry(case):
    """Generate outward wet panels from the repository's public analytic forms."""
    from digitalmodel.hydrodynamics.hull_library.mesh_generator import HullMeshGenerator, MeshGeneratorConfig
    from digitalmodel.hydrodynamics.hull_library.parametric_form import MonohullFormParameters, generate_profile
    from digitalmodel.solvers.benchmark.cases import BARGE, _barge_panels

    length, beam, draft = (case[k] for k in ("length_m", "beam_m", "draft_m"))
    if case["form"] == "box":
        nx, ny, nz = case["box_resolution"]
        if any(not isinstance(n, int) or n <= 0 for n in (nx, ny, nz)):
            raise ValueError("Panel subdivisions must be positive integers")
        # The reference connectivity is retained at its original resolution.
        if (nx, ny, nz) == (50, 10, 8):
            quads = np.asarray(_barge_panels(), float)
            quads *= np.array([length / BARGE["length_m"], beam / BARGE["beam_m"], draft / BARGE["draft_m"]])
        else:
            xs, ys, zs = np.linspace(-length/2, length/2, nx+1), np.linspace(-beam/2, beam/2, ny+1), np.linspace(-draft, 0, nz+1)
            quads = []
            for x0, x1 in zip(xs[:-1], xs[1:]):
                for y0, y1 in zip(ys[:-1], ys[1:]):
                    quads.append([(x0,y1,-draft),(x1,y1,-draft),(x1,y0,-draft),(x0,y0,-draft)])
                for z0, z1 in zip(zs[:-1], zs[1:]):
                    quads.extend([[(x0,-beam/2,z0),(x1,-beam/2,z0),(x1,-beam/2,z1),(x0,-beam/2,z1)],
                                  [(x1,beam/2,z0),(x0,beam/2,z0),(x0,beam/2,z1),(x1,beam/2,z1)]])
            for y0, y1 in zip(ys[:-1], ys[1:]):
                for z0, z1 in zip(zs[:-1], zs[1:]):
                    quads.extend([[(-length/2,y1,z0),(-length/2,y0,z0),(-length/2,y0,z1),(-length/2,y1,z1)],
                                  [(length/2,y0,z0),(length/2,y1,z0),(length/2,y1,z1),(length/2,y0,z1)]])
            quads = np.asarray(quads)
        vertices, inverse = np.unique(quads.reshape(-1, 3).round(6), axis=0, return_inverse=True)
        return vertices, inverse.reshape(-1, 4)
    if case["form"] != "analytic_ship":
        raise ValueError("Unknown hull form")
    params = MonohullFormParameters(length_bp=length, beam=beam, draft=draft, depth=draft,
                                   cb=4/9, wigley=True, bilge_radius_fraction=0)
    profile = generate_profile(params, name=case["case_id"])
    mesh = HullMeshGenerator().generate(profile, MeshGeneratorConfig(target_panels=case["ship_target_panels"], symmetry=False, close_ends=True))
    vertices = mesh.vertices.copy()
    vertices[:, 0] -= length / 2
    return vertices, mesh.panels


def geometry_digest(vertices, panels):
    """Hash canonical little-endian coordinates and ordered connectivity."""
    return hashlib.sha256(np.asarray(vertices, dtype="<f8").tobytes() + np.asarray(panels, dtype="<i8").tobytes()).hexdigest()


def freeze_criteria(path, run_log):
    """Append a byte-bound receipt before the first pilot computation."""
    receipt = {"criteria_sha256": digest(path), "recorded_at": datetime.now(timezone.utc).isoformat()}
    with Path(run_log).open("a", encoding="utf-8") as stream:
        stream.write(json.dumps(receipt, sort_keys=True) + "\n")
    return receipt


def validate_receipt(path, receipt, first_start):
    """Deny modified criteria or a receipt recorded after the computation."""
    if digest(path) != receipt["criteria_sha256"] or datetime.fromisoformat(receipt["recorded_at"]) >= datetime.fromisoformat(first_start):
        raise ValueError("Criteria digest/time does not precede the first run")



def point_reasons(record, basis=None):
    """Identify coefficient-sign defects and the inclusive box interior-mode band."""
    omega = record["omega_rad_s"]
    reasons = [[] for _ in omega]
    for field in ("added_mass", "radiation_damping"):
        if field not in record:
            continue  # Synthetic response-only database fixtures have no coefficients.
        values = np.asarray(record[field], float)
        if values.shape != (len(omega), 6, 6) or not np.isfinite(values).all():
            raise ValueError("Six finite coefficient rows and columns are required")
        for index, matrix in enumerate(values):
            if field == "added_mass" and matrix[2, 2] < 0:
                reasons[index].append("negative heave added mass")
            if field == "radiation_damping":
                for dof in np.flatnonzero(np.diag(matrix) < 0):
                    reasons[index].append("negative diagonal radiation damping: " + DOFS[dof])
    mode = None
    if basis is not None and record["case"]["form"] == "box":
        case = record["case"]
        k = np.pi * np.sqrt(1/case["length_m"]**2 + 1/case["beam_m"]**2)
        mode = float(np.sqrt(basis["gravity_m_s2"] * k / np.tanh(k*case["draft_m"])))
        lower = mode * (1 + basis["irregular_frequency_lower_fraction"])
        upper = mode * (1 + basis["irregular_frequency_upper_fraction"])
        for index, frequency in enumerate(omega):
            if lower <= frequency <= upper:
                reasons[index].append("within declared interior-mode band")
    return reasons, mode


def masked_indices(record):
    """Validate that every mask identifies one native frequency and its reason."""
    indices = set()
    for point in record.get("masked_points", []):
        index = point["index"]
        if (not isinstance(index, int) or not 0 <= index < len(record["omega_rad_s"])
                or index in indices or point["omega_rad_s"] != record["omega_rad_s"][index]
                or not point["reasons"]):
            raise ValueError("Invalid masked response point")
        indices.add(index)
    return indices


def mask_irregular_points(record, basis):
    """Flag native diagnostic responses; remove them when exporting the database."""
    reasons, mode = point_reasons(record, basis)
    return record | {
        "masked_points": [{"index": i, "omega_rad_s": record["omega_rad_s"][i], "reasons": why}
                          for i, why in enumerate(reasons) if why],
        "interior_mode_omega_rad_s": mode,
        "mask_relative_band": {
            "lower_fraction": basis["irregular_frequency_lower_fraction"],
            "upper_fraction": basis["irregular_frequency_upper_fraction"],
        },
        "masked_response_policy": "Diagnostic native responses retained here; removed from database and graphs",
    }


def validate_physical_points(record, basis=None):
    """Fail the build if a sign defect or interior-mode point remains unmasked."""
    masked = masked_indices(record)
    reasons, _ = point_reasons(record, basis)
    for index, why in enumerate(reasons):
        if why and index not in masked:
            raise ValueError(f"Unmasked {', '.join(why)} at {record['omega_rad_s'][index]:g} rad/s")


def database_entry(record):
    """Convert ordered SI complex responses to the established six-mode schema."""
    from digitalmodel.hydrodynamics.models import RAOData

    if not re.fullmatch(r"[0-9a-f]{64}", record["geometry_sha256"]) or not record["solver_version"]:
        raise ValueError("Geometry digest and solver version are required")
    omega, heading = np.asarray(record["omega_rad_s"], float), np.asarray(record["heading_deg"], float)
    response = np.asarray(record["rao_real"], float) + 1j * np.asarray(record["rao_imag"], float)
    if response.shape != (len(omega), len(heading), 6) or not np.isfinite(response).all():
        raise ValueError("Six finite responses are required at every axis point")
    if not np.isfinite(omega).all() or not np.isfinite(heading).all() or (omega <= 0).any() or len(set(omega)) != len(omega) or len(set(heading)) != len(heading):
        raise ValueError("Response axes must be unique and finite")
    masked = masked_indices(record)
    oi = np.asarray([i for i in np.argsort(omega) if i not in masked], dtype=int)
    if not len(oi):
        raise ValueError("No unmasked response frequencies remain")
    hi = np.argsort(heading)
    response = response[oi][:, hi]
    amplitude = np.abs(response)
    amplitude[:, :, 3:] = np.rad2deg(amplitude[:, :, 3:])
    case = record["case"]
    return RAODatabaseEntry(case["case_id"], {k: case[k] for k in ("length_m", "beam_m", "draft_m")},
                            RAOData(omega[oi], heading[hi], amplitude, np.rad2deg(np.angle(response)), case["case_id"]),
                            {"form": case["form"], "geometry_sha256": record["geometry_sha256"],
                             "masked_points": record.get("masked_points", []),
                             "interior_mode_omega_rad_s": record.get("interior_mode_omega_rad_s"),
                             "mask_relative_band": record.get("mask_relative_band"),
                             "solver": "capytaine", "solver_version": record["solver_version"],
                             "panel_count": record["panel_count"], "input_sha256": record["input_sha256"],
                             "geometry_input_sha256": record.get("geometry_input_sha256", record["geometry_sha256"]),
                             "translation_unit": "m/m", "rotation_unit": "deg/m", "phase_unit": "deg",
                             "time_convention": "exp(-i omega t)", "heading_convention": "wave propagation from positive x"})


def compare_heave(actual, reference, criteria):
    """Apply the beforehand criterion point by point without rounding."""
    actual, expected = np.asarray(actual, float), np.asarray(reference["heave_rao"], float)
    if actual.shape != expected.shape or not np.isfinite(actual).all():
        raise ValueError("Benchmark response axis does not match")
    bounds = criteria["heave_absolute_tolerance_m_per_m"] + criteria["heave_relative_tolerance"] * np.abs(expected)
    delta = np.abs(actual - expected)
    return {"passed": bool(np.all(delta <= bounds)), "omega_rad_s": reference["omega_rad_s"],
            "actual_heave_m_per_m": actual.tolist(), "reference_heave_m_per_m": expected.tolist(),
            "absolute_difference_m_per_m": delta.tolist(), "allowed_difference_m_per_m": bounds.tolist(),
            "max_fraction_of_bound": float(np.max(delta / bounds))}


def convergence_record(records, criteria):
    """Record panel-count sensitivity on a shared head-sea heave axis."""
    if len(records) < 3 or len({r["panel_count"] for r in records}) != len(records):
        raise ValueError("At least three distinct panel counts are required")
    records = sorted(records, key=lambda r: r["panel_count"])
    arrays = [np.asarray(r["heave_rao"], float) for r in records]
    if any(a.shape != arrays[0].shape or not np.isfinite(a).all() for a in arrays):
        raise ValueError("Convergence responses must share a finite axis")
    delta = float(np.max(np.abs(arrays[-1] - arrays[-2])))
    return {"panel_counts": [r["panel_count"] for r in records], "heave_m_per_m": [a.tolist() for a in arrays],
            "max_absolute_change_m_per_m": delta, "criterion_m_per_m": criteria["convergence_absolute_tolerance_m_per_m"],
            "passed": delta <= criteria["convergence_absolute_tolerance_m_per_m"],
            "scope": "Head-sea heave at the three reference sample frequencies only"}


def _write_mesh(path, vertices, panels):
    lines = ["Public analytic pilot geometry", "1.0 9.80665", "0 0", str(len(panels))]
    lines.extend(" ".join(f"{v:.17g}" for v in xyz) for xyz in vertices[panels].reshape(-1, 3))
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")


def input_digest(case, basis, geometry_sha, solver_version, receipt):
    """Bind a retained record to geometry, implementation, basis and criteria."""
    binding = {"case": case, "basis": basis, "geometry_sha256": geometry_sha,
               "solver_version": solver_version, "criteria_sha256": receipt["criteria_sha256"],
               "implementation_sha256": digest(__file__)}
    return hashlib.sha256(json.dumps(binding, sort_keys=True, allow_nan=False).encode()).hexdigest()


def read_record(path, case, basis, receipt):
    """Deny stale or tampered saved results before reusing a completed case."""
    record = json.loads(Path(path).read_text())
    checksum = record.pop("record_sha256")
    vertices, panels = geometry(case)
    geometry_sha = geometry_digest(vertices, panels)
    if record["panel_count"] != len(panels):
        raise ValueError("Retained panel count differs from its geometry")
    expected = input_digest(case, basis, geometry_sha, record["solver_version"], receipt)
    if checksum != hashlib.sha256(json.dumps(record, sort_keys=True, allow_nan=False).encode()).hexdigest() or record["input_sha256"] != expected:
        raise ValueError("Resumed record digest does not match its input")
    if record["case"] != case or record["geometry_sha256"] != geometry_sha or record["omega_rad_s"] != basis["omega_rad_s"] or record["heading_deg"] != basis["heading_deg"] or record.get("criteria_sha256") != receipt["criteria_sha256"]:
        raise ValueError("Retained record axes, geometry or criteria differ")
    database_entry(record)
    return record


def floating_body(config):
    """Create the pilot's six-mode body using the current immersion API."""
    import capytaine as cpt

    body = cpt.FloatingBody(
        mesh=cpt.load_mesh(str(config.mesh.path)),
        dofs=cpt.rigid_body_dofs(rotation_center=config.rotation_center_or_com),
        center_of_mass=config.center_of_mass,
        name=config.mesh.name,
    )
    return body.immersed_part()


def bem_solver(library, config):
    """Build the current engine with its Green function and cached LU solver."""
    engine = library.DefaultMatrixEngine(
        green_function=library.Delhommeau(), linear_solver=config.linear_solver
    )
    return library.BEMSolver(engine=engine, method=config.method)



def homogeneous_mass_properties(vertices, panels, rho):
    """Integrate a wetted surface closed at z=0, then transfer to its centroid.

    Each oriented triangle and the waterplane origin define a signed tetrahedron.
    The omitted waterplane has zero tetrahedron volume. Its first and second
    moments also vanish, so these integrals are those of the closed solid.
    """
    unique_vertices, inverse = np.unique(np.asarray(vertices, float), axis=0,
                                         return_inverse=True)
    faces = inverse[np.asarray(panels)]
    edges = np.sort(np.stack((faces, np.roll(faces, -1, axis=1)), axis=2)
                    .reshape(-1, 2), axis=1)
    face_ids = np.repeat(np.arange(len(faces)), faces.shape[1])
    nonzero = edges[:, 0] != edges[:, 1]
    face_edges = np.unique(np.column_stack((face_ids[nonzero], edges[nonzero])),
                           axis=0)
    edges, counts = np.unique(face_edges[:, 1:], axis=0, return_counts=True)
    boundary = edges[counts == 1]
    heights = np.abs(unique_vertices[boundary, 2]).max(axis=1)
    offending = heights > 1e-9
    if np.any(offending):
        raise ValueError(
            f"{np.count_nonzero(offending)} boundary edges lie off z=0; "
            f"largest |z|={heights.max():.9g} m")
    quads = np.asarray(vertices, float)[np.asarray(panels)]
    triangles = np.concatenate((quads[:, [0, 1, 2]], quads[:, [0, 2, 3]]))
    volumes = np.einsum("ij,ij->i", triangles[:, 0],
                        np.cross(triangles[:, 1], triangles[:, 2])) / 6
    volume = volumes.sum()
    if not np.isfinite(volume) or volume <= 0:
        raise ValueError("A positive outward closed volume is required")
    sums = triangles.sum(axis=1)
    centroid = np.einsum("i,ij->j", volumes, sums) / (4 * volume)
    # Tetrahedron E[xx^T] = (sum(v v^T) + sum(v) sum(v)^T) / 20.
    second = np.einsum("i,ijk->jk", volumes,
                       np.einsum("nvi,nvj->nij", triangles, triangles)
                       + np.einsum("ni,nj->nij", sums, sums)) / 20
    # Parallel-axis transfer of the raw second moment to the volume centroid.
    central = second - volume * np.outer(centroid, centroid)
    rotational = rho * (np.trace(central) * np.eye(3) - central)
    return float(rho * volume), centroid, rotational


def _solver_type(base_solver, case, basis):
    """Specialize the existing wrapper for explicit pilot loading and gravity."""
    import xarray as xr

    class PilotSolver(base_solver):
        def _build_solver(self, library):
            return bem_solver(library, self.solver_config)

        def solve(self, compute_hydrostatics=True):
            import capytaine as cpt

            valid, errors = self.validate_inputs()
            if not valid or not compute_hydrostatics:
                raise ValueError(f"Pilot requires validated inputs and hydrostatics: {errors}")
            self._body = floating_body(self.body_config)
            self._compute_and_attach_hydrostatics(cpt)
            self._solver = self._build_solver(cpt)
            self._dataset = self._run_fill_dataset(cpt, True)
            return self._extract_results()

        def _compute_and_attach_hydrostatics(self, library):
            if case["form"] == "analytic_ship":
                mass, centroid, rotational = homogeneous_mass_properties(
                    self._body.mesh.vertices, self._body.mesh.faces, basis["rho_kg_m3"])
                self._body.center_of_mass = centroid
                self._body.mass = mass
                self._body.dofs = library.rigid_body_dofs(rotation_center=centroid)
            self._body.hydrostatic_stiffness = self._body.compute_hydrostatic_stiffness(rho=basis["rho_kg_m3"], g=basis["gravity_m_s2"])
            if case["form"] == "box":
                mass = case["length_m"] * case["beam_m"] * case["draft_m"] * basis["rho_kg_m3"]
                radii = np.asarray(basis["box_gyradii_m"]) * np.array([case["beam_m"]/basis["reference_dimensions_m"][1], case["length_m"]/basis["reference_dimensions_m"][0], case["length_m"]/basis["reference_dimensions_m"][0]])
                inertia = np.diag([mass]*3 + (mass*radii**2).tolist())
                self._body.inertia_matrix = xr.DataArray(inertia, dims=["influenced_dof", "radiating_dof"], coords={"influenced_dof": DOFS, "radiating_dof": DOFS})
            else:
                inertia = np.zeros((6, 6))
                inertia[:3, :3] = mass * np.eye(3)
                inertia[3:, 3:] = rotational
                self._body.inertia_matrix = xr.DataArray(inertia, dims=["influenced_dof", "radiating_dof"], coords={"influenced_dof": DOFS, "radiating_dof": DOFS})

        def _run_fill_dataset(self, library, hydrostatics):
            matrix = xr.Dataset(coords={"omega": basis["omega_rad_s"], "wave_direction": np.deg2rad(basis["heading_deg"]), "radiating_dof": DOFS,
                                        "water_depth": [basis["water_depth_m"]], "rho": [basis["rho_kg_m3"]], "g": [basis["gravity_m_s2"]]})
            return self._solver.fill_dataset(matrix, self._body, hydrostatics=True, n_jobs=1)

    return PilotSolver


def solve_case(case, basis, scratch, receipt):
    """Run the existing wrapper sequentially with explicit gravity and inertia."""
    if not re.fullmatch(r"[a-z][a-z0-9_]*", case["case_id"]):
        raise ValueError("Case identifier must be a plain filename stem")
    import capytaine as cpt
    from digitalmodel.hydrodynamics.capytaine.models import BodyConfig, MeshConfig, SolverConfig, WaveConditions, BEMResult
    from digitalmodel.hydrodynamics.capytaine.solver import CapytaineSolver
    from digitalmodel.hydrodynamics.capytaine.rao import compute_rao_manual

    vertices, panels = geometry(case)
    geometry_sha = geometry_digest(vertices, panels)
    input_sha = input_digest(case, basis, geometry_sha, cpt.__version__, receipt)
    path = scratch / (case["case_id"] + ".json")
    if path.exists():
        return read_record(path, case, basis, receipt)
    started = datetime.now(timezone.utc).isoformat()
    mesh_path = scratch / (case["case_id"] + ".gdf")
    _write_mesh(mesh_path, vertices, panels)
    cog = (0, 0, basis["cog_draft_fraction"] * case["draft_m"])

    solver = _solver_type(CapytaineSolver, case, basis)(BodyConfig(mesh=MeshConfig(path=mesh_path, name=case["case_id"]), center_of_mass=cog),
                         WaveConditions(omegas=np.asarray(basis["omega_rad_s"]), headings=np.deg2rad(basis["heading_deg"]), rho=basis["rho_kg_m3"], water_depth=basis["water_depth_m"], g=basis["gravity_m_s2"]), SolverConfig(n_jobs=1))
    solver.solve()
    if solver.body.mesh.nb_faces != len(panels):
        raise ValueError("Solver changed the input panel count")
    ds = solver.dataset
    def coefficients(name, dims):
        return ds[name].squeeze(drop=True).transpose(*dims).sel({d: DOFS for d in dims if "dof" in d}).values
    added = coefficients("added_mass", ("omega", "influenced_dof", "radiating_dof"))
    damping = coefficients("radiation_damping", ("omega", "influenced_dof", "radiating_dof"))
    excitation = coefficients("excitation_force", ("omega", "wave_direction", "influenced_dof"))
    inertia = solver.body.inertia_matrix.sel(influenced_dof=DOFS, radiating_dof=DOFS).values
    stiffness = solver.body.hydrostatic_stiffness.sel(influenced_dof=DOFS, radiating_dof=DOFS).values
    response = compute_rao_manual(BEMResult(added_mass=added, radiation_damping=damping, excitation_force=excitation,
                                           omegas=np.asarray(basis["omega_rad_s"]), headings=np.deg2rad(basis["heading_deg"]), dof_names=DOFS), inertia, stiffness).rao
    record = {"case": case, "geometry_sha256": geometry_sha, "solver_version": cpt.__version__, "panel_count": len(panels), "input_sha256": input_sha,
              "native_coordinate_units": ["m", "m", "m", "rad", "rad", "rad"],
              "native_load_units": ["N", "N", "N", "N m", "N m", "N m"], "excitation_normalization": "per metre wave amplitude",
              "matrix_unit_rule": "inertia and added mass: load*s^2/coordinate; damping: load*s/coordinate; stiffness: load/coordinate",
              "geometry_input_sha256": digest(mesh_path), "criteria_sha256": receipt["criteria_sha256"], "started_at": started, "omega_rad_s": basis["omega_rad_s"], "heading_deg": basis["heading_deg"],
              "rao_real": response.real.tolist(), "rao_imag": response.imag.tolist(),
              # Retain native coefficient axes for compatibility with diagnostic records.
              "hydrodynamic_matrix_axes": ["omega", "radiating_dof", "influenced_dof"],
              "center_of_mass_m": np.asarray(solver.body.center_of_mass).tolist(),
              "added_mass": added.transpose(0, 2, 1).tolist(), "radiation_damping": damping.transpose(0, 2, 1).tolist(),
              "excitation_real": excitation.real.tolist(), "excitation_imag": excitation.imag.tolist(), "inertia": inertia.tolist(), "hydrostatic_stiffness": stiffness.tolist()}
    record = mask_irregular_points(record, basis)
    validate_physical_points(record, basis)
    database_entry(record)
    saved = record | {"record_sha256": hashlib.sha256(json.dumps(record, sort_keys=True, allow_nan=False).encode()).hexdigest()}
    write_json(path, saved)
    return record
