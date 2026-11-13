"""Router for floating offshore wind turbine (FOWT) specifications.

Converts :class:`~..schema.fowt.FowtSpec` into GenericModel-compatible dicts.
Object layout and OrcaFlex key names follow Orcina's public K03 15 MW
semi-submersible example held under ``docs/domains/orcaflex/examples/`` (hull
Vessel with VesselType, tower Line, lumped 6D buoy for the nacelle, chain
lines). The OrcaFlex Turbine object is not emitted; see the schema module
docstring for the thrust representation and its limits.
"""

from __future__ import annotations

import math
from typing import Any

from ..schema.fowt import FowtSpec
from .base_router import BaseRouter

DRAUGHT_NAME = "Draught1"


def symmetric_radiation_matrix(matrix: list[list[float]]) -> list[list[float]]:
    """Mirror the upper triangle within the source acceptance tolerance.

    OrcaFlex requires exactly symmetric added mass and radiation damping
    (Orcina help, Vessel types: stiffness, added mass and damping). The
    OrcaFlex WAMIT importer "imports only the upper right triangle of each
    matrix and sets the lower left triangle values to match" (Orcina help,
    Importing hydrodynamic data: WAMIT, read 2026-11-12); the same policy is
    applied here so that generated and natively imported vessel types agree.
    Reject a pair difference above 1 % of sqrt(abs(Mii)) * sqrt(abs(Mjj)),
    plus 1e-9 in that entry's dimensional units for roundoff when a diagonal
    is zero. The 1 % policy matches the source-data symmetry check; it is not
    a solver tolerance. Inputs remain unchanged and nonfinite values are
    rejected.
    """
    if any(not math.isfinite(value) for row in matrix for value in row):
        raise ValueError("radiation matrix entries must be finite")
    result = [list(row) for row in matrix]
    for i in range(6):
        for j in range(i + 1, 6):
            scale = math.sqrt(abs(matrix[i][i])) * math.sqrt(abs(matrix[j][j]))
            if abs(matrix[i][j] - matrix[j][i]) > 0.01 * scale + 1e-9:
                raise ValueError(
                    f"radiation matrix exceeds symmetry tolerance at ({i + 1}, {j + 1})")
            result[j][i] = matrix[i][j]
    return result


def full_hydrostatic_stiffness(spec: FowtSpec) -> list[list[float]]:
    """Return the heave/roll/pitch block in the OrcaFlex convention.

    OrcaFlex expects a matrix that includes both moment-arm contributions
    (buoyancy and weight) and removes ``(m_w z_b - m z_g) g`` from the roll
    and pitch diagonals itself when the centre of buoyancy is given (Orcina,
    OrcaFlex help, Vessel theory: stiffness, added mass and damping). The
    input block excludes the weight term, so ``m g (-z_G)`` of this hull is
    added to the roll and pitch diagonals here.
    """
    hull = spec.hull
    k = [list(row) for row in hull.hydrostatic_stiffness_excluding_gravity]
    weight_arm = hull.mass * spec.gravity * (-hull.centre_of_mass[2])
    k[1][1] += weight_arm
    k[2][2] += weight_arm
    return k


def vessel_rotation(heel: float, trim: float, heading: float) -> list[list[float]]:
    """Vessel-to-global rotation for an OrcaFlex vessel orientation (degrees).

    OrcaFlex applies heading about Z, then trim about the new y, then heel
    about the new x (Orcina, OrcaFlex help, Vessel data), so R = Rz Ry Rx.
    Positive angles are right-handed about each axis.
    """
    cx, sx = math.cos(math.radians(heel)), math.sin(math.radians(heel))
    cy, sy = math.cos(math.radians(trim)), math.sin(math.radians(trim))
    cz, sz = math.cos(math.radians(heading)), math.sin(math.radians(heading))
    return [[cz * cy, cz * sy * sx - sz * cx, cz * sy * cx + sz * sx],
            [sz * cy, sz * sy * sx + cz * cx, sz * sy * cx - cz * sx],
            [-sy, cy * sx, cy * cx]]


def buoy_attitude(r: list[list[float]]) -> list[float]:
    """6D buoy rotations 1, 2, 3 (degrees) that reproduce the rotation ``r``.

    A 6D buoy applies rotation 1 about x, then rotation 2 about the new y,
    then rotation 3 about the new z (Orcina, OrcaFlex help, 6D buoys: common
    data), so R = Rx Ry Rz; rotation 2 lies in [-90, 90] degrees.
    """
    r2 = math.asin(max(-1.0, min(1.0, r[0][2])))
    r1 = math.atan2(-r[1][2], r[2][2])
    r3 = math.atan2(-r[0][1], r[0][0])
    return [math.degrees(r1), math.degrees(r2), math.degrees(r3)]


# Written precision of the RNA principal-axes child buoy. np.linalg.eigh
# results differ between numpy builds at about 1e-14 relative, which changed
# the generated text in the 14th significant figure. Moments are written to
# 11 significant figures (rounding step at most 5e-11 of each moment, 20 times
# inside the 1e-9 tensor-reconstruction tolerance) and the attitude to 1e-9
# degree (at most 9e-12 rad). Both steps are three or more orders of
# magnitude above the numpy differences, so builds agree unless a value lies
# within about 1e-14 of a rounding boundary.
RNA_MOMENT_SIGNIFICANT_FIGURES = 11
RNA_ATTITUDE_DECIMAL_PLACES = 9


def principal_axes(inertia: list[list[float]]) -> tuple[list[float], list[list[float]]]:
    """Principal moments and the rotation whose columns are the principal axes.

    Eigenvectors are assigned to the host axes one-to-one, choosing the
    permutation with the largest summed alignment |cos| (the first such
    permutation when axes tie, e.g. two principal axes at 45 deg to two host
    axes). Each column is signed so that its diagonal entry is non-negative;
    if that leaves a reflection, the column with the smallest diagonal entry
    is reversed, so the rotation is proper (det +1) and close to the
    identity. The moments are returned in that column order.
    """
    import itertools

    import numpy as np

    values, vectors = np.linalg.eigh(np.array(inertia, dtype=float))
    columns = list(max(
        itertools.permutations(range(3)),
        key=lambda perm: sum(abs(vectors[axis, perm[axis]]) for axis in range(3)),
    ))
    p = vectors[:, columns]
    moments = values[columns]
    for k in range(3):
        if p[k, k] < 0:
            p[:, k] = -p[:, k]
    if np.linalg.det(p) < 0:
        k = int(np.argmin(np.abs(np.diag(p))))
        p[:, k] = -p[:, k]
    return [float(x) for x in moments], p.tolist()


class FowtRouter(BaseRouter):
    """Route a FowtSpec to vessel types, vessels, line types, lines and 6D buoys."""

    def route(self, spec: FowtSpec) -> dict[str, Any]:
        hull = spec.hull
        type_name = hull.type_name or f"{hull.name} type"
        line_types = [self._mooring_line_type(t) for t in spec.mooring_line_types]
        lines = []
        buoys: list[dict[str, Any]] = []

        if spec.representation == "flexible":
            line_types.extend(self._tower_line_types(spec))
            buoys.extend(self._rna_buoys(spec))
            lines.append(self._tower_line(spec))

        lines.extend(self._mooring_line(spec, line) for line in spec.mooring_lines)

        if spec.rotor_thrust is not None:
            buoys.append(self._hub_load_point(spec))

        result: dict[str, Any] = {
            "vessel_types": [{"name": type_name, "properties": self._vessel_type(spec)}],
            "vessels": [{"name": hull.name, "properties": self._vessel(spec, type_name)}],
            "line_types": line_types,
            "lines": lines,
        }
        if buoys:
            result["buoys_6d"] = buoys
        return result

    # ------------------------------------------------------------------ hull
    def _vessel_type(self, spec: FowtSpec) -> dict[str, Any]:
        hull = spec.hull
        k = full_hydrostatic_stiffness(spec)
        draught = {
            "Name": DRAUGHT_NAME,
            "Mass": hull.mass,
            "MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ":
                [list(row) for row in hull.inertia_about_centre_of_mass],
            "CentreOfMass": list(hull.centre_of_mass),
            "AMDMethod": "Frequency dependent",
            "ReferenceOrigin": [0, 0, 0],
            "ReferenceOriginDatumPosition": [0, 0, 0],
            "HydrostaticStiffnessMethod": "Displacement",
            "DisplacedVolume": hull.displaced_volume,
            "CentreOfBuoyancy": list(hull.centre_of_buoyancy),
            "HydrostaticStiffnessz, HydrostaticStiffnessRx, HydrostaticStiffnessRy": k,
            "FrequencyDependentAddedMassAndDamping": [
                {
                    "AMDPeriodOrFrequency": entry.period,
                    "AddedMassMatrixX, AddedMassMatrixY, AddedMassMatrixZ, "
                    "AddedMassMatrixRx, AddedMassMatrixRy, AddedMassMatrixRz":
                        symmetric_radiation_matrix(entry.added_mass),
                    "DampingX, DampingY, DampingZ, DampingRx, DampingRy, DampingRz":
                        symmetric_radiation_matrix(entry.damping),
                }
                for entry in hull.radiation
            ],
            "OtherDampingOrigin": [0, 0, 0],
            "OtherDampingFrameOfReference": "Matching calculation mode",
            "OtherDampingCalculatedFrom": "Total motion",
            "OtherLinearDamping": list(hull.other_linear_damping),
            "OtherQuadraticDamping": list(hull.other_quadratic_damping),
        }
        return {
            "Length": hull.length,
            "WavesReferredToBy": "period (s)",
            "Draughts": [draught],
        }

    def _vessel(self, spec: FowtSpec, type_name: str) -> dict[str, Any]:
        hull = spec.hull
        return {
            "VesselType": type_name,
            "Draught": DRAUGHT_NAME,
            "Length": None,
            "Connection": "Free",
            "InitialPosition": list(hull.initial_position),
            "Orientation": list(hull.initial_orientation),
            "IncludedInStatics": hull.included_in_statics,
            "PrimaryMotion": "Calculated (6 DOF)",
            "SuperimposedMotion": "None",
            "IncludeAppliedLoads": "No",
            "IncludeWaveLoad1stOrder": "Yes" if hull.include_wave_load_first_order else "No",
            "IncludeWaveDriftLoad2ndOrder": "No",
            "IncludeWaveDriftDamping": "No",
            "IncludeSumFrequencyLoad": "No",
            "IncludeAddedMassAndDamping": "Yes",
            "IncludeManoeuvringLoad": "No",
            "IncludeOtherDamping": "Yes",
            "IncludeCurrentLoad": "No",
            "IncludeWindLoad": "No",
            "PrimaryMotionIsTreatedAs": "Both low and wave frequency",
            "CalculateHydrostaticStiffnessAnglesBy": "Orientation",
        }

    # --------------------------------------------------------------- mooring
    @staticmethod
    def _mooring_line_type(t) -> dict[str, Any]:
        return {
            "name": t.name,
            "properties": {
                "Category": "General",
                "OD": t.outer_diameter,
                "ID": 0,
                "MassPerUnitLength": t.mass_per_length,
                "CompressionIsLimited": "Yes",
                "AllowableTension": t.minimum_breaking_load,
                "EI": [0, None],
                "EA": t.axial_stiffness,
                "PoissonRatio": 0.5,
                "GJ": 0,
                "Ca": [t.normal_added_mass_coefficient, None, t.axial_added_mass_coefficient],
                "Cd": [t.normal_drag_coefficient, None, t.axial_drag_coefficient],
                "SeabedLateralFrictionCoefficient": 0,
                "SeabedAxialFrictionCoefficient": None,
                "RayleighDampingCoefficients": "(no damping)",
            },
        }

    def _mooring_line(self, spec: FowtSpec, line) -> dict[str, Any]:
        f = line.fairlead
        a = [line.anchor_xy[0], line.anchor_xy[1], line.anchor_height_above_seabed]
        return {
            "name": line.name,
            "properties": {
                "IncludeTorsion": "No",
                "TopEnd": "End A",
                "LengthAndEndOrientations": "Explicit",
                "Representation": "Finite element",
                "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
                "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, "
                "ConnectionzRelativeTo": [
                    [spec.hull.name, f[0], f[1], f[2], 0, 90, 0, None],
                    ["Anchored", a[0], a[1], a[2], 0, 90, 0, None],
                ],
                "ConnectionxBendingStiffness, ConnectionyBendingStiffness": [[0, None], [0, None]],
                "LineType, Length, TargetSegmentLength": [
                    [line.line_type, line.length, line.target_segment_length]],
                "IncludedInStatics": "Yes",
                "StaticsStep1": "Catenary",
                "StaticsStep2": "Full statics",
            },
        }

    # ------------------------------------------------------ tower, RNA, hub
    @staticmethod
    def _tower_type_name(index: int) -> str:
        return f"Tower section {index + 1}"

    def _tower_line_types(self, spec: FowtSpec) -> list[dict[str, Any]]:
        types = []
        for index, section in enumerate(spec.tower.sections):
            types.append({
                "name": self._tower_type_name(index),
                "properties": {
                    "Category": "General",
                    "OD": section.outer_diameter,
                    "ID": 0,
                    "MassPerUnitLength": section.mass_per_length,
                    "CompressionIsLimited": "No",
                    "EI": [section.bending_stiffness, None],
                    "EA": section.axial_stiffness,
                    "PoissonRatio": section.poisson_ratio,
                    "GJ": section.torsional_stiffness,
                    "Ca": [0, None, 0],
                    "Cd": [0, None, 0],
                    "RayleighDampingCoefficients": "(no damping)",
                },
            })
        return types

    def _tower_line(self, spec: FowtSpec) -> dict[str, Any]:
        tower, rna = spec.tower, spec.rna
        sections = [[self._tower_type_name(i), s.length, tower.target_segment_length]
                    for i, s in enumerate(tower.sections)]
        sections.reverse()  # End A is the tower top
        return {
            "name": tower.name,
            "properties": {
                "IncludeTorsion": "Yes",
                "TopEnd": "End A",
                "LengthAndEndOrientations": "Explicit",
                "Representation": "Finite element",
                "Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionAzimuth, "
                "ConnectionDeclination, ConnectionGamma, ConnectionReleaseStage, "
                "ConnectionzRelativeTo": [
                    [rna.name, 0, 0, 0, 0, 180, 0, None],
                    [spec.hull.name, 0, 0, tower.base_elevation, 180, 180, 0, None],
                ],
                "ConnectionxBendingStiffness, ConnectionyBendingStiffness, "
                "ConnectionTwistingStiffness, ConnectionDamping": [
                    ["Infinity", None, "Infinity"], ["Infinity", None, "Infinity"]],
                "LineType, Length, TargetSegmentLength": sections,
                "IncludedInStatics": "Yes",
                "StaticsStep1": "Quick",
                "StaticsStep2": "Full statics",
            },
        }

    def _rna_buoys(self, spec: FowtSpec) -> list[dict[str, Any]]:
        rna = spec.rna
        inertia = rna.inertia_about_centre_of_mass
        host = self._rna_buoy(spec)
        if not any(inertia[i][j] for i in range(3) for j in range(3) if i != j):
            return [host]
        # Products of inertia: the host keeps mass, centre and tower-top axes
        # (so the tower connection, hub load point and reported motions are
        # unchanged) and a rigidly attached, massless child on the principal
        # axes carries the moments (Orcina help, 6D buoys: common data, read
        # 2026-11-13: buoy axes in the principal directions; object
        # connections: a child is rigidly attached to its parent).
        moments, axes = principal_axes(inertia)
        host["properties"]["MomentsOfInertia"] = [0, 0, 0]
        child = {
            "name": f"{rna.name} inertia",
            "properties": {
                "BuoyType": "Lumped buoy",
                "Connection": rna.name,
                "InitialPosition": list(rna.centre_of_mass_from_tower_top),
                "InitialAttitude": [round(a, RNA_ATTITUDE_DECIMAL_PLACES)
                                    for a in buoy_attitude(axes)],
                "Mass": 0,
                "MomentsOfInertia": [float(f"{m:.{RNA_MOMENT_SIGNIFICANT_FIGURES}g}")
                                     for m in moments],
                "CentreOfMass": [0, 0, 0],
                "Volume": 0,
                "CentreOfVolume": [0, 0, 0],
            },
        }
        return [host, child]

    def _rna_buoy(self, spec: FowtSpec) -> dict[str, Any]:
        rna, hull = spec.rna, spec.hull
        top = spec.tower.top_elevation
        inertia = rna.inertia_about_centre_of_mass
        if any(hull.initial_orientation[:2]):
            # Tilted start: carry the tower top with the hull attitude.
            r = vessel_rotation(*hull.initial_orientation)
            position = [p + r[i][2] * top for i, p in enumerate(hull.initial_position)]
            attitude = buoy_attitude(r)
        else:
            position = [hull.initial_position[0], hull.initial_position[1],
                        hull.initial_position[2] + top]
            attitude = [0, 0, hull.initial_orientation[2]]
        return {
            "name": rna.name,
            "properties": {
                "BuoyType": "Lumped buoy",
                "Connection": "Free",
                "DegreesOfFreedomInStatics": "All",
                "InitialPosition": position,
                "InitialAttitude": attitude,
                "Mass": rna.mass,
                # Diagonal tensor; with products, _rna_buoys moves the
                # moments to a principal-axes child buoy.
                "MomentsOfInertia": [inertia[0][0], inertia[1][1], inertia[2][2]],
                "CentreOfMass": list(rna.centre_of_mass_from_tower_top),
                "Volume": 0,
                "CentreOfVolume": [0, 0, 0],
            },
        }

    def _hub_load_point(self, spec: FowtSpec) -> dict[str, Any]:
        thrust = spec.rotor_thrust
        tilt = math.radians(thrust.shaft_tilt)
        force = [thrust.thrust * math.cos(tilt), 0.0, -thrust.thrust * math.sin(tilt)]
        if spec.representation == "flexible":
            host = spec.rna.name
            top = spec.tower.top_elevation
            position = [thrust.hub_position[0], thrust.hub_position[1],
                        thrust.hub_position[2] - top]
        else:
            host = spec.hull.name
            position = list(thrust.hub_position)
        return {
            "name": thrust.name,
            "properties": {
                "BuoyType": "Lumped buoy",
                "Connection": host,
                "InitialPosition": position,
                "InitialAttitude": [0, 0, 0],
                "Mass": 0,
                "MomentsOfInertia": [0, 0, 0],
                "CentreOfMass": [0, 0, 0],
                "Volume": 0,
                "CentreOfVolume": [0, 0, 0],
                "LocalAppliedLoads": [
                    {"Origin": [0, 0, 0], "Force": force, "Moment": [0, 0, 0]}],
            },
        }
