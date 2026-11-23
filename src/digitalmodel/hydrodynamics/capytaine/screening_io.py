# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Read the public reference inputs without a licensed solver.

Conversion uses tonne and kilonewton coefficient units; rotational responses
remain in radians per metre. The workbook does not state units. These
interpretations require long-period scale checks and engineering acceptance.
"""

from hashlib import sha256
from pathlib import Path
from typing import Any

import numpy as np
import yaml

from digitalmodel.hydrodynamics.diffraction.output_schemas import HydrodynamicMatrix
from digitalmodel.hydrodynamics.hull_library.rao_extractor import PipelineXlsxReader
from .screening import rigid_body_mass_matrix


REFERENCE_DIGEST = '3a560ee394e31037b6ffdd696db9a768bfa79051dde2210e4fcd0a272625f131'


def read_reference(root: Path = Path('.')) -> dict[str, Any]:
    """Read and validate the held workbook and public deck.

    Args:
        root: Repository root containing the held inputs.

    Returns:
        SI coefficients, radian responses, deck properties and source digests.

    Raises:
        ValueError: If the workbook digest, sheets or grid differ from the basis.
    """
    workbook = root / 'tests/fixtures/solver/L02_OC4_semi_sub.xlsx'
    if sha256(workbook.read_bytes()).hexdigest() != REFERENCE_DIGEST:
        raise ValueError('The held workbook digest differs from the analysis basis')
    reader = PipelineXlsxReader(workbook)
    try:
        if set(reader._wb.sheetnames) != {'Summary', 'RAOs', 'AddedMass', 'Damping', 'Discretization'}:
            raise ValueError('The held workbook sheets differ from the analysis basis')
        response = reader.read_rao_data('OC4')
        coefficients = reader.read_hydro_coefficients()
    finally:
        reader._wb.close()
    periods = 2 * np.pi / response.frequencies
    if len(periods) != 32 or not np.allclose([min(periods), max(periods)], [3.75, 30]):
        raise ValueError('The held period grid differs from the analysis basis')
    if response.directions.shape != (9,) or not np.allclose(response.directions, np.arange(0, 181, 22.5)):
        raise ValueError('The held heading grid differs from the analysis basis')
    deck_path = root / 'docs/domains/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub.yml'
    deck = yaml.safe_load(deck_path.read_text())
    properties = _reference_properties(deck['Bodies'][0])
    return dict(properties, periods_s=periods, headings_deg=response.directions,
                amplitude=response.amplitudes, phase_deg=response.phases,
                added_mass=1000 * coefficients.added_mass,
                damping=1000 * coefficients.damping,
                workbook_sha256=REFERENCE_DIGEST,
                deck_sha256=sha256(deck_path.read_bytes()).hexdigest())


def _reference_properties(body: dict[str, Any]) -> dict[str, Any]:
    """Convert deck tonne and kilonewton properties to SI about the origin."""
    mass = 1000 * float(body['BodyMass'])
    cog = np.asarray(body['BodyCentreOfMass'], dtype=float)
    inertia_key = 'BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz'
    prefix = 'BodyExternal'
    suffix = 'Matrixx, BodyExternal{kind}Matrixy, BodyExternal{kind}Matrixz, BodyExternal{kind}MatrixRx, BodyExternal{kind}MatrixRy, BodyExternal{kind}MatrixRz'
    matrices = {}
    for kind in ('Stiffness', 'Damping'):
        key = prefix + kind + suffix.format(kind=kind)
        matrices['external_' + kind.lower()] = 1000 * np.asarray(body[key], dtype=float)
    return dict(matrices, mass_kg=mass, cog=cog,
                inertia=rigid_body_mass_matrix(mass, cog, 1000 * np.asarray(body[inertia_key], dtype=float)))


def diffraction_results(bem: Any, response: np.ndarray,
                        hydrostatics: dict[str, Any], created_date: str) -> Any:
    """Build the repository result schema with SI matrices and degree rotations.

    Args:
        bem: Ordered BEMResult from the existing solver wrapper.
        response: Complex responses in metres or radians per metre of wave amplitude.
        hydrostatics: Filled SI HydrostaticResults fields, excluding the identifier.
        created_date: UTC analysis date.

    Returns:
        DiffractionResults with solver source and an explicit time convention.
    """
    from digitalmodel.hydrodynamics.diffraction.output_schemas import (
        ADDED_MASS_UNITS, DAMPING_UNITS, AddedMassSet, DampingSet,
        DiffractionResults, DOF, FrequencyData, HeadingData, HydrodynamicMatrix,
        HydrostaticResults, RAOComponent, RAOSet,
    )
    frequencies = FrequencyData(bem.omegas, None, 0, 0, 0)
    headings = HeadingData(np.rad2deg(bem.headings), 0, 0, 0)
    components: dict[str, RAOComponent] = {}
    for i, name in enumerate(bem.dof_names):
        magnitude = np.abs(response[:, :, i]) * (1 if i < 3 else 180 / np.pi)
        components[name.lower()] = RAOComponent(
            DOF[name.upper()], magnitude, np.rad2deg(np.angle(response[:, :, i])),
            frequencies, headings, '',
        )
    metadata: dict[str, Any] = dict(vessel_name=bem.body_name, analysis_tool='Capytaine',
                                    water_depth=bem.water_depth, created_date=created_date)
    added = AddedMassSet(**metadata, frequencies=frequencies, matrices=_matrix_records(
        bem.added_mass, bem.omegas, 'added_mass', ADDED_MASS_UNITS,
    ))
    damping = DampingSet(**metadata, frequencies=frequencies, matrices=_matrix_records(
        bem.radiation_damping, bem.omegas, 'damping', DAMPING_UNITS,
    ))
    return DiffractionResults(
        **metadata, raos=RAOSet(**metadata, surge=components['surge'],
                              sway=components['sway'], heave=components['heave'],
                              roll=components['roll'], pitch=components['pitch'],
                              yaw=components['yaw']),
        added_mass=added, damping=damping,
        hydrostatics=HydrostaticResults(vessel_name=bem.body_name, **hydrostatics),
        phase_convention='unknown', unit_system='SI',
        notes='Capytaine exp(-i omega t); phase is the complex argument in degrees. '
              'Rotational response magnitudes are stored in deg/m.',
    )


def _matrix_records(values: np.ndarray, omegas: np.ndarray, kind: str,
                    units: dict[str, str]) -> list[HydrodynamicMatrix]:
    """Retain each native matrix as an explicitly typed solver-source record."""
    return [HydrodynamicMatrix(matrix, float(omega), kind, units, source='solver')
            for matrix, omega in zip(values, omegas)]
