# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run an offline generic jack-up screening example; write outputs externally.

Usage: PYTHONPATH=src python examples/domains/geotechnical/jackup_screening.py OUTPUT
No site-specific environmental envelope is claimed. The structural envelope
uses a synthetic load sensitivity, not an environmental transfer function.
"""

import json
import math
import sys
from dataclasses import asdict
from pathlib import Path

from digitalmodel.geotechnical.spudcan import SandOverClay, penetration_curve, punch_through_screen
from digitalmodel.marine_ops.jackup_inputs import frozen_inputs
from digitalmodel.marine_ops.jackup_screening import screen_three_leg_unit

INPUTS = Path(__file__).resolve().parents[2] / 'data' / 'jackup_screening'


def make_profile(data):
    """Build a generic footing on the selected public Gulf soil profile."""
    area = data['unit']['spudcan_area_m2']['value']
    soil = data['soil']
    params = {key: soil[key] for key in (
        'sand_thickness_m', 'interface_su_kpa', 'clay_gradient_kpa_m',
        'sand_submerged_weight_kn_m3', 'clay_submerged_weight_kn_m3', 'relative_density')}
    params.update({key: data['method_assumptions'][key] for key in (
        'critical_state_friction_deg', 'log_crushing_strength_kpa', 'base_cone_angle_deg')})
    return SandOverClay(diameter_m=math.sqrt(4 * area / math.pi), area_m2=area, **params)


def run_example():
    """Return geotechnical results and explicitly synthetic structural sensitivity."""
    from digitalmodel.marine_ops.operation_envelope import jackup_operating_envelope

    data = frozen_inputs(INPUTS)
    profile = make_profile(data)
    unit = data['unit']
    longitudinal, transverse = unit['leg_spacing_m']['value']
    supports = ((2 * longitudinal / 3, 0), (-longitudinal / 3, transverse / 2),
                (-longitudinal / 3, -transverse / 2))
    weight_kn = (unit['lightship_t']['value'] + unit['variable_load_t']['value']) * 9.81
    sensitivity = data['structural_sensitivity']

    def evaluate(hs, tp):
        # The period is intentionally inactive: no dynamic response is supplied.
        force = sensitivity['force_coefficient_kn_m2']['value'] * hs**2
        return screen_three_leg_unit(
            vertical_load_kn=weight_kn, supports_m=supports, moment_x_knm=0,
            moment_y_knm=force * sensitivity['application_height_m']['value'],
            horizontal_force_kn=force, preload_per_leg_kn=unit['preload_per_leg_kn']['value'],
            sliding_resistance_kn=sensitivity['sliding_resistance_kn']['value'])

    curve = penetration_curve(profile, [i / 10 for i in range(251)])
    if max(point.depth_m for point in curve) > data['soil']['qualified_bottom_m']:
        raise ValueError('curve extends below the published soil interval')
    result = {
        'qualification': 'generic_screening_only', 'environment_status': 'not_evaluated',
        'operating_envelope_status': 'synthetic_load_sensitivity_only',
        'curve': [asdict(point) for point in curve],
        'punch_through': asdict(punch_through_screen(profile, unit['preload_per_leg_kn']['value'])),
        'zero_environment_statics': asdict(evaluate(0, 10)),
        'static_envelope': [asdict(point) for point in jackup_operating_envelope(
            [6.0, 10.0, 14.0], [i / 4 for i in range(33)], evaluate)],
        'limits': [
            'No qualified ERA5 input, GEBCO depth or return levels are available.',
            'Public soil profile and buoy records are not colocated.',
            'Synthetic force coefficient and application height are illustrative assumptions.',
            'Preload comparison is not structural capacity or combined foundation resistance.',
            'Interpolated post-peak curve and static recovery depth do not predict dynamic penetration.',
        ],
    }
    return result


if __name__ == '__main__':
    destination = Path(sys.argv[1])
    destination.mkdir(parents=True, exist_ok=True)
    (destination / 'results.json').write_text(
        json.dumps(run_example(), indent=2, sort_keys=True, allow_nan=False) + '\n', encoding='utf-8')
    print('Generic screening results written; environmental qualification remains unavailable.')
