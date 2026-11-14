"""Run the synthetic approved-plan towing example without a licensed solver.

Usage from repository root: PYTHONPATH=src python examples/marine_ops/towing/towing_scenario.py
Citation resolution uses LLM_WIKI_PATH or the existing resolver search chain.
"""
from dataclasses import asdict
from importlib.resources import files
import json
import math

import yaml

from digitalmodel.hydrodynamics.models import VesselProperties
from digitalmodel.marine_ops.towing.bollard_pull import BollardPullCheck
from digitalmodel.marine_ops.towing.config_optimizer import optimize_configuration
from digitalmodel.marine_ops.towing.dnv_st_n001_checks import (
    Result, check_bridle_angle, check_fairlead, check_towline,
)
from digitalmodel.marine_ops.towing.drift_force import compute_drift_force, WindProfile
from digitalmodel.marine_ops.towing.towline_analysis import catenary_from_pull


def _load_fixture():
    fixture = files('digitalmodel.marine_ops.towing').joinpath('fixtures/barge_tug_500t_pull.yml')
    return yaml.safe_load(fixture.read_text(encoding='utf-8'))


def _line_checks(line, geometry, mbl, constraints):
    peak = geometry.top_tension_n * line['dynamic_amplification']
    leg = peak / (2 * math.cos(math.radians(line['bridle_angle_deg'] / 2)))
    reaction = 2 * leg * math.sin(math.radians(line['fairlead_deflection_deg'] / 2))
    strength = check_towline(mbl_n=mbl, static_tension_n=geometry.top_tension_n,
                             dynamic_tension_n=peak)
    citations = strength.citations
    lo, hi = constraints['scope_bounds']
    scope_margin = min(geometry.scope - lo, hi - geometry.scope)
    clearance_margin = (constraints['available_depth_m'] - geometry.sag_m
                        - constraints['minimum_clearance_m'])
    return {
        'towline': strength,
        'fairlead': check_fairlead(reaction_n=reaction, capacity_n=line['fairlead_capacity_n']),
        'bridle_angle': check_bridle_angle(line['bridle_angle_deg'],
                         minimum_deg=constraints['bridle_angle_bounds_deg'][0],
                         maximum_deg=constraints['bridle_angle_bounds_deg'][1]),
        'scope': Result(scope_margin >= 0, 'line length / span versus supplied bounds',
                        scope_margin, citations),
        'clearance': Result(clearance_margin >= 0, 'sag versus supplied depth and clearance',
                            clearance_margin, citations),
    }


def run_scenario(*, environment='benign', diameter_scale=1.0):
    """Evaluate the fixed fixture; optimization is reported without replacing its verdict."""
    if not math.isfinite(diameter_scale) or diameter_scale <= 0:
        raise ValueError('diameter scale must be finite and positive')
    config = _load_fixture()
    env, line = config['environments'][environment], config['line']
    constraints = config['constraints']
    loads = compute_drift_force(VesselProperties(**config['vessel']), env['current_speed_m_s'],
        WindProfile(env['wind_speed_m_s'], env['wind_direction_deg']),
        current_direction_deg=env['current_direction_deg'])
    tug = config['tug']
    pull = BollardPullCheck(tug['bollard_pull_t'] * 1000 * 9.80665, loads,
                            tug['efficiency'], tug['reserve_factor']).check()
    geometry = catenary_from_pull(horizontal_tension_n=pull.required_pull_n,
        length_m=line['length_m'], weight_n_m=line['weight_n_m'] * diameter_scale**2)
    mbl = line['mbl_n'] * diameter_scale**2
    checks = _line_checks(line, geometry, mbl, constraints)
    best = None
    if pull.passed:
        best = optimize_configuration(horizontal_load_n=pull.required_pull_n,
            **config['grid'], reference_diameter_m=line['diameter_m'],
            reference_weight_n_m=line['weight_n_m'], reference_mbl_n=line['mbl_n'],
            **constraints, fairlead_capacity_n=line['fairlead_capacity_n'],
            fairlead_deflection_deg=line['fairlead_deflection_deg'],
            dynamic_amplification=line['dynamic_amplification'])
    passed = pull.passed and all(check.passed for check in checks.values())
    return {'verdict': 'PASS' if passed else 'FAIL',
            'assessment': 'plan-criterion screening',
            'operational_acceptance': 'NOT_ESTABLISHED',
            'equipment_compatibility': {
                'mbl_below_bollard_pull': mbl < tug['bollard_pull_t'] * 1000 * 9.80665,
                'margin_n': mbl - tug['bollard_pull_t'] * 1000 * 9.80665,
                'basis': 'MBL versus nominal tug bollard pull; no standard strength criterion applied',
            },
            'environment': environment, 'line_length_m': line['length_m'],
            'line_diameter_m': line['diameter_m'] * diameter_scale, 'mbl_n': mbl,
            'bollard_pull': asdict(pull), 'loads': asdict(loads),
            'catenary': asdict(geometry), 'checks': {k: asdict(v) for k, v in checks.items()},
            'citations': [asdict(c) for c in checks['towline'].citations],
            'optimized': asdict(best) if best else None}


if __name__ == '__main__':
    print(json.dumps(run_scenario(), indent=2, allow_nan=False))
