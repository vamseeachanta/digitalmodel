# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Assumption-based soil sensitivity using the layered penetration envelope.

Margins describe load to the first reachable descending branch. No-loss curves
have no punch-through margin; finite-depth support is reported independently.
All probabilities are conditional on the chosen model and input distributions.
"""

from __future__ import annotations

import copy
import csv
import math
import random
from contextlib import nullcontext
from pathlib import Path

from digitalmodel.geotechnical.spudcan import (
    LayeredProfile,
    SoilLayer,
    Spudcan,
    finite,
    penetration_curve,
)


def nominal_values(case: dict) -> dict[str, float]:
    """Read each uncertain input's nominal value from the complete input case."""
    layers = case['input']['profile']['layers']
    result = {}
    for name in sorted(case['parameters']):
        if name == 'top_thickness_m':
            result[name] = layers[0]['bottom_m'] - layers[0]['top_m']
        else:
            _, index, field = name.split('.')
            result[name] = layers[int(index)][field]
    return result


def validate_case(case: dict) -> None:
    """Check distribution coverage, nominal values and coupled geometry bounds.

    Each clay strength, any surface sand friction angle and an interface-bearing
    top-layer thickness must be represented exactly once. A uniform half-space
    control has no thickness interface to vary. Deeper boundaries stay fixed.
    """
    layers = case['input']['profile']['layers']
    expected = {f'layers.{i}.su_kpa' for i, layer in enumerate(layers) if layer['kind'] == 'clay'}
    expected.update(f'layers.{i}.friction_deg' for i, layer in enumerate(layers) if layer['kind'] == 'sand')
    if len(layers) > 1:
        expected.add('top_thickness_m')
    if set(case['parameters']) != expected:
        raise ValueError('uncertainty inputs must cover exactly the active soil parameters')
    nominal = nominal_values(case)
    for name, parameter in case['parameters'].items():
        if parameter['distribution'] != 'uniform':
            raise ValueError('only explicitly bounded uniform distributions are supported')
        low, high = parameter['low'], parameter['high']
        finite(name, low, strict=True)
        finite(name, high, minimum=low)
        if not low <= nominal[name] <= high:
            raise ValueError(f'{name}: nominal value lies outside the distribution bounds')
        if name == 'top_thickness_m' and high >= layers[1]['bottom_m']:
            raise ValueError('top thickness could overlap the next fixed interface')
        if name.endswith('friction_deg') and not 20 <= low <= high <= 40:
            raise ValueError('friction range exceeds the layered method bounds')
        if name == 'top_thickness_m' and layers[0]['kind'] == 'sand':
            if high > case['input']['spudcan']['diameter_m']:
                raise ValueError('sand thickness range exceeds the layered method bounds')
    # The inherited API validates every fixed input and the nominal depth span.
    _evaluate_input(_realize(case, nominal))


def _realize(case: dict, values: dict[str, float]) -> dict:
    """Copy a realization, moving both sides of the uncertain first interface."""
    data = copy.deepcopy(case['input'])
    layers = data['profile']['layers']
    for name, value in values.items():
        if name == 'top_thickness_m':
            layers[0]['bottom_m'] = layers[0]['top_m'] + value
            layers[1]['top_m'] = layers[0]['bottom_m']
        else:
            _, index, field = name.split('.')
            layers[int(index)][field] = value
    return data


def realize(case: dict, values: dict[str, float]) -> dict:
    """Return a complete replayable input after validating bounds and coverage."""
    validate_case(case)
    if set(values) != set(case['parameters']):
        raise ValueError('realization values must cover exactly the uncertain inputs')
    for name, value in values.items():
        bounds = case['parameters'][name]
        finite(name, value, minimum=bounds['low'])
        if value > bounds['high']:
            raise ValueError(f'{name}: realization exceeds the upper bound')
    return _realize(case, values)


def _evaluate_input(data: dict) -> dict:
    """Evaluate full tip interval; the inherited API inserts exact transitions."""
    profile_data = data['profile']
    profile = LayeredProfile(
        tuple(SoilLayer(**layer) for layer in profile_data['layers']),
        profile_data['sand_method'], profile_data['spread_ratio'],
        profile_data.get('punching_coefficient'),
    )
    curve = penetration_curve(profile, Spudcan(**data['spudcan']), data['tip_interval_m'],
                              preload_kn=data['preload_kn'])
    # For each event, any higher earlier reaction must also be traversed. The
    # minimum of those running maxima is the load at the first reachable loss.
    thresholds = [max(point.load_kn for point in curve.points if point.depth_m <= event.peak.depth_m)
                  for event in curve.events]
    threshold = min(thresholds) if thresholds else None
    return {
        'punch_through': curve.punch_through_flag,
        'loss_threshold_kn': threshold,
        'margin_kn': None if threshold is None else threshold - data['preload_kn'],
        'unsupported': curve.preload_support_depth_m is None,
        'preload_support_tip_m': curve.preload_support_depth_m,
        'capacity_loss_count': len(curve.events),
    }


def evaluate(case: dict, values: dict[str, float]) -> dict:
    """Evaluate one bounded soil realization through penetration_curve()."""
    return _evaluate_input(realize(case, values))


def wilson_interval(successes: int, total: int) -> tuple[float, float]:
    """Return a two-sided 95% Wilson score interval for a binomial probability.

    Inversion of (k-n*p)^2 <= z^2*n*p*(1-p), z=1.959963984540054.
    This interval quantifies sampling uncertainty, excluding model uncertainty.
    """
    if (type(successes) is not int or type(total) is not int
            or total <= 0 or not 0 <= successes <= total):
        raise ValueError('binomial counts must be integers with 0 <= successes <= total')
    z = 1.959963984540054
    z2 = z * z
    center = (successes + z2 / 2) / (total + z2)
    half = z * math.sqrt(successes * (total - successes) / total + z2 / 4) / (total + z2)
    low = 0.0 if successes == 0 else max(0.0, center - half)
    high = 1.0 if successes == total else min(1.0, center + half)
    return low, high


def monte_carlo(case: dict, *, n: int, seed: int, sample_path: Path | None = None) -> dict:
    """Sample independent uniforms in lexical input order with a local RNG.

    Every input consumes one Random.random draw, including a fixed interval.
    Each profile starts from the stated seed, so input file order cannot affect
    results. A sand method comparison uses common random numbers. There is no
    rejection sampling or silent replacement of invalid realizations.
    """
    validate_case(case)
    if type(n) is not int or n <= 0 or type(seed) is not int:
        raise ValueError('sample count must be a positive integer and seed an integer')
    rng = random.Random(seed)
    names = sorted(case['parameters'])
    count = unsupported = no_loss = 0
    stream = sample_path.open('w', newline='', encoding='utf-8') if sample_path else nullcontext()
    with stream as output:
        writer = csv.writer(output, lineterminator='\n') if output is not None else None
        if writer:
            writer.writerow(['sample', *names, 'punch_through', 'margin_kn', 'unsupported'])
        for index in range(n):
            values = {name: case['parameters'][name]['low']
                      + (case['parameters'][name]['high'] - case['parameters'][name]['low']) * rng.random()
                      for name in names}
            result = _evaluate_input(_realize(case, values))
            count += int(result['punch_through'])
            unsupported += int(result['unsupported'])
            no_loss += int(result['margin_kn'] is None)
            if writer:
                writer.writerow([index, *values.values(), result['punch_through'],
                                 result['margin_kn'], result['unsupported']])
    return {'n': n, 'seed': seed, 'punch_through_count': count,
            'probability': count / n, 'interval_95': list(wilson_interval(count, n)),
            'unsupported_count': unsupported, 'no_loss_count': no_loss}


def tornado(case: dict) -> tuple[list[dict], dict]:
    """Calculate one-at-a-time bound changes and retain complete named cases.

    A positive margin is preload headroom to a descending capacity threshold;
    zero reaches it. Null means no descending branch in the evaluated interval.
    A missing margin at either bound produces a null difference, never zero.
    """
    validate_case(case)
    nominal = nominal_values(case)
    reference = _evaluate_input(_realize(case, nominal))
    cases = {'nominal': {'values': nominal, 'input': _realize(case, nominal), 'result': reference}}
    rows = []
    for name in sorted(case['parameters']):
        row = {'profile': case['name'], 'parameter': name, 'nominal': nominal[name],
               'nominal_margin_kn': reference['margin_kn']}
        for bound in ('low', 'high'):
            values = dict(nominal, **{name: case['parameters'][name][bound]})
            data = _realize(case, values)
            result = _evaluate_input(data)
            cases[f'{name}.{bound}'] = {'values': values, 'input': data, 'result': result}
            row[f'{bound}_value'] = values[name]
            row[f'{bound}_margin_kn'] = result['margin_kn']
            row[f'{bound}_delta_kn'] = (None if result['margin_kn'] is None or reference['margin_kn'] is None
                                      else result['margin_kn'] - reference['margin_kn'])
            row[f'{bound}_has_loss'] = result['margin_kn'] is not None
        rows.append(row)
    return rows, cases
