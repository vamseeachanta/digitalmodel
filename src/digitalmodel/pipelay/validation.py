"""Explicit screening inputs; bounds follow docs/screening-basis.md, V1-V4."""
import math
from collections.abc import Mapping

from digitalmodel.orcaflex.pipelay_analysis import LayOperability, PipelayPipeProperties

PIPE_REQUIRED = {'outer_diameter', 'wall_thickness', 'steel_density',
                 'coating_thickness', 'coating_density', 'seawater_density'}
COMMON_DEFAULTS = {'allowable_overbend_strain': 0.002}
JLAY_DEFAULTS = {'allowable_sagbend_strain': 0.002, **COMMON_DEFAULTS}
REEL_DEFAULTS = {'allowable_reeling_strain': 0.02, 'straightener_efficiency': 0.95,
                 'residual_out_of_straightness_span_m': 12.0, **COMMON_DEFAULTS}
OUTPUT_DEFAULTS = {'directory': 'results/pipelay', 'summary_json': 'pipelay_summary.json'}


def check_keys(values, allowed, label):
    if not isinstance(values, Mapping):
        raise ValueError(f'{label} must be a mapping')
    unknown = set(values) - set(allowed)
    if unknown:
        raise ValueError(f'Unknown {label} keys: {", ".join(sorted(map(str, unknown)))}')


def require(values, keys, label):
    missing = set(keys) - set(values)
    if missing:
        raise ValueError(f'Missing required {label} keys: {", ".join(sorted(missing))}')


def finite_number(value, key, lower=0.0, upper=math.inf, inclusive=False):
    try:
        number = float(value)
    except (TypeError, ValueError, OverflowError) as error:
        raise ValueError(f'{key} must be a finite number') from error
    in_range = lower <= number <= upper if inclusive else lower < number < upper
    if isinstance(value, bool) or not math.isfinite(number) or not in_range:
        bounds = f'[{lower}, {upper}]' if inclusive else f'({lower}, {upper})'
        raise ValueError(f'{key} must be finite and in {bounds}')
    return number


def validate_settings(settings, method):
    """Reject unknown fields before missing-field checks; return applied defaults."""
    check_keys(settings, {'method', 'case', 'pipe', 'config', 'operability', 'outputs'}, 'pipelay')
    required = ({'water_depth_m', 'tower_angle_deg', 'top_tension_kN'}
                if method == 'J-Lay' else {'reel_core_radius_m'})
    defaults = JLAY_DEFAULTS if method == 'J-Lay' else REEL_DEFAULTS
    pipe_cfg, config = settings.get('pipe', {}), settings.get('config', {})
    check_keys(pipe_cfg, PipelayPipeProperties.model_fields, 'pipe')
    check_keys(config, required | defaults.keys(), 'config')
    check_keys(settings.get('case', {}), {'id', 'description'}, 'case')
    check_keys(settings.get('outputs', {}), OUTPUT_DEFAULTS, 'outputs')
    check_keys(settings.get('operability', {}), LayOperability.model_fields, 'operability')
    require(config, required, 'config')
    require(pipe_cfg, PIPE_REQUIRED, 'pipe')
    config = {**defaults, **config}
    for key, value in config.items():
        upper = {'allowable_sagbend_strain': 0.01, 'allowable_reeling_strain': 0.05,
                 'allowable_overbend_strain': 1.0}.get(key, math.inf)
        config[key] = finite_number(value, key, upper=upper,
                                    inclusive=key == 'straightener_efficiency')
    if 'straightener_efficiency' in config:
        config['straightener_efficiency'] = finite_number(
            config['straightener_efficiency'], 'straightener_efficiency', upper=1.0, inclusive=True)
    applied = {f'config.{k}': v for k, v in defaults.items() if k not in settings.get('config', {})}
    applied.update({f'pipe.{k}': f.default for k, f in PipelayPipeProperties.model_fields.items()
                    if k not in pipe_cfg})
    if 'method' not in settings:
        applied['method'] = method
    if 'operability' in settings:
        applied.update({f'operability.{k}': f.default for k, f in LayOperability.model_fields.items()
                        if k not in settings['operability']})
    return PipelayPipeProperties(**pipe_cfg), config, applied
