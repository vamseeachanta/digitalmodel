"""Validate reader units and load either documented JSON layout."""
import json
from pathlib import Path

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import load_from_json_file
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    CardData, DynacardAnalysisContext, InputParameters, PumpProperties, RodSection,
    SurveyData,
)

ROD_FIELDS = {
    'diameter_in', 'length_ft', 'material', 'modulus_psi', 'density_lb_ft3',
    'weight_lb_ft', 'wave_speed_ft_s', 'damping_rate_s_inv',
}
TOP_FIELDS = {
    'surface_card', 'strokes_per_minute', 'rod_string', 'pump',
    'fluid_density_lb_ft3', 'tubing_pressure_psi', 'tubing_anchored', 'survey',
}


def number(value, field, *, zero=False):
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f'{field} requires a finite number')
    if not np.isfinite(value) or (value < 0 if zero else value <= 0):
        raise ValueError(f'{field} requires a finite ' + ('nonnegative' if zero else 'positive') + ' number')
    return float(value)


def fields(data, allowed, required, label):
    if not isinstance(data, dict):
        raise ValueError(f'{label} requires an object')
    missing, extra = required - data.keys(), data.keys() - allowed
    if missing or extra:
        raise ValueError(f'{label}: missing {sorted(missing)}; unsupported {sorted(extra)}')


def samples(values, field):
    if not isinstance(values, list) or any(
        isinstance(v, bool) or not isinstance(v, (int, float)) or not np.isfinite(v)
        for v in values
    ):
        raise ValueError(f'{field} requires an array of finite non-boolean numeric samples')
    return values


def reader_rods(raw):
    if not isinstance(raw, list) or not raw:
        raise ValueError('rod_string requires a nonempty list of tapers, surface first')
    rods = []
    for row in raw:
        fields(row, ROD_FIELDS, {'diameter_in', 'length_ft'}, 'rod_string section')
        material = row.get('material', 'steel')
        if not isinstance(material, str) or material.lower() != 'steel':
            raise ValueError('rod_string.material requires the string steel; other materials are unsupported')
        names = {'diameter_in': 'diameter', 'length_ft': 'length',
                 'modulus_psi': 'modulus_of_elasticity', 'density_lb_ft3': 'density',
                 'weight_lb_ft': 'weight_per_foot', 'wave_speed_ft_s': 'speed_of_sound',
                 'damping_rate_s_inv': 'damping_factor'}
        values = {dest: number(row[key], f'rod_string.{key}', zero=key == 'damping_rate_s_inv')
                  for key, dest in names.items() if key in row}
        rods.append(RodSection(**values))
    return rods


def reader_context(raw):
    fields(raw, TOP_FIELDS, {'surface_card', 'rod_string', 'pump', 'strokes_per_minute'}, 'reader card')
    fields(raw['surface_card'], {'position_in', 'load_lbf'}, {'position_in', 'load_lbf'}, 'surface_card')
    fields(raw['pump'], {'diameter_in', 'depth_ft'}, {'diameter_in', 'depth_ft'}, 'pump')
    if 'tubing_anchored' in raw and raw['tubing_anchored'] is not True:
        raise ValueError('tubing_anchored must be true; unanchored tubing movement is not modelled')
    speed = number(raw['strokes_per_minute'], 'strokes_per_minute')
    survey = None
    if 'survey' in raw:
        fields(raw['survey'], {'measured_depth_ft', 'inclination_deg', 'azimuth_deg'},
               {'measured_depth_ft', 'inclination_deg', 'azimuth_deg'}, 'survey')
        for key, values in raw['survey'].items():
            samples(values, 'survey.' + key)
        survey = SurveyData(measured_depth=raw['survey']['measured_depth_ft'],
                            inclination=raw['survey']['inclination_deg'], azimuth=raw['survey']['azimuth_deg'])
    return DynacardAnalysisContext(
        api14='reader', surface_card=CardData(position=samples(raw['surface_card']['position_in'], 'surface_card.position_in'),
                                             load=samples(raw['surface_card']['load_lbf'], 'surface_card.load_lbf')),
        rod_string=reader_rods(raw['rod_string']), survey=survey, spm=speed,
        pump=PumpProperties(diameter=number(raw['pump']['diameter_in'], 'pump.diameter_in'),
                            depth=number(raw['pump']['depth_ft'], 'pump.depth_ft')),
        fluid_density=number(raw.get('fluid_density_lb_ft3', 62.4), 'fluid_density_lb_ft3', zero=True),
        input_params=InputParameters(strokes_per_minute=speed,
                                     tubing_pressure=number(raw.get('tubing_pressure_psi', 100),
                                                            'tubing_pressure_psi', zero=True)),
    )


def validate_context(ctx):
    position, load = (np.asarray(v, dtype=float) for v in
                      (ctx.surface_card.position, ctx.surface_card.load))
    if (position.ndim != 1 or load.ndim != 1 or len(position) != len(load)
            or not len(position) or not np.all(np.isfinite(position))
            or not np.all(np.isfinite(load))):
        raise ValueError('surface_card requires equal-length finite position_in and load_lbf arrays')
    count = len(position) - int(position[0] == position[-1] and load[0] == load[-1])
    if count < 30 or np.ptp(position) <= 0 or np.ptp(load) <= 0:
        raise ValueError('surface_card requires at least 30 usable time samples and nonzero stroke/load ranges; a repeated closing pair is excluded')
    number(ctx.spm, 'strokes_per_minute')
    number(ctx.fluid_density, 'fluid_density_lb_ft3', zero=True)
    number(ctx.pump.depth, 'pump.depth_ft')
    number(ctx.pump.diameter, 'pump.diameter_in')
    if not ctx.rod_string:
        raise ValueError('rod_string requires a nonempty list')
    for rod in ctx.rod_string:
        for key in ('length', 'diameter', 'modulus_of_elasticity', 'density',
                    'weight_per_foot', 'speed_of_sound'):
            number(getattr(rod, key), 'rod_string.' + key)
        number(rod.damping_factor, 'rod_string.damping_rate_s_inv', zero=True)
    if ctx.survey is not None:
        s = ctx.survey
        a = np.asarray([s.measured_depth, s.inclination, s.azimuth], dtype=float)
        if (a.ndim != 2 or a.shape[1] < 2 or not np.all(np.isfinite(a))
                or np.any(np.diff(a[0]) <= 0) or np.any(a[1] < 0) or np.any(a[1] > 180)
                or a[0, 0] != 0 or a[0, -1] < ctx.rod_length):
            raise ValueError('survey requires finite paired stations from surface through the rod length with increasing measured depth')


def load_card(path):
    try:
        raw = json.loads(Path(path).read_text(encoding='utf-8'))
    except (OSError, ValueError) as exc:
        raise ValueError('card input must be a readable JSON object') from exc
    if not isinstance(raw, dict):
        raise ValueError('card input must be a JSON object')
    try:
        if 'surface_card' in raw:
            ctx = reader_context(raw)
        else:
            if not {'cardData', 'equipmentData', 'InputParameters'} <= raw.keys():
                raise ValueError('card input requires the documented reader or fixture layout')
            legacy_required(raw)
            ctx = load_from_json_file(path)
        validate_context(ctx)
    except (KeyError, TypeError) as exc:
        raise ValueError('card input fields require the documented objects and arrays') from exc
    return ctx, raw


def legacy_required(raw):
    """Reject missing core inputs rather than accept the legacy loader defaults."""
    for key in ('Position', 'Load'):
        if key not in raw['cardData']:
            raise ValueError('surface_card requires position and load arrays')
        samples(raw['cardData'][key], 'surface_card.' + key)
    if 'StrokesPerMinute' not in raw['InputParameters']:
        raise ValueError('strokes_per_minute is required')
    equipment = raw['equipmentData']
    if not equipment.get('Rods') or any(not {'Diameter', 'TotalLength'} <= r.keys() for r in equipment['Rods']):
        raise ValueError('rod_string diameter and length are required for each taper')
    if not {'Diameter', 'Depth'} <= equipment.get('Pump', {}).keys():
        raise ValueError('pump diameter and depth are required')
