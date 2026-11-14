"""Sanitised input basis, model assumptions and public shape evidence."""
import hashlib
import json

from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostic_plate import load_diagnostic_plate
from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.plate_scoring import _GLOSSES
from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.measurements import loop_distance
from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import classify_shape, read_model_bytes, SHAPE_MODEL_PATH
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    rod_string_from_context, _effective_density_lb_ft3,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.damping import estimate_damping_profile
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings import units as U

TESTDATA = 'tests/marine_ops/artificial_lift/dynacard/testdata'
PLATE = TESTDATA + '/mendeley_plate/Working condition diagnosis plate.xls'
SCORE = TESTDATA + '/plate_classifier_score.json'
MODE_LABELS = {
    'NORMAL': 'Normal operation', 'FLUID_POUND': 'Fluid pound',
    'GAS_INTERFERENCE': 'Gas interference', 'GAS_LOCK': 'Gas lock',
    'ROD_PARTING': 'Rod parting', 'PUMP_TAGGING_UP': 'Tagging up',
    'PUMP_TAGGING_DOWN': 'Tagging down', 'VALVE_LEAK_TV': 'Traveling valve leakage',
    'VALVE_LEAK_SV': 'Standing valve leakage', 'PLUNGER_OUT_OF_BARREL': 'Plunger out of barrel',
    'SAND_ABRASION': 'Sand abrasion', 'STUCK_PUMP': 'Stuck pump',
    'DELAYED_TV_CLOSURE': 'Delayed traveling valve closure',
}
VALIDATION = ('The coupled Gibbs transfer is checked against closed-form cases derived from '
              'the cited equations and against the stored pump cards of five public '
              'example wells (median load nRMSE 1.3 %, maximum 2.0 %, 15 harmonics); no openly '
              'readable published worked example with tabulated card values was found, so a '
              'published-example check is NOT_EVALUATED.')


def simple_inputs(ctx):
    rods = [{'diameter_in': r.diameter, 'length_ft': r.length, 'material': 'steel',
             'modulus_psi': r.modulus_of_elasticity, 'density_lb_ft3': r.density,
             'weight_lb_ft': r.weight_per_foot, 'wave_speed_ft_s': r.speed_of_sound,
             'damping_rate_s_inv': r.damping_factor} for r in ctx.rod_string]
    return {'surface_card': {'position_in': ctx.surface_card.position, 'load_lbf': ctx.surface_card.load},
            'strokes_per_minute': ctx.spm, 'rod_string': rods,
            'pump': {'diameter_in': ctx.pump.diameter, 'depth_ft': ctx.pump.depth},
            'fluid_density_lb_ft3': ctx.fluid_density,
            'tubing_pressure_psi': ctx.input_params.tubing_pressure if ctx.input_params else 100}


def assumptions(ctx, raw):
    rods = rod_string_from_context(ctx)
    damping = estimate_damping_profile(rods.diameters, rods.coupling_diameters, rods.lengths,
                                       rods.densities, ctx.pump.diameter * U.IN2M, 2.441 * U.IN2M, 0.01)
    rows = [
        ('vertical_well', 'Vertical cross-check; default uses supplied survey or vertical fallback',
         'Deviation is not modelled in the frequency-domain cross-check. The public example well record gives a two-station survey with zero inclination.'),
        ('pump_boundary', {'rod_length_ft': ctx.rod_length, 'pump_depth_ft': ctx.pump.depth,
                           'unrepresented_length_ft': ctx.pump.depth - ctx.rod_length},
         'The supplied rod endpoint is identified with the pump boundary; qualification at the stated pump depth requires a reconciled as-run rod and completion tally.'),
        ('rod_damping', {'default_up_down_s_inv': damping.tolist(),
                         'gibbs_s_inv': [r.damping_factor for r in ctx.rod_string]},
         'Default damping is estimated from assumed annular geometry and viscosity; Gibbs rates are independently prescribed, not calibrated.'),
        ('viscosity_cp', 10.0, 'Default damping viscosity is not measured.'),
        ('tubing_inner_diameter_in', 2.441, 'Default damping annulus is not supplied.'),
        ('coupling_diameter_ratio', 1.5, 'Default damping assumes coupling diameter relative to rod diameter.'),
        ('coulomb_friction_coefficient', ctx.calc_params.coulomb_friction_coefficient,
         'No independent friction calibration is supplied.'),
        ('tubing_anchored', True, 'Tubing movement is excluded; unanchored tubing cannot be analysed by this workflow.'),
        ('steel_modulus_psi', [r.modulus_of_elasticity for r in ctx.rod_string],
         'Supplied nominal values are taken as given; omitted reader values use the package steel default.'),
        ('steel_density_lb_ft3', {'bare': [r.density for r in ctx.rod_string],
                                 'effective': [_effective_density_lb_ft3(r) for r in ctx.rod_string]},
         'Bare density defaults to steel; dynamic density in the default solver includes the supplied or derived rod weight.'),
        ('gibbs_wave_speed_ft_s', [r.speed_of_sound for r in ctx.rod_string],
         'Prescribed wave speed is independent of modulus and density; consistency is not enforced by the cross-check.'),
        ('load_cell_calibration', 'Taken as given', 'No independent load-cell calibration is supplied.'),
        ('uniform_time_sampling', True, 'Samples are assumed uniformly timed over a full cycle; geometry alone cannot establish timing.'),
        ('displacement_basis', {'runtime_hours': 24, 'volumetric_efficiency_pct': 100},
         'Continuous fillage-adjusted displacement excludes slip and is not measured production.'),
    ]
    if 'surface_card' in raw:
        if 'fluid_density_lb_ft3' not in raw:
            rows.append(('fluid_density_lb_ft3', ctx.fluid_density, 'Fluid density was omitted; the water-density default affects buoyancy.'))
        if 'tubing_pressure_psi' not in raw:
            rows.append(('tubing_pressure_psi', ctx.input_params.tubing_pressure, 'Pressure was omitted; it does not enter the reported card metrics.'))
        if any('material' not in r for r in raw['rod_string']):
            rows.append(('rod_material', 'steel', 'Unspecified material defaults to steel.'))
    elif not raw['equipmentData'].get('WellBore'):
        rows.append(('fluid_density_lb_ft3', ctx.fluid_density, 'No mixture basis was supplied; the legacy loader uses water density.'))
    else:
        rows.append(('fluid_density_lb_ft3', ctx.fluid_density,
                     'Not measured: derived by the loader from the well-test water cut and the water and oil gravities in the well record.'))
    return [{'id': key, 'status': 'ASSUMED', 'value': value, 'why': why} for key, value, why in rows]


def shape_evidence(card, root):
    record = json.loads((root / SCORE).read_text(encoding='utf-8'))
    digest = hashlib.sha256(read_model_bytes(SHAPE_MODEL_PATH)).hexdigest()  # inference JSON, not its storage
    if digest != record['models']['shape_only']['sha256']:
        raise ValueError('recorded plate score does not match the shape model; rescore before reporting')
    score = {name: {key: record['shape_only'][name][key] for key in ('correct', 'count')}
             for name in ('literal', 'including_assumed')}
    result = classify_shape(card)
    shapes = [{'label': _GLOSSES[c.label], 'position': list(c.position), 'load': list(c.values),
               'crosswalk_status': 'LITERAL' if c.fault_mode else 'ASSUMED'}
              for c in load_diagnostic_plate(root / PLATE) if c.ordinate == 'load']
    distances = sorted([{'label': s['label'], 'crosswalk_status': s['crosswalk_status'],
                         'distance': loop_distance(card.position, card.load, s['position'], s['load'])}
                        for s in shapes], key=lambda row: row['distance'])
    common = {'status': 'HYPOTHESIS', 'plate_score': score}
    top = [{**common, 'label': MODE_LABELS.get(r['mode'], r['mode'].replace('_', ' ').title()),
            'probability': r['probability']} for r in result.differential]
    near = [{**common, **row, 'rank': index + 1} for index, row in enumerate(distances[:3])]
    return shapes, {**common, 'top_three': top, 'nearest_three': near,
                    'probability_basis': 'Synthetic class separation; not a calibrated field probability.',
                    'field_accuracy': record['field_accuracy']['status'],
                    'model_sha256': digest, 'score_sha256': hashlib.sha256((root / SCORE).read_bytes()).hexdigest()}
