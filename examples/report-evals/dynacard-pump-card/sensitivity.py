"""One-input-at-a-time perturbations of the default surface-to-pump solver."""
import numpy as np

INPUTS = ('rod_length_depth', 'rod_diameter', 'rod_modulus', 'pumping_speed',
          'surface_load_scale', 'surface_position_scale', 'fluid_density', 'pump_diameter')
OUTPUTS = ('pump_stroke_in', 'fluid_load_lbf', 'fillage_pct', 'maximum_pump_load_lbf',
           'pump_displacement_bbl_day')
# A response is reported as stepped when the +/-1 % and +/-5 % central factors
# differ by more than this amount (% output per % input); a chosen display rule.
STEPPED_FACTOR_DIFFERENCE = 0.10
STEPS = (-5, -1, 1, 5)


def perturb(ctx, name, change_pct):
    """Keep material density fixed during diameter perturbations, including couplings."""
    changed = ctx.model_copy(deep=True)
    factor = 1 + change_pct / 100
    if name == 'rod_length_depth':
        for section in changed.rod_string:
            section.length *= factor
        changed.pump.depth *= factor
        if changed.survey is not None:
            changed.survey.measured_depth = [v * factor for v in changed.survey.measured_depth]
    elif name == 'rod_diameter':
        for section in changed.rod_string:
            section.diameter *= factor
            section.weight_per_foot *= factor ** 2
    elif name == 'rod_modulus':
        for section in changed.rod_string:
            section.modulus_of_elasticity *= factor
    elif name == 'pumping_speed':
        changed.spm *= factor
    elif name == 'surface_load_scale':
        changed.surface_card.load = [v * factor for v in changed.surface_card.load]
    elif name == 'surface_position_scale':
        changed.surface_card.position = [v * factor for v in changed.surface_card.position]
    elif name == 'fluid_density':
        changed.fluid_density *= factor
    elif name == 'pump_diameter':
        changed.pump.diameter *= factor
    else:
        raise ValueError('unknown sensitivity input')
    return changed


def sensitivity_table(ctx, base, solve_metrics):
    rows = []
    for name in INPUTS:
        steps = []
        for change in STEPS:
            outputs = solve_metrics(perturb(ctx, name, change))
            relative = {}
            for key in OUTPUTS:
                if base[key] == 0:
                    raise ValueError(f'{key}: relative sensitivity is undefined at a zero baseline')
                value = 100 * (outputs[key] / base[key] - 1)
                relative[key] = 0.0 if abs(value) < 1e-10 else float(value)
            steps.append({'input_change_pct': change, 'output_change_pct': relative})
        factors = {str(size): {key: (steps[hi]['output_change_pct'][key]
                                    - steps[lo]['output_change_pct'][key]) / (2 * size)
                              for key in OUTPUTS}
                   for size, lo, hi in ((1, 1, 2), (5, 0, 3))}
        stepped = sorted(key for key in OUTPUTS
                         if abs(factors['1'][key] - factors['5'][key]) > STEPPED_FACTOR_DIFFERENCE)
        rows.append({'input': name, 'steps': steps, 'factor_1pct': factors['1'],
                     'factor_5pct': factors['5'], 'stepped_outputs': stepped,
                     'depends_on': 'ASSUMED model inputs'})
    return rows
