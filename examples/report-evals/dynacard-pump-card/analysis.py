"""Offline surface-to-pump calculation; shape outputs remain hypotheses."""
from pathlib import Path
import hashlib
import json
import argparse
import warnings

import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (
    calculate_fluid_load, calculate_pump_fillage, calculate_theoretical_production,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.data_loader import get_expected_downhole_card
from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import compare_cards
from digitalmodel.marine_ops.artificial_lift.dynacard.physics import DynacardPhysicsSolver
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import EverittJenningsContextSolver
from digitalmodel.marine_ops.artificial_lift.dynacard.power_consumption import calculate_card_area
from card_input import load_card
from model_basis import assumptions, simple_inputs, shape_evidence, PLATE, SCORE, VALIDATION
from sensitivity import sensitivity_table, STEPS
from report_contract import missing_data, CANNOT_DO, PINNED_COMMIT

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
FIXTURE = ROOT / 'tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json'
PUBLIC_SHA256 = '2404c333a9f458ebdf0817dfa98e00962e48639455ae9b612766e14a39dbbd9f'
# Source revisions are independent of the historical calculation-code pin.
# Each immutable revision contains exactly the bytes accepted below.
SOURCE_PINS = {
    FIXTURE.relative_to(ROOT).as_posix(): (
        '9680e7e1ac4a4c225b788e4308322cf87460af70', PUBLIC_SHA256),
    SCORE: ('91e9ad115c6994d4d2546b515ba0751efa782277',
            'cb11b1fd0399285c3a795dacc2c1c2d0b2cdb38e3103f7bc2859ca79a480743d'),
}


def repository_source(path, relative):
    """Reject changed inputs rather than cite bytes from a different revision."""
    revision, expected = SOURCE_PINS[relative]
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    if digest != expected:
        raise ValueError(f'{relative}: source bytes do not match the cited revision')
    return {'revision': revision, 'sha256': digest,
            'url': f'https://api.github.com/repositories/590299785/contents/{relative}?ref={revision}'}


def key_results(ctx, card):
    """Use package definitions; displacement includes fillage at continuous runtime."""
    fill = calculate_pump_fillage(card)
    fluid = calculate_fluid_load(card)
    production = calculate_theoretical_production(ctx, fill)
    stroke = float(np.ptp(ctx.surface_card.position))
    return {
        'surface_stroke_in': stroke,
        'peak_polished_rod_load_lbf': float(max(ctx.surface_card.load)),
        'minimum_polished_rod_load_lbf': float(min(ctx.surface_card.load)),
        'pump_stroke_in': fill.gross_stroke,
        'stroke_ratio': fill.gross_stroke / stroke,
        'maximum_pump_load_lbf': float(max(card.load)),
        'minimum_pump_load_lbf': float(min(card.load)),
        'fluid_load_lbf': fluid.fluid_load,
        'fillage_pct': fill.fillage,
        'effective_travel_in': fill.net_stroke,
        'pump_displacement_bbl_day': production.net_displacement,
        'gross_displacement_bbl_day': production.gross_displacement,
        'pump_card_area_ft_lbf': calculate_card_area(card),
    }


def unique_phases(card):
    """Remove an exact closing pair; nearby measurements remain distinct phases."""
    if card.position[0] == card.position[-1] and card.load[0] == card.load[-1]:
        return card.model_copy(update={'position': card.position[:-1], 'load': card.load[:-1]})
    return card


def solve_default(ctx):
    """Return the default pump card; see solve_default_with_flags."""
    return solve_default_with_flags(ctx)[0]


def solve_default_with_flags(ctx):
    """Give the periodic finite-difference stencil its repeated endpoint.

    Return unique phases so neither metrics nor comparison weight phase zero
    twice. The frequency-domain solver consumes unique phases directly. Also
    return the solver's deviation disclosure: it runs with along-hole gravity
    and contact friction off, so any inclination is flagged, not modelled.
    """
    surface = unique_phases(ctx.surface_card)
    closed = surface.model_copy(update={
        'position': [*surface.position, surface.position[0]],
        'load': [*surface.load, surface.load[0]],
    })
    closed_ctx = ctx.model_copy(update={'surface_card': closed})
    result = EverittJenningsContextSolver(closed_ctx).solve()
    return unique_phases(result.downhole_card), {
        'max_inclination_deg': result.max_inclination_deg,
        'solver_flags': list(result.solver_flags)}


def survey_record(ctx):
    """The survey as supplied to the default solver, in feet and degrees."""
    s = ctx.survey
    return None if s is None else {'measured_depth_ft': list(s.measured_depth),
                                   'inclination_deg': list(s.inclination),
                                   'azimuth_deg': list(s.azimuth)}


def resample_phases(card, count):
    """Interpolate a stored cycle periodically onto the comparison phase grid."""
    card = unique_phases(card)
    if len(card.position) == count:
        return card
    phases = np.arange(count) / count
    source_phases = np.arange(len(card.position)) / len(card.position)
    return card.model_copy(update={
        name: np.interp(phases, source_phases, getattr(card, name), period=1).tolist()
        for name in ('position', 'load')
    })


def _cross_check(card, alternative, reference):
    gibbs = alternative.downhole_card
    cross = {'gibbs_default_load_nrmse_pct': compare_cards(gibbs, card).load_nrmse_pct,
             'default_reference_load_nrmse_pct': None,
             'gibbs_reference_load_nrmse_pct': None}
    if reference is not None:
        cross['default_reference_load_nrmse_pct'] = compare_cards(card, reference).load_nrmse_pct
        cross['gibbs_reference_load_nrmse_pct'] = compare_cards(gibbs, reference).load_nrmse_pct
    return cross


def sources(root, card_path, public):
    fixture = (repository_source(Path(card_path), FIXTURE.relative_to(ROOT).as_posix()) if public
               else {'url': '', 'sha256': hashlib.sha256(Path(card_path).read_bytes()).hexdigest()})
    score = repository_source(root / SCORE, SCORE)
    rows = [('Public example surface card, rod string and stored pump card' if public else 'Reader surface card and equipment',
             'Repository field fixture' if public else 'Reader supplied',
             'MIT' if public else 'Reader supplied; rights not established',
             fixture),
            ('Public reference shapes', 'Public diagnostic plate', 'CC0 1.0',
             {'url': 'https://doi.org/10.17632/hnfwm9mjs6.1',
              'sha256': hashlib.sha256((root / PLATE).read_bytes()).hexdigest()}),
            ('Recorded shape-only plate score', 'Repository benchmark', 'MIT',
             score)]
    return [{'what': what, 'source': source, 'licence': licence,
             'date': '2026-10-11' if what == 'Public reference shapes' else '2026-11-12', **identity}
            for what, source, licence, identity in rows]


def run(card_path=FIXTURE, *, include_sensitivity=True, resource_root=ROOT):
    """Analyse one card; resource_root supplies version-selected public plate evidence."""
    ctx, raw = load_card(card_path)
    ctx = ctx.model_copy(update={'surface_card': unique_phases(ctx.surface_card)})
    card, flags = solve_default_with_flags(ctx)
    deviation = {'survey': survey_record(ctx), **flags}
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', message='.*comparison only: not field validated.*')
        alternative = DynacardPhysicsSolver(ctx).solve_wave_equation(harmonic_count=15)
    gibbs = alternative.downhole_card
    public = hashlib.sha256(Path(card_path).read_bytes()).hexdigest() == PUBLIC_SHA256
    reference = resample_phases(get_expected_downhole_card(raw), len(card.position)) if public else None
    key = key_results(ctx, card)
    table = sensitivity_table(ctx, key, lambda changed: key_results(
        changed, solve_default(changed))) if include_sensitivity else []
    shapes, hypotheses = shape_evidence(card, Path(resource_root))
    fluid = calculate_fluid_load(card)
    # The historical pin identifies calculation code. Candidate eval records
    # hash the corrected code; refresh the pin after that revision is published.
    commit = PINNED_COMMIT
    cards = {'surface': ctx.surface_card, 'default': card, 'gibbs': gibbs, 'reference': reference}
    return {
        'condition_name': 'public example well' if public else 'reader card', 'commit': commit,
        'key_results': key, 'inputs': simple_inputs(ctx), 'assumptions': assumptions(ctx, raw, deviation),
        'deviation': deviation,
        'geometry_basis': {'rod_length_ft': ctx.rod_length, 'pump_depth_ft': ctx.pump.depth,
                           'unrepresented_length_ft': ctx.pump.depth - ctx.rod_length},
        'solver_cross_check': _cross_check(card, alternative, reference),
        'cards': {k: {'position_in': v.position, 'load_lbf': v.load} if v else None for k, v in cards.items()},
        'fluid_load_levels': {'upstroke_lbf': fluid.upstroke_load, 'downstroke_lbf': fluid.downstroke_load},
        'plate_shapes': shapes, 'shape_hypothesis': hypotheses, 'sensitivity': table,
        'sensitivity_basis': 'One input family at a time; all sections scaled together; diameter changes preserve effective density; factors are central output-percent differences divided by twice input-percent step.',
        'settings': {'harmonic_count': 15, 'runtime_hours': 24, 'volumetric_efficiency_pct': 100,
                     'perturbation_steps_pct': list(STEPS), 'default_spatial_nodes': 200},
        'validation': {'statement': VALIDATION, 'published_example': {'status': 'NOT_EVALUATED',
                       'missing_evidence': 'Openly readable published worked example with tabulated card values'}},
        'sources': sources(Path(resource_root), card_path, public),
        'missing_data': missing_data(table), 'cannot_do': CANNOT_DO,
        'checks': [{'status': 'PASS', 'criterion': 'Reported stroke ratio equals pump stroke divided by surface stroke.',
                    'scope': 'Arithmetic only; pump stroke depends on ASSUMED model inputs.'}],
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('card', type=Path, nargs='?', default=FIXTURE)
    parser.add_argument('--out', type=Path, required=True)
    parser.add_argument('--figure', type=Path)
    args = parser.parse_args()
    try:
        result = run(args.card)
    except (ValueError, ArithmeticError) as exc:
        parser.exit(2, f'Card input or calculation error: {exc}\n')
    args.out.parent.mkdir(parents=True, exist_ok=True)
    args.out.write_text(json.dumps(result, indent=2, allow_nan=False) + '\n', encoding='utf-8')
    if args.figure:
        from plotting import render_figure
        render_figure(result, args.figure)


if __name__ == '__main__':
    main()
