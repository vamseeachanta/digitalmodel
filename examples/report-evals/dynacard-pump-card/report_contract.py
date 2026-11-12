"""Display precision, reader data requests and fixed report scope."""
import json
from pathlib import Path

TITLE = ('Rod pump dynamometer card analysis: pump card, fluid load and fillage '
         'from a surface card by the wave equation')
REPO = 'https://github.com/vamseeachanta/digitalmodel'
FOLDER = 'examples/report-evals/dynacard-pump-card'
METRICS = {
    'surface_stroke_in': ('Surface stroke', 'in', 2),
    'peak_polished_rod_load_lbf': ('Peak polished-rod load', 'lbf', 0),
    'minimum_polished_rod_load_lbf': ('Minimum polished-rod load', 'lbf', 0),
    'pump_stroke_in': ('Pump stroke, gross plunger travel', 'in', 2),
    'stroke_ratio': ('Pump/surface stroke ratio', 'ratio', 3),
    'maximum_pump_load_lbf': ('Maximum pump load', 'lbf', 0),
    'minimum_pump_load_lbf': ('Minimum pump load', 'lbf', 0),
    'fluid_load_lbf': ('Fluid load, mean phase difference', 'lbf', 0),
    'fillage_pct': ('Pump fillage', '%', 2),
    'effective_travel_in': ('Effective plunger travel', 'in', 2),
    'pump_displacement_bbl_day': ('Fillage-adjusted displacement', 'bbl/day', 2),
    'gross_displacement_bbl_day': ('Gross displacement', 'bbl/day', 2),
    'pump_card_area_ft_lbf': ('Pump-card work per stroke', 'ft-lbf', 1),
}
CANNOT_DO = [
    'Name a pump fault with a stated field accuracy: every shape label is a HYPOTHESIS; field accuracy is NOT_EVALUATED because independent labelled field cards are unavailable.',
    'Analyse a deviated well as vertical without saying so; the frequency-domain cross-check is vertical.',
    'Correct a mis-calibrated load cell without independent calibration evidence.',
    'Give production from a card alone: operating runtime and plunger/valve slip are needed.',
    'Establish a published-example check without an openly readable worked example with tabulated card values; it is NOT_EVALUATED.',
]


# pin.json names the commit that holds the code and inputs behind the published
# values, and the pull request whose head keeps that commit fetchable before it
# reaches main. It is data, not code: re-pinning changes no Python file, so a
# pinned rebuild can verify every source file against the pinned commit.
_PIN = json.loads((Path(__file__).resolve().parent / 'pin.json').read_text(encoding='utf-8'))
PINNED_COMMIT = _PIN['commit']
PULL_REQUEST = _PIN['pull_request']

FACTOR_OUTPUTS = (('pump_stroke_in', 'pump stroke'), ('fluid_load_lbf', 'fluid load'),
                  ('fillage_pct', 'fillage'), ('maximum_pump_load_lbf', 'maximum pump load'),
                  ('pump_displacement_bbl_day', 'displacement'))


def missing_data(sensitivity):
    """Reader data requests: what, why, the effect of its absence, how to supply it."""
    rows = {r['input']: r for r in sensitivity}

    def impact(key):
        if key not in rows:
            return 'The sensitivity table gives the affected results.'
        row = rows[key]
        def text(value):
            if value != 0 and abs(value) < 0.005:
                return '< 0.01 in magnitude'
            return f'{value:.2f}'.replace('-', '−')
        parts = [f"{label} {text(row['factor_1pct'][output])}"
                 + (' (stepped)' if output in row.get('stepped_outputs', ()) else '')
                 for output, label in FACTOR_OUTPUTS]
        return ('On the public example well a 1 % error in it changes: ' + ', '.join(parts)
                + ' (% per %).')

    entries = [
        ('surface_card', 'The surface card: position and load samples over one full stroke, with units',
         'The wave equation is solved from the measured polished-rod position and load.',
         'Without it there is no calculation; stop. The samples are ASSUMED evenly spaced in time.',
         'Export the card from the pump-off controller or dynamometer as position (in) and load (lbf) columns, in acquisition order.'),
        ('rod_string', 'The rod string as run: diameter, length and material of each taper, surface first',
         'Rod stretch, wave travel time and buoyant rod weight come from the taper geometry.',
         'Without diameter or length there is no calculation; stop. A missing material is ASSUMED steel. '
         + 'Rod length: ' + impact('rod_length_depth') + ' Rod diameter: ' + impact('rod_diameter'),
         'Take it from the rod tally or the last workover ticket; a remembered string is weaker evidence than the tally.'),
        ('pump', 'Pump setting depth and plunger diameter',
         'The pump depth closes the rod string; the plunger diameter converts plunger travel to volume.',
         'Without either there is no calculation; stop. Plunger diameter: ' + impact('pump_diameter'),
         'Take both from the pump or completion record.'),
        ('speed', 'Pumping speed in strokes per minute',
         'Speed sets the time of one stroke and so the dynamic part of the rod load.',
         'Without it there is no calculation; stop. ' + impact('pumping_speed'),
         'Read it from the controller, or time ten strokes.'),
        ('calibration', 'Load-cell calibration date and method',
         'Every pump load is the surface load less the buoyant rod weight, so a load-cell error passes straight into it.',
         'If not supplied, the surface loads are ASSUMED correct as recorded. ' + impact('surface_load_scale')
         + ' The effect of a zero offset is NOT_EVALUATED.',
         'Give the date of the last calibration and whether the cell was zeroed unloaded.'),
        ('density', 'Fluid density, or water cut with oil and water densities',
         'Fluid density sets the buoyancy of the rod string.',
         'If not supplied, fluid density is ASSUMED 62.4 lb/ft3 (fresh water). ' + impact('fluid_density'),
         'Give the produced-fluid density or the water cut and the two phase densities.'),
        ('anchoring', 'Whether the tubing is anchored',
         'Unanchored tubing stretches with the fluid load and shortens the plunger travel relative to the barrel.',
         'If not supplied, anchored tubing is ASSUMED; pump stroke, fillage and displacement rest on it. Unanchored tubing is not modelled and its effect is NOT_EVALUATED.',
         'State anchored or unanchored, with the anchor depth.'),
        ('survey', 'Deviation survey',
         'In a deviated well, rod weight and friction act along the well path.',
         'If not supplied, a vertical well is ASSUMED. The frequency-domain cross-check is always vertical. The effect of deviation is NOT_EVALUATED without a survey.',
         'Give measured depth, inclination and azimuth from surface to the pump.'),
        ('pressure', 'Tubing pressure',
         'It is used for pump intake pressure, which this report does not give.',
         'If not supplied, 100 psi is ASSUMED; none of the reported card results uses it.',
         'Read it at the wellhead.'),
    ]
    return [{'id': key, 'what': what, 'why': why, 'effect': effect, 'how_to_supply': how}
            for key, what, why, effect, how in entries]
