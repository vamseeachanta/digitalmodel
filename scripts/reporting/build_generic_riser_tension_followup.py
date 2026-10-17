"""Build solver-free review evidence from the regenerated generic riser inputs."""
from __future__ import annotations

import argparse
import copy
import json
from pathlib import Path

import yaml

from digitalmodel.drilling_riser import generic_campaign as campaign
from digitalmodel.drilling_riser import generic_tension as tension


def link_records(root: Path, prepared: Path, table: dict) -> list[dict]:
    """Resolve each generated master and solve its scaled hand operating point."""
    rows = []
    for case in table['cases']:
        folder = prepared / 'models' / case['id']
        master = folder / 'master.yml'
        includes = yaml.safe_load(master.read_text(encoding='utf-8'))
        expected = {'includes/01_general.yml', 'includes/03_environment.yml', 'includes/20_generic_objects.yml'}
        if len(includes) != 3 or {row['includefile'] for row in includes} != expected:
            raise ValueError('Unexpected generated master include set')
        if not all((folder / row['includefile']).is_file() for row in includes):
            raise ValueError('Missing generated include')
        objects_path = folder / 'includes/20_generic_objects.yml'
        objects = yaml.safe_load(objects_path.read_text(encoding='utf-8'))
        spec = copy.deepcopy(campaign.read_spec(root))
        spec['generic']['links'] = [dict(name=link['Name'], properties=link)
                                    for link in objects['Links'] if link['Name'] in tension.LINKS]
        # Generated tables already contain the common scale: do not scale twice.
        hand = tension.hand_estimate(spec, table['tension_basis'],
            case['offset_m']['value'], case['current_mps']['value'], factor=1)
        for link, length in zip(spec['generic']['links'], hand['link_lengths_m']):
            curve = link['properties']['SpringLength, SpringTension']
            low, high = curve[0][0], curve[-1][0]
            if not low <= length <= high:
                raise ValueError('Hand operating length outside generated spring table')
            rows.append(dict(case_id=case['id'], link=link['name'], length_m=length,
                table_min_m=low, table_max_m=high, table_fraction=(length-low)/(high-low),
                master_sha256=campaign.digest(master), objects_sha256=campaign.digest(objects_path)))
    if len(rows) != 96:
        raise ValueError('Expected 24 masters and 96 tensioner links')
    return rows


def followup_record(root: Path, prepared: Path) -> dict:
    """Retain hand evidence separately from recorded old-setting native readings."""
    table = yaml.safe_load((prepared / 'case-table.yml').read_text(encoding='utf-8'))
    if table != campaign.case_table(root):
        raise ValueError('Unsupported design basis; regenerate the prepared inputs')
    spec, basis = campaign.read_spec(root), table['tension_basis']
    cases = []
    for case_id, native in (('o0-c0', 9801.7), ('o5-c3', 11684.9)):
        case = next(case for case in table['cases'] if case['id'] == case_id)
        args = (spec, basis, case['offset_m']['value'], case['current_mps']['value'])
        old, raised = tension.hand_estimate(*args, factor=1), tension.hand_estimate(*args)
        cases.append(dict(case_id=case_id, old_hand=old, raised_hand=raised,
            recorded_native_old_vertical_kn=native, hand_minus_native_kn=old['vertical_kn']-native))
    links = link_records(root, prepared, table)
    reference = cases[0]
    margin = reference['raised_hand']['check_b_margin_kn']
    share = tension.surviving_share(basis=basis)
    ratio_correction = share * reference['raised_hand']['vertical_kn'] * (
        1-reference['recorded_native_old_vertical_kn']/reference['old_hand']['vertical_kn'])
    return dict(qualification='hand operating points only; native statics decide; production held',
        source='https://github.com/vamseeachanta/digitalmodel/pull/2684',
        spring_table_factor=basis['spring_table_factor'], cases=cases, links=links,
        largest_table_fraction=max(row['table_fraction'] for row in links),
        check_b_before_correction_kn=margin, vertical_correction_kn=-reference['hand_minus_native_kn'],
        check_b_after_correction_kn=margin-share*reference['hand_minus_native_kn'],
        check_b_after_ratio_calibration_kn=margin-ratio_correction)


def write_followup(root: Path, prepared: Path) -> None:
    """Write machine-readable evidence and the human-facing review note."""
    record = followup_record(root, prepared)
    basis = tension.tension_basis(campaign.read_spec(root))
    rows = []
    for case in record['cases']:
        old, raised = case['old_hand'], case['raised_hand']
        values = [case['case_id'], f"{old['vertical_kn']:,.1f}",
                  f"{case['recorded_native_old_vertical_kn']:,.1f}", f"{case['hand_minus_native_kn']:+.1f}"]
        values += [f'{value:+.3f}' for value in (old['check_a_margin_kn'], old['check_b_margin_kn'],
                    raised['check_a_margin_kn'], raised['check_b_margin_kn'])]
        rows.append('<tr>' + ''.join(f'<td>{value}</td>' for value in values) + '</tr>')
    headers = ['Case', 'Old hand link sum (kN)', 'Recorded native old link sum (kN)',
               'Hand − native (kN)', 'Old hand A (kN)', 'Old hand B (kN)',
               'Raised hand A (kN)', 'Raised hand B (kN)']
    html = f'''<!doctype html>
<html lang="en"><meta charset="utf-8"><title>Generic drilling riser tension follow-up</title>
<style>body{{font:16px system-ui;max-width:80rem;margin:3rem auto;padding:0 1rem;line-height:1.6}}table{{border-collapse:collapse}}td,th{{border:1px solid #aaa;padding:.4rem;text-align:right}}caption{{caption-side:bottom;text-align:left}}</style>
<h1>Generic drilling riser tension follow-up</h1>
<p>Production remains held; nothing released to production. No operating window is established. The review and appended decision govern choices left open by the task. Native statics decide; if native check B fails, the factor will be revisited from native readings.</p>
<p>{tension.BASIS}. The 8,345.7 kN comparator is a legacy screen reading, not graded.</p>
<p>Point of consideration: lower flex joint at z = −1000 m. Flooded steel apparent weight Ws = {basis['ws_kn']:,.3f} kN; Ai = {basis['internal_area_m2']:.9f} m²; T_SRmin = {basis['tsrmin_kn']:,.3f} kN. Fluid head is counted once. Mud density = 2.2 te/m³; Hm = 1020 m; Hw = 1000 m; Bn = 0; gravity = 9.81 m/s² is assumed.</p>
<p>The recorded arrangement supplies N = {basis['n_units']}, n = {basis['n_fail']} and fwt = {basis['fwt']}. Check A requires intact vertical support less W_r ≥ T_SRmin, separate End B sum ≥ (fwt − 1) Ws, and each member minimum ≥ 0. Check B requires (N − n)/N times vertical link sum less W_r ≥ T_SRmin. Every link shall remain inside its table.</p>
<p>The common spring-table factor is {basis['spring_table_factor']:.3f} (+{100*(basis['spring_table_factor']-1):.1f}%), per the appended decision. Hand W_r = {basis['carried_weight_kn']:.3f} kN includes half the wet slip joint; native extraction uses the actual slip-joint End B global reaction. Required vertical sum = {basis['required_vertical_kn']:,.3f} kN. Mechanical/hydraulic efficiency = 1 is assumed; system factor = {basis['system_factor']:.9f}, fleet cosine = {basis['fleet_cosine']:.9f}, fleet-adjusted factor = {basis['fleet_adjusted_system_factor']:.9f}. Fleet angle is applied separately.</p>
<table><thead><tr>{''.join(f'<th>{header}</th>' for header in headers)}</tr></thead><tbody>{''.join(rows)}</tbody><caption>Table 1. Hand margins against the binding criteria beside recorded old-setting native link sums; raised-setting columns are hand estimates. The old setting fails check B.</caption></table>
<p>At o0-c0 the {record['vertical_correction_kn']:.6f} kN correction to hand vertical force reduces check B by {record['check_b_before_correction_kn']-record['check_b_after_correction_kn']:.3f} kN: {record['check_b_before_correction_kn']:+.3f} kN before correction, {record['check_b_after_correction_kn']:+.3f} kN after the fixed correction. Applying the review's proportional calibration {record['cases'][0]['recorded_native_old_vertical_kn']:,.1f}/{record['cases'][0]['old_hand']['vertical_kn']:,.6f} to the raised hand vertical force gives {record['check_b_after_ratio_calibration_kn']:+.3f} kN, consistent with the decision's approximately +33 kN. Neither calculation is a native result. At o5-c3 the old hand sum is 442.2 kN below the recorded native sum; its hand margins are not representative of native acceptance.</p>
<p>All 24 generated masters resolve 72 includes. All 96 scaled hand operating lengths lie inside their own 0–16 m spring tables. The largest fraction (length − minimum)/(maximum − minimum) is {record['largest_table_fraction']:.9f} ({100*record['largest_table_fraction']:.3f}%). Individual lengths, limits and input digests are retained in tension-followup.json. Native static and dynamic containment remains unqualified.</p>
<p>The hand model omits pressure strain, bending, contact and nonlinear current shape. Old-setting evidence: models tree e3b58942173d96ace534b298a86d43f25f554ecb; driver blob a9691ac9261b1d199b255f065b0c30b4f09a3c74. Historical execution comprised 72 current ramp steps plus 24 zero-current equilibrium solves.</p>
<p>End B moment: 1,858 kN.m native against a 1,363–1,440 kN.m hand estimate; explanation pending the checker. Next: native statics and channel readback for all 24 cases, held-edition clause confirmation, shielding and moment disposition by the maintainer.</p>
</html>'''
    campaign.write_text(prepared / 'tension-followup.json', json.dumps(record, indent=2, allow_nan=False))
    campaign.write_text(prepared / 'tension-followup.html', html)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path.cwd())
    parser.add_argument('--prepared', type=Path)
    args = parser.parse_args()
    write_followup(args.root, args.prepared or args.root / campaign.DEST)
