# ABOUTME: Bounded synthetic characterization of existing FFS L1/L2 implementation.
# ABOUTME: Returns no allowable wall; source qualification and width physics are unresolved.
"""Run with python -m digitalmodel.asset_integrity.uniform_loss_diagnostic --output PATH.

These are implementation diagnostics, never a Part 5 acceptance envelope.
Production assessment physics is delegated to the canonical coordinator.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import math
import platform
import subprocess
from datetime import datetime, timezone
from pathlib import Path

import numpy as np
import pandas as pd

from .assessment.ffs_coordinator import FFSComponent, assess_component
from .corroded_pipe import SMYS_PSI

BASE_REVISION = '2a52374d401e2f445d4baf435322f2cf18346c98'
GEOMETRIES = ((12.75, 0.375), (16.0, 0.625), (20.0, 0.5), (30.0, 0.375))
LENGTHS = (0.5, 1.0, 2.0, 4.0, 8.0, 16.0, 32.0)
WIDTH_FRACTIONS = (0.02, 0.10, 0.25, 0.50)
REMAINING_FRACTIONS = (0.20, 0.40, 0.60, 0.70, 0.85, 0.95, 1.0)


def diagnose_case(*, od_in: float, nominal_wall_in: float,
                  axial_length_in: float, width_fraction: float,
                  remaining_fraction: float) -> dict:
    """Characterize one synthetic external rectangular LTA in inches/psi.

    Cell-center labels plus 16 equal axial cells preserve requested edge length
    for deep loss. The shallow-loss segmentation discrepancy remains visible.
    Width is physically encoded in coordinates but not assessed by the engine.
    """
    values = (od_in, nominal_wall_in, axial_length_in, width_fraction,
              remaining_fraction)
    if not all(math.isfinite(value) and value > 0 for value in values):
        raise ValueError('Inputs must be finite and positive')
    if 2 * nominal_wall_in >= od_in or width_fraction > 1 or remaining_fraction > 1:
        raise ValueError('Invalid pipe geometry or fraction')
    smys = SMYS_PSI['X52']
    pressure = 0.70 * 2 * smys * 0.72 * nominal_wall_in / od_in
    wall = remaining_fraction * nominal_wall_in
    arc = width_fraction * math.pi * (od_in - nominal_wall_in)
    dx, dy = axial_length_in / 16, arc / 8
    grid = pd.DataFrame(np.full((16, 8), wall),
                        index=(np.arange(16) + 0.5) * dx,
                        columns=(np.arange(8) + 0.5) * dy)
    component = FFSComponent(
        component_id='synthetic-uniform-loss-diagnostic', design_code='B31.8',
        nominal_od_in=od_in, nominal_wt_in=nominal_wall_in,
        design_pressure_psi=pressure, smys_psi=smys,
        fca_in=0.0, corrosion_rate_in_per_yr=0.0, rsf_a=0.9)
    result = assess_component(component, grid, force_type='LML')
    raw_l2 = dict(result.level2)
    raw_l2['applicability'] = raw_l2['applicability'].to_dict()
    limitations = ['WIDTH_NOT_ASSESSED', 'PART5_LEVEL1_INCOMPLETE',
                   'REFERENCE_WALL_IS_CODE_MINIMUM', 'FOLIAS_SOURCE_UNVERIFIED',
                   'FULL_PART5_APPLICABILITY_UNVERIFIED',
                   'CLOSED_END_AXIAL_STRESS_NOT_ASSESSED', 'ROUTING_FORCED',
                   'NO_SOUND_REGION_IN_GRID', 'MIN_WALL_RT_LMSD_SPACING_UNVERIFIED']
    if wall < 0.1:
        limitations.append('CANDIDATE_MIN_WALL_FLOOR_REQUIRES_SOURCE_CHECK')
    if not math.isclose(raw_l2['flaw_length_in'], axial_length_in, rel_tol=1e-10):
        limitations.append('ALGORITHM_LENGTH_DIFFERS')
    return {
        'qualification': 'UNQUALIFIED_BASELINE',
        'case_kind': 'intact-control' if remaining_fraction == 1 else 'synthetic-LTA',
        'assessment_status': ('UNQUALIFIED' if raw_l2['applicability']['ok']
                              else 'INAPPLICABLE'),
        'allowable_remaining_wall_in': None,
        'allowable_wall_reason': 'Edition-matched Part 5 validation incomplete',
        'inputs': dict(od_in=od_in, nominal_wall_in=nominal_wall_in,
                       sound_wall_in=nominal_wall_in, fca_in=0.0,
                       remaining_wall_in=wall, axial_length_in=axial_length_in,
                       width_fraction=width_fraction, width_arc_in=arc,
                       width_angle_deg=360 * width_fraction,
                       pressure_psi=pressure, smys_psi=smys,
                       design_factor=0.72, pressure_fraction=0.70,
                       temperature_c=20.0, end_condition='closed',
                       superimposed_axial_force_lbf=0.0,
                       bending_moment_lbf_in=0.0, torque_lbf_in=0.0,
                       loss_surface='external', grid_shape=[16, 8],
                       axial_cell_width_in=dx, circumferential_cell_width_in=dy,
                       sound_wall_normalized_length=axial_length_in / math.sqrt(
                           (od_in - 2 * nominal_wall_in) * nominal_wall_in),
                       coordinate_convention='cell centers; edge extent = N * spacing'),
        'raw_level1': result.level1, 'raw_level2': raw_l2,
        'limitations': limitations,
    }


def run_study() -> dict:
    """Deterministic bounded sweep. No root-finding or acceptance interpolation."""
    root = Path(__file__).parents[1]
    files = sorted(root.rglob('*.py'))
    source_hashes = {str(path.relative_to(root)).replace('\\', '/'):
                     hashlib.sha256(path.read_bytes()).hexdigest() for path in files}
    cases = [diagnose_case(od_in=diameter, nominal_wall_in=wall,
                          axial_length_in=length, width_fraction=width,
                          remaining_fraction=remaining)
             for diameter, wall in GEOMETRIES for length in LENGTHS
             for width in WIDTH_FRACTIONS for remaining in REMAINING_FRACTIONS]
    return {'meta': {
        'issue': 'https://github.com/vamseeachanta/digitalmodel/issues/2287',
        'base_revision': BASE_REVISION,
        'target_standard': 'API 579-1/ASME FFS-1 2021 Part 5',
        'design_thickness_reference': 'B31.8-2022; F=.72 E=T=1',
        'standard_verification': 'exact 2021 equations/access not established',
        'qualified_cases': 0, 'source_sha256': source_hashes,
        'source_hash_scope': 'all digitalmodel Python source files; raw checkout bytes',
        'checkout_portability': 'hashes pin exact bytes; CRLF/LF differences change digests',
        'config_basis': 'fixed constants and synthetic inputs in hashed runner; no external config loaded',
        'units': {'length': 'in', 'pressure': 'psi', 'stress': 'psi',
                  'temperature': 'degC', 'fractions': '1'},
        'purpose': 'implementation diagnostic only; no allowable-wall results',
        'data_origin': 'synthetic; geometry-only ecosystem precedents',
        'assumptions': ['ambient pressure-only closed ends', 'X52 assumed common grade',
                        'FCA=0; mill tolerance=0; historical corrosion allowance=0',
                        'nominal and sound-region wall equal; no measured data'],
    }, 'cases': cases}


def run_preliminary_pressure_study() -> dict:
    """84 named pressure-only thresholds, separate from API 579 diagnostics."""
    from .ffs_acceptance_curves import pipe_pressure_wall_screen
    cases = [pipe_pressure_wall_screen(
        diameter, wall, 'X52', method, axial_length_in=length,
        pressure_psi=.70*2*SMYS_PSI['X52']*.72*wall/diameter, safety_factor=1/.72)
        for diameter, wall in GEOMETRIES for method in
        ('b31g', 'modified_b31g', 'rstreng') for length in LENGTHS]
    root = Path(__file__).parents[1]
    hashes = {str(path.relative_to(root)).replace('\\', '/'):
              hashlib.sha256(path.read_bytes()).hexdigest()
              for path in sorted(root.rglob('*.py'))}
    return {'meta': {
        'issue': 'https://github.com/vamseeachanta/digitalmodel/issues/2287',
        'purpose': 'preliminary hoop-controlled pressure-demand thresholds only',
        'evidence_status': 'PRELIMINARY_ARITHMETIC_ONLY', 'qualified_cases': 0,
        'source_sha256': hashes,
        'source_hash_scope': 'all digitalmodel Python source files; raw checkout bytes',
        'config_basis': 'fixed hashed-runner constants; no external config',
        'units': {'length': 'in', 'pressure': 'psi', 'stress': 'psi'},
        'data_origin': 'synthetic; four geometry-only ecosystem precedents',
        'assumptions': ['X52 common assumed grade', 'ambient 20 degC',
                        'external blunt longitudinal loss; no width classification',
                        'nominal=sound wall; FCA=0; mill tolerance=0',
                        'pressure demand=.70*intact B31.8 F=.72 E=T=1 reference',
                        'explicit pressure safety factor=1/.72, not historical 1.39',
                        'closed ends; separate axial tension capacity unassessed',
                        'no superimposed axial force, bending, torque or external pressure',
                        'implemented depth bound=.80; full code applicability not qualified'],
        'api579_allowable_remaining_wall_in': None, 'asset_acceptance': None,
    }, 'cases': cases}


def plot_preliminary_pressure_study(result: dict, output: Path) -> None:
    """Static scientific plot, with censoring gaps and explicit limited scope."""
    if any(row['status'] not in {'PRELIMINARY_THRESHOLD', 'LOWER_BOUND_CENSORED'}
           for row in result['cases']):
        raise ValueError('Plot supports threshold/censored cells only; inspect other dispositions in JSON')
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    fig, axes = plt.subplots(2, 2, figsize=(12, 8), sharex=True)
    for ax, (diameter, wall) in zip(axes.flat, GEOMETRIES):
        for method in ('b31g', 'modified_b31g', 'rstreng'):
            rows = [row for row in result['cases']
                    if row['inputs']['od_in'] == diameter and row['method'] == method]
            line, = ax.plot([r['inputs']['axial_length_in'] for r in rows],
                    [r['preliminary_remaining_wall_in']
                     if r['status'] == 'PRELIMINARY_THRESHOLD' else float('nan')
                     for r in rows], marker='o', label=method)
            censored = [r for r in rows if r['status'] == 'LOWER_BOUND_CENSORED']
            ax.scatter([r['inputs']['axial_length_in'] for r in censored],
                       [r['passing_bracket']['remaining_wall_in'] for r in censored],
                       marker='v', facecolors='none', edgecolors=line.get_color())
        demand = rows[0]['inputs']['pressure_psi']
        ax.set_title(f'OD {diameter:g} in x wall {wall:g} in; demand {demand:.1f} psi')
        ax.set_xscale('log', base=2)
        ax.set_xticks(LENGTHS, [f'{length:g}' for length in LENGTHS])
        ax.set_xlabel('Axial blunt-loss length (in)')
        ax.set_ylabel('Model remaining-wall threshold (in)')
        ax.grid(alpha=.3)
        ax.legend(fontsize=8)
    fig.suptitle('Preliminary pressure-demand thresholds: B31G / Modified B31G / RSTRENG')
    fig.text(.5, .015, 'Synthetic X52; demand = 70% of F=0.72 reference pressure; '
             'safety factor 1/0.72.\nHoop containment only; not API 579 or asset acceptance. '
             'Open triangles: lower study bound meets demand '
             '(threshold unresolved). Width and axial capacity unassessed.\n'
             'Lines guide between seven sampled lengths; no qualified interpolation.', ha='center', fontsize=9)
    fig.tight_layout(rect=(0, .065, 1, .95))
    output.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(output, dpi=160)
    plt.close(fig)


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, required=True)
    parser.add_argument('--preliminary-pressure', action='store_true')
    parser.add_argument('--plot', type=Path)
    args = parser.parse_args()
    if args.plot and not args.preliminary_pressure:
        parser.error('--plot requires --preliminary-pressure')
    repo = Path(__file__).resolve().parents[3]
    try:
        executing_revision = subprocess.run(['git', 'rev-parse', 'HEAD'], cwd=repo,
                                            check=True, text=True, capture_output=True).stdout.strip()
        dirty = bool(subprocess.run(['git', 'status', '--porcelain'], cwd=repo,
                                   check=True, text=True, capture_output=True).stdout.strip())
        revision_status = 'GIT_CHECKOUT'
    except (subprocess.CalledProcessError, FileNotFoundError):
        executing_revision, dirty, revision_status = None, None, 'GIT_PROVENANCE_UNAVAILABLE'
    result = run_preliminary_pressure_study() if args.preliminary_pressure else run_study()
    result['meta']['runtime'] = dict(python=platform.python_version(),
                                   numpy=np.__version__, pandas=pd.__version__,
                                   execution_platform=platform.system(), executing_revision=executing_revision,
                                   working_tree_dirty=dirty, revision_status=revision_status,
                                   run_utc=datetime.now(timezone.utc).isoformat())
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2, allow_nan=False) + '\n',
                           encoding='utf-8')
    if args.plot:
        plot_preliminary_pressure_study(result, args.plot)
    print(f'{len(result["cases"])} cases; 0 qualified allowable-wall cases')


if __name__ == '__main__':
    main()
