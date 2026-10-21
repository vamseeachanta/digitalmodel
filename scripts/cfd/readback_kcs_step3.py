# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline Case 2.10 readback and admission; no solver commands."""
from __future__ import annotations

import argparse
import json
from pathlib import Path

from digitalmodel.solvers.openfoam.validation import kcs_step3 as qa
from digitalmodel.solvers.openfoam.validation.kcs_step1 import require_run_admission


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('command', choices=('compare', 'admit', 'triplet'))
    parser.add_argument('--reference', type=Path, required=True)
    parser.add_argument('--criteria', type=Path, required=True)
    parser.add_argument('--condition', choices=tuple(f'C{i}' for i in range(1, 6)), required=True)
    parser.add_argument('--history', type=Path, action='append', default=[])
    parser.add_argument('--metadata', type=Path, action='append', default=[])
    parser.add_argument('--next-mesh', choices=('coarse', 'medium', 'fine'))
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args(argv)
    try:
        expected = 3 if args.command == 'triplet' else 1
        if args.command == 'admit' and args.next_mesh == 'coarse':
            expected = 0
        if len(args.history) != expected or len(args.metadata) != expected:
            raise ValueError(f'{expected} history/metadata pairs required')
        if args.command == 'admit':
            report = require_run_admission(args.next_mesh, args.reference, args.criteria,
                                           args.history[0] if expected else None,
                                           args.metadata[0] if expected else None,
                                           case='2.10', condition_id=args.condition)
            passed = report['admitted']
        else:
            reference, policy = qa.load_reference(args.reference), qa.load_policy(args.criteria)
            row = next(r for r in reference['reference_rows'] if r['id'] == args.condition)
            metas = [json.loads(p.read_text(encoding='utf-8')) for p in args.metadata]
            if args.command == 'triplet':
                report = qa.compare_triplet(args.history, metas, row, policy)
                passed = report['amplitude_gci_passed']
            else:
                report = qa.compare_run(args.history[0], metas[0], row, policy)
                passed = report['comparison_passed']
    except (OSError, ValueError, KeyError, TypeError) as exc:
        reason = str(exc) if isinstance(exc, ValueError) else 'required evidence is missing or malformed'
        report, passed = dict(admitted=False, qualified=False, reason=reason), False
    args.output.write_text(json.dumps(report, indent=2, allow_nan=False) + '\n', encoding='utf-8')
    print('PASS' if passed else 'FAIL')
    return 0 if passed else 1


if __name__ == '__main__':
    raise SystemExit(main())
