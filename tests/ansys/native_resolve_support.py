"""Explicitly opted-in native re-solves; offline tests replace the runner boundary."""
from datetime import datetime, timezone
import hashlib
import json
import math
from pathlib import Path
import shutil

from digitalmodel.ansys.results_extractor import ResultsExtractor
from digitalmodel.ansys.runner import ANSYSRunStatus, run_ansys


def _sha256(path):
    with Path(path).open('rb') as stream:
        return hashlib.file_digest(stream, 'sha256').hexdigest()


def _digest(path):
    text = Path(path).read_text(encoding='utf-8')
    keys = text.strip().split(',')[::2]
    if len(keys) != len(set(keys)):
        raise ValueError('Duplicate result labels')
    values = ResultsExtractor().parse_result_digest(text)
    if not values or not all(math.isfinite(value) for value in values.values()):
        raise ValueError('Missing or nonfinite result values')
    return values


def _candidate(case):
    provenance = json.loads((case / 'golden/PROVENANCE.json').read_text(encoding='utf-8'))
    profile = provenance['solver']
    if (profile['cores'] != 1 or profile['parallel'] != 'smp'
            or profile['argv'].split() != ['-b', '-np', '1', '-smp']
            or profile['equation_solver'] != 'default sparse direct'):
        raise ValueError('Unsupported recorded run profile')
    name = provenance['input']['deck']
    if Path(name).name != name or '/' in name or '\\' in name:
        raise ValueError('Deck must be one filename within its case')
    deck = case / name
    if _sha256(deck) != provenance['input']['sha256']:
        raise ValueError('Deck SHA-256 differs from recorded golden input')
    digests = list((case / 'golden').glob('*_result.csv'))
    if len(digests) != 1:
        raise ValueError('Exactly one golden digest is required')
    return provenance, deck, digests[0], _digest(digests[0])


def _check_result(result, output, golden_path, expected, profile):
    if result.status != ANSYSRunStatus.COMPLETED or result.return_code != 0:
        raise ValueError(f'Native solve did not complete: {result.error_message}')
    log = result.log_file
    digest = output / golden_path.name
    if log is None or log not in result.result_files or digest not in result.result_files:
        raise ValueError('Fresh log and expected digest must be attributed to this run')
    with log.open('r', encoding='utf-8', errors='replace') as stream:
        header = stream.read(131072)
    if not all(str(profile[key]) in header for key in ('release', 'build', 'update', 'platform')):
        raise ValueError('Native log does not establish the recorded runtime profile')
    observed = _digest(digest)
    if observed.keys() != expected.keys():
        raise ValueError('Native and golden digest fields differ')
    for key, value in expected.items():
        # Plan-fixed relative stability criterion; zero requires exact agreement.
        if not math.isclose(observed[key], value, rel_tol=1e-3, abs_tol=0.0):
            raise ValueError(f'Native {key} differs from golden beyond relative 1e-3')
    return observed


def resolve_golden(case, output, executable):
    """Retain exact input and fresh native evidence without claiming qualification."""
    case, output = Path(case).resolve(), Path(output).resolve()
    provenance, deck, golden_path, expected = _candidate(case)
    if output.exists():
        raise ValueError('Re-solve requires a new, unused output directory')
    output.mkdir(parents=True, exist_ok=False)
    copied = output / deck.name
    shutil.copyfile(deck, copied)
    if _sha256(copied) != provenance['input']['sha256']:
        raise ValueError('Copied deck differs from approved candidate bytes')
    result = run_ansys(copied, output_dir=output, executable_path=executable,
                       timeout_seconds=120, extra_args=['-np', '1', '-smp'])
    observed = _check_result(result, output, golden_path, expected, provenance['solver'])
    paths = {copied, *result.result_files}
    receipt = {
        'status': 'comparison_passed_unreviewed', 'native_qualification_complete': False,
        'case': provenance['case'], 'observed_at': datetime.now(timezone.utc).isoformat(),
        'profile': provenance['solver'], 'input_sha256': _sha256(copied),
        'harness_sha256': _sha256(__file__), 'relative_tolerance': 1e-3,
        'observed': observed,
        'artifacts': [{'path': path.relative_to(output).as_posix(), 'sha256': _sha256(path)}
                      for path in sorted(paths)],
    }
    target = output / 'resolve_receipt.json'
    target.write_text(json.dumps(receipt, indent=2, allow_nan=False), encoding='utf-8')
    if json.loads(target.read_text(encoding='utf-8')) != receipt:
        raise ValueError('Receipt readback differs')
    return receipt
