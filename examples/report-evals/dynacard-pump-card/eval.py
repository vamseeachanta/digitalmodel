"""Rebuild public-case arithmetic from this checkout; no runtime network access."""
import hashlib
import importlib.util
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
import evalkit

REPO_ROOT = HERE.parents[2]
FOLDER = 'examples/report-evals/dynacard-pump-card'
FIXTURE = 'tests/marine_ops/artificial_lift/test_data/cleansed_well_001.json'


def add_args(parser):
    parser.add_argument('--repo-root', type=Path, default=REPO_ROOT)
    parser.set_defaults(offline=True)


def check_package(args, package, head):
    root = Path(args.repo_root).resolve()
    if root not in package.resolve().parents:
        raise ImportError('digitalmodel must load from the checkout being graded; set PYTHONPATH to its src folder')
    if args.revision == 'pinned' and head != args.pinned_commit:
        raise ImportError('pinned reproduction requires the package checkout at code.commit; the current pin is provisional until commit')


def load_analysis(root):
    folder = root / FOLDER
    if not (folder / 'analysis.py').exists():
        raise ImportError('analysis code is absent at this revision; re-pin after committing the report')
    sys.path.insert(0, str(folder))
    spec = importlib.util.spec_from_file_location('graded_dynacard_analysis', folder / 'analysis.py')
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def provenance(args, context):
    import digitalmodel
    root = Path(args.repo_root).resolve()
    package = Path(digitalmodel.__file__).resolve()
    head = evalkit.git_head(package.parent)
    check_package(args, package, head)
    context.update(repo_head=head, digitalmodel_head=head,
                   digitalmodel_file=package.relative_to(root).as_posix(), code_mode=args.revision)
    paths = [root / FIXTURE, *sorted((root / FOLDER).glob('*.py')),
             *sorted((root / 'src/digitalmodel/marine_ops/artificial_lift/dynacard').rglob('*.py')),
             *sorted((root / 'src/digitalmodel/marine_ops/artificial_lift/dynacard/data').glob('*.json*')),
             root / 'tests/marine_ops/artificial_lift/dynacard/testdata/plate_classifier_score.json',
             root / 'tests/marine_ops/artificial_lift/dynacard/testdata/mendeley_plate/Working condition diagnosis plate.xls']
    context['sha256'] = {p.relative_to(root).as_posix(): evalkit.sha256_file(p) for p in paths}
    if args.revision == 'pinned':
        for path in paths:
            rel = path.relative_to(root).as_posix()
            blob = evalkit.git_blob(root, args.pinned_commit, rel, fetch=False)
            if blob is None or hashlib.sha256(blob).hexdigest() != evalkit.sha256_file(path):
                raise ImportError('pinned reproduction requires unmodified source and inputs at code.commit')
    return root


def compute(args, values, notes, context):
    root = provenance(args, context)
    module = load_analysis(root)
    result = module.run(root / FIXTURE, resource_root=root)
    from build_results import numerical_results
    for key, label, value, unit, digits in numerical_results(result):
        values[key] = value
        notes[key] = 'Arithmetic only; derived pump metrics depend on ASSUMED model inputs.'
    context['source_sha256'] = {s['what']: s['sha256'] for s in result['sources']}
    context['shape_hypothesis_status'] = result['shape_hypothesis']['status']
    context['published_example_status'] = result['validation']['published_example']['status']


if __name__ == '__main__':
    raise SystemExit(evalkit.main(HERE / 'results.json', compute, add_args))
