"""The numerical intake pins eager package code as well as parser helpers."""
from pathlib import Path
from digitalmodel.ansys.analysis_replay_inputs import source_inventory, validate_review
from digitalmodel.ansys.analysis_records import digest_bytes


def test_inventory_covers_package_initializers_and_all_ansys_sources():
    import digitalmodel.ansys.analysis_replay_inputs as module
    directory = Path(module.__file__).resolve().parent
    root = directory.parents[2]
    expected = [*directory.glob('*.py'), directory.parent / '__init__.py',
                directory.parent / '_compat.py']
    inventory = source_inventory()
    for path in expected:
        assert inventory[path.relative_to(root).as_posix()] == digest_bytes(path.read_bytes())


def test_review_matches_absolute_parent_package_paths():
    digest = 'a' * 64
    path = 'src/digitalmodel/_compat.py'
    review = dict(status='REVIEW_RECEIVED', bundle_sha256=digest,
                  review=dict(verdict='MINOR', bundle_sha256=digest),
                  files=[dict(path='C:/fixture/' + path, sha256=digest)])
    validate_review(review, {path: digest})
