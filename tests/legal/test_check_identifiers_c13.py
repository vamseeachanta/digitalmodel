"""Owner decisions C13/C16 (2026-09-25): exclusions removed, salt rotated.

* No whole-file exclusion remains for confidential content. What is left is
  the gate's own files and public-source data the owner chose to keep.
* The name-hash salt is rotated. A hash whose plaintext could not be recovered
  from the private sources is kept under the salt it was made with, in
  ``legacy_hashed_names``, so rotating does not drop a name from the gate.
  The gate matches both sets and fails closed on a malformed legacy block.

Every name here is synthetic.
"""

from __future__ import annotations

import hashlib
import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
import yaml

REPO = Path(__file__).resolve().parents[2]
CHECKER = REPO / "scripts" / "legal" / "check_identifiers.py"
RULES = REPO / ".legal-deny-list.yaml"
OLD_SALT = "digitalmodel-identifier-check-v1"
_GIT_BINDINGS = ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE")

pytestmark = pytest.mark.skipif(
    not CHECKER.exists() or not RULES.exists(), reason="identifier gate not installed"
)


def _h(salt: str, token: str) -> str:
    return hashlib.sha256(f"{salt}:{token.lower()}".encode()).hexdigest()


def _committed() -> dict:
    return yaml.safe_load(RULES.read_text(encoding="utf-8"))


# -- the committed rules -------------------------------------------------------


def test_the_salt_is_rotated():
    rules = _committed()
    assert rules["salt"] != OLD_SALT
    assert len(str(rules["salt"])) >= 24


def test_legacy_hashes_keep_their_own_salt_and_do_not_repeat():
    rules = _committed()
    legacy = rules.get("legacy_hashed_names")
    current = {h.lower() for h in rules["hashed_names"]}
    if legacy is None:
        return
    assert legacy["salt"] != rules["salt"]
    old = {h.lower() for h in legacy["hashes"]}
    assert old and not (old & current)


_CONFIDENTIAL_PREFIXES = (
    "docs/domains/ansys/",
    "docs/domains/pipecapacity/",
    "docs/domains/cathodic_protection/",
    "scripts/sanitize_s7_models.py",
)
_KEPT_PUBLIC = (
    ".legal-deny-list.yaml",
    "scripts/legal/check_identifiers.py",
    "data/vessels/",
    "docs/domains/freecad/src/ref/",
    "src/digitalmodel/web/digitaltwinfeed/StockAnalysis/static/StockAnalysis/data/",
)


def test_no_whole_file_exclusion_for_confidential_content():
    paths = [str(e["path"]) for e in _committed().get("exclusions") or []]
    bad = [p for p in paths if p.startswith(_CONFIDENTIAL_PREFIXES)]
    bad += [
        p
        for p in paths
        if os.path.splitext(p)[1].lower() in (".docx", ".pptx", ".xlsx")
    ]
    assert not bad
    unexpected = [p for p in paths if not p.startswith(_KEPT_PUBLIC)]
    assert not unexpected, unexpected


def test_no_exclusion_is_left_pending_a_decision():
    for e in _committed().get("exclusions") or []:
        assert "pending" not in str(e.get("reason", "")).lower(), e["path"]


# -- the gate honours both salts ----------------------------------------------


@pytest.fixture()
def gate(tmp_path):
    root = tmp_path / "repo"
    (root / "scripts" / "legal").mkdir(parents=True)
    shutil.copy(CHECKER, root / "scripts" / "legal" / "check_identifiers.py")
    home = tmp_path / "home"
    home.mkdir()

    def write_rules(rules: dict) -> None:
        (root / ".legal-deny-list.yaml").write_text(
            yaml.safe_dump(rules), encoding="utf-8"
        )

    def run(*args):
        e = {k: v for k, v in os.environ.items() if k not in _GIT_BINDINGS}
        for k in ("DIGITALMODEL_DENY_LIST", "CI", "GITHUB_ACTIONS"):
            e.pop(k, None)
        e["HOME"] = str(home)
        e["USERPROFILE"] = str(home)
        return subprocess.run(
            [
                sys.executable,
                str(root / "scripts" / "legal" / "check_identifiers.py"),
                *args,
            ],
            cwd=root,
            capture_output=True,
            text=True,
            env=e,
            encoding="utf-8",
            errors="replace",
        )

    run.root = root
    run.write_rules = write_rules
    return run


def _rules(**extra) -> dict:
    base = {"salt": "new-salt-for-test-000000000", "structural": [], "hashed_names": []}
    base.update(extra)
    return base


def _sample(gate, text: str) -> str:
    p = gate.root / "sample.md"
    p.write_text(text, encoding="utf-8")
    return str(p)


def test_a_name_under_the_current_salt_is_denied(gate):
    gate.write_rules(
        _rules(hashed_names=[_h("new-salt-for-test-000000000", "zzcurrentname")])
    )
    r = gate(_sample(gate, "built for zzcurrentname in 2020\n"))
    assert r.returncode != 0 and "denied-name" in r.stdout + r.stderr


def test_a_name_under_the_legacy_salt_is_still_denied(gate):
    gate.write_rules(
        _rules(
            legacy_hashed_names={
                "salt": OLD_SALT,
                "hashes": [_h(OLD_SALT, "zzlegacyname")],
            }
        )
    )
    r = gate(_sample(gate, "built for zzlegacyname in 2020\n"))
    assert r.returncode != 0 and "denied-name" in r.stdout + r.stderr


def test_a_legacy_hash_does_not_match_under_the_current_salt(gate):
    # The same name hashed with the old salt but listed as current is not a
    # match: each list is compared with its own salt.
    gate.write_rules(_rules(hashed_names=[_h(OLD_SALT, "zzlegacyname")]))
    r = gate(_sample(gate, "built for zzlegacyname in 2020\n"))
    assert r.returncode == 0, r.stdout + r.stderr


@pytest.mark.parametrize(
    "legacy",
    [
        {"hashes": ["a" * 64]},
        {"salt": "", "hashes": ["a" * 64]},
        {"salt": OLD_SALT, "hashes": "a" * 64},
        {"salt": OLD_SALT, "hashes": ["not-a-hash"]},
        ["a" * 64],
    ],
)
def test_a_malformed_legacy_block_fails_closed(gate, legacy):
    gate.write_rules(_rules(legacy_hashed_names=legacy))
    r = gate(_sample(gate, "nothing to see\n"))
    assert r.returncode != 0
    assert "legacy_hashed_names" in r.stdout + r.stderr
