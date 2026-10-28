# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Checkout conversions must preserve hash-bound campaign bytes."""

import json
from pathlib import Path
import subprocess

import pytest


ROOT = Path(__file__).resolve().parents[2]
CAMPAIGN = ROOT / "examples/riser-campaign"


@pytest.fixture(scope="module")
def pinned_paths(tmp_path_factory):
    """Audit campaign artifacts and every source captured by the writer."""
    from scripts.riser_campaign.generate_inputs import generate

    index = generate(CAMPAIGN / "manifest.json", tmp_path_factory.mktemp("checkout-audit"))
    paths = {p.relative_to(ROOT).as_posix() for p in CAMPAIGN.rglob("*") if p.is_file()}
    paths.update(entry["path"].removeprefix("source/")
                 for entry in index["frozen_tranche"]["code_sources"])
    return sorted(paths)


def test_hash_bound_paths_declare_lf_checkout(pinned_paths):
    result = subprocess.run(
        ["git", "check-attr", "text", "eol", "--", *pinned_paths],
        cwd=ROOT, check=True, capture_output=True, text=True, encoding="utf-8",
    )
    expected = {f"{path}: {attribute}: {value}" for path in pinned_paths
                for attribute, value in (("text", "set"), ("eol", "lf"))}
    assert set(result.stdout.splitlines()) == expected


@pytest.mark.parametrize("autocrlf", ["true", "false", "input"])
def test_checkout_preserves_committed_campaign_bytes(pinned_paths, autocrlf):
    for relative in pinned_paths:
        # A new path has no HEAD blob before staging. A committed LF probe
        # still exercises Git's real checkout filters for that exact path.
        blob = f"HEAD:{relative}"
        exists = subprocess.run(["git", "cat-file", "-e", blob], cwd=ROOT, capture_output=True)
        if exists.returncode:
            blob = "HEAD:examples/riser-campaign/manifest.json"
            assert b"\r" not in (ROOT / relative).read_bytes(), relative
        committed = subprocess.run(
            ["git", "show", blob],
            cwd=ROOT, check=True, capture_output=True,
        ).stdout
        checked_out = subprocess.run(
            ["git", "-c", f"core.autocrlf={autocrlf}", "-c", "core.eol=crlf",
             "cat-file", "--filters", f"--path={relative}", blob],
            cwd=ROOT, check=True, capture_output=True,
        ).stdout
        assert checked_out == committed, relative


@pytest.mark.parametrize("name,kind", [
    ("matrix-i0339-2.tsv", "scope"), ("criteria-i0339-2.json", "criteria"),
])
def test_line_ending_mutations_still_fail_raw_digest_validation(name, kind):
    from scripts.riser_campaign.frozen_scope import source_payloads, validate_tranche

    tranche = json.loads((CAMPAIGN / "manifest.json").read_text(encoding="utf-8"))["frozen_tranche"]
    payloads = source_payloads()
    payloads[name] = payloads[name].replace(b"\n", b"\r\n")
    with pytest.raises(ValueError, match=f"frozen {kind} digest mismatch"):
        validate_tranche(tranche, payloads)
