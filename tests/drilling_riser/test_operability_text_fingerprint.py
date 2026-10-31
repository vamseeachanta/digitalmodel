# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Text provenance contract for the analytical operability atlas (#2761).

Expected hashes implement the specified byte rule independently: decode UTF-8,
replace CRLF with LF, encode UTF-8. Engineering values come from the declared
generic config; numerical comparisons use exact equality, without tolerances.
"""
from __future__ import annotations

from copy import deepcopy
import hashlib
import itertools
import json
from pathlib import Path

import numpy as np
import pandas as pd
import pytest
import yaml

from digitalmodel.drilling_riser import operability_atlas as oa
from digitalmodel.parametric.atlas import Atlas

CONFIG_REL = oa.CONFIG_PATH.relative_to(oa.REPO_ROOT).as_posix()
TEXT_INPUTS = (CONFIG_REL, *oa.SOURCE_FILES)


@pytest.fixture
def basis():
    """Snapshot only the declared text basis and an independent config mapping."""
    texts = {rel: (oa.REPO_ROOT / rel).read_bytes() for rel in TEXT_INPUTS}
    original = yaml.safe_load(texts[CONFIG_REL].decode("utf-8"))
    return texts, original, deepcopy(original)


def _copy_basis(root, texts, crlf=(), omit=()):
    for rel, raw in texts.items():
        if rel in omit:
            continue
        canonical = raw.decode("utf-8").replace("\r\n", "\n")
        target = root / rel
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(
            (canonical.replace("\n", "\r\n") if rel in crlf else canonical).encode("utf-8")
        )
    return root / CONFIG_REL


def _expected_fingerprint(texts):
    """Hand-constructed basis using the existing atlas identity schema."""
    canonical = {
        rel: raw.decode("utf-8").replace("\r\n", "\n").encode("utf-8")
        for rel, raw in texts.items()
    }
    basis = {
        "config_yaml": canonical[CONFIG_REL].decode("utf-8"),
        "standards": yaml.safe_load(canonical[CONFIG_REL])["provenance"]["standards"],
        "source_sha256": {
            rel: hashlib.sha256(canonical[rel]).hexdigest() for rel in oa.SOURCE_FILES
        },
        # Identity schema declared in operability_atlas, not engineering inputs.
        "spec": {"basename": "drilling_riser_operability",
                 "response": "governing_utilisation", "physics": "linear"},
    }
    return hashlib.sha256(json.dumps(basis, sort_keys=True).encode("utf-8")).hexdigest()


@pytest.mark.parametrize("comment", ["# ordinary ASCII text\n", "# café Δ 深度\n"])
def test_all_declared_text_inputs_are_newline_invariant(tmp_path, basis, comment):
    texts, original, snapshot = basis
    texts = {rel: raw + comment.encode("utf-8") for rel, raw in texts.items()}
    expected = _expected_fingerprint(texts)
    lf_root, mixed_root = tmp_path / "lf", tmp_path / "mixed"
    lf_config = _copy_basis(lf_root, texts)
    assert oa.content_fingerprint(lf_config, lf_root) == expected
    # Exhaust all assignments, including each member alone and all members CRLF.
    for flags in itertools.product((False, True), repeat=len(TEXT_INPUTS)):
        crlf = {rel for rel, enabled in zip(TEXT_INPUTS, flags) if enabled}
        config = _copy_basis(mixed_root, texts, crlf)
        assert oa.content_fingerprint(config, mixed_root) == expected, sorted(crlf)
        assert oa.load_config(config) == snapshot
    assert original == snapshot
    assert {p.relative_to(mixed_root).as_posix() for p in mixed_root.rglob("*")
            if p.is_file()} == set(TEXT_INPUTS)


def test_semantic_edits_and_edition_changes_invalidate_fingerprint(tmp_path, basis):
    texts, original, snapshot = basis
    root = tmp_path / "basis"
    config = _copy_basis(root, texts)
    fingerprint = oa.content_fingerprint(config, root)
    changed = deepcopy(snapshot)
    # Both stroke values are taken from the reviewed generic input example.
    changed["stackups"]["rsu_a"]["tj_stroke_m"] = snapshot["stackups"]["rsu_b"]["tj_stroke_m"]
    old_stroke = snapshot["stackups"]["rsu_a"]["tj_stroke_m"]
    new_stroke = changed["stackups"]["rsu_a"]["tj_stroke_m"]
    config.write_bytes(texts[CONFIG_REL].replace(
        f"tj_stroke_m: {old_stroke}".encode("utf-8"),
        f"tj_stroke_m: {new_stroke}".encode("utf-8"), 1,
    ))
    assert oa.load_config(config) == changed
    assert oa.content_fingerprint(config, root) != fingerprint

    _copy_basis(root, texts)
    edition = snapshot["provenance"]["standards"][0]["edition"]
    config.write_bytes(texts[CONFIG_REL].replace(
        edition.encode("utf-8"), b"synthetic-edition", 1
    ))
    assert oa.content_fingerprint(config, root) != fingerprint

    for rel in oa.SOURCE_FILES:
        _copy_basis(root, texts)
        # Actual language tokens, rather than a newline or comment-only change.
        token = b"\nfingerprint_control = True\n" if rel.endswith(".py") else b"\nfingerprint_control: true\n"
        (root / rel).write_bytes(texts[rel] + token)
        assert oa.content_fingerprint(config, root) != fingerprint, rel
    assert original == snapshot


def test_equivalent_numeric_spelling_is_not_reserialized(tmp_path, basis):
    texts, _, snapshot = basis
    root = tmp_path / "basis"
    config = _copy_basis(root, texts)
    fingerprint = oa.content_fingerprint(config, root)
    stroke = snapshot["stackups"]["rsu_a"]["tj_stroke_m"]
    changed = dict(texts)
    changed[CONFIG_REL] = texts[CONFIG_REL].replace(
        f"tj_stroke_m: {stroke}".encode("utf-8"),
        f"tj_stroke_m: {stroke}0".encode("utf-8"), 1,
    )
    config.write_bytes(changed[CONFIG_REL])
    assert oa.load_config(config) == snapshot
    assert oa.content_fingerprint(config, root) == _expected_fingerprint(changed)
    assert oa.content_fingerprint(config, root) != fingerprint


@pytest.mark.parametrize("rel", TEXT_INPUTS)
@pytest.mark.parametrize("failure", ["missing", "unreadable", "invalid-utf8"])
def test_missing_basis_refuses_verification(tmp_path, basis, monkeypatch, rel, failure):
    texts, _, _ = basis
    root = tmp_path / "basis"
    config = _copy_basis(root, texts, omit=(rel,) if failure == "missing" else ())
    target = root / rel
    if failure == "unreadable":
        read_bytes = Path.read_bytes

        def guarded_read(path):
            if path == target:
                raise PermissionError("declared text is unreadable")
            return read_bytes(path)

        # Deterministic even with elevated filesystem permissions.
        monkeypatch.setattr(Path, "read_bytes", guarded_read)
    elif failure == "invalid-utf8":
        target.write_bytes(b"\xff")
    with pytest.raises((OSError, UnicodeError)):
        oa.content_fingerprint(config, root)


@pytest.mark.parametrize("rel", TEXT_INPUTS)
@pytest.mark.parametrize("suffix", [b" \n", b"\n", b"\r", "# e\u0301\n".encode("utf-8")])
def test_other_text_bytes_remain_significant(tmp_path, basis, rel, suffix):
    texts, _, _ = basis
    root = tmp_path / "basis"
    config = _copy_basis(root, texts)
    fingerprint = oa.content_fingerprint(config, root)
    changed = dict(texts)
    changed[rel] += suffix
    # Write exact bytes: a lone CR must not be normalized by universal newlines.
    (root / rel).write_bytes(changed[rel])
    assert oa.content_fingerprint(config, root) == _expected_fingerprint(changed)
    assert oa.content_fingerprint(config, root) != fingerprint


def test_normalization_preserves_inputs_and_numeric_cache_values(tmp_path, basis):
    texts, original, snapshot = basis
    roots = (tmp_path / "lf", tmp_path / "crlf")
    configs = (
        _copy_basis(roots[0], texts),
        _copy_basis(roots[1], texts, crlf=TEXT_INPUTS),
    )
    before = [{rel: (root / rel).read_bytes() for rel in TEXT_INPUTS} for root in roots]
    atlases = [oa.build_atlas(config, root) for config, root in zip(configs, roots)]
    assert atlases[0].atlas_id == atlases[1].atlas_id
    pd.testing.assert_frame_equal(atlases[0].grid, atlases[1].grid, check_exact=True)
    assert atlases[0].validation == atlases[1].validation
    retained = Atlas.load(oa.REPO_ROOT / "atlases", oa.BASENAME)
    for atlas, config, root, raw_snapshot in zip(atlases, configs, roots, before):
        pd.testing.assert_frame_equal(atlas.grid, retained.grid, check_exact=True)
        # Operability boundary is the existing governing-utilisation <= 1 contract.
        np.testing.assert_array_equal(atlas.grid[oa.RESPONSE] <= 1, retained.grid[oa.RESPONSE] <= 1)
        assert [axis.to_dict() for axis in atlas.axes] == [axis.to_dict() for axis in retained.axes]
        assert oa.load_config(config) == snapshot
        assert {rel: (root / rel).read_bytes() for rel in TEXT_INPUTS} == raw_snapshot
    assert original == snapshot
    assert {rel: (oa.REPO_ROOT / rel).read_bytes() for rel in TEXT_INPUTS} == texts
