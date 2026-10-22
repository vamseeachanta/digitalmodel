"""Software identity checks; expected hashes use independently assembled bytes."""
import hashlib
import json

import pytest
import yaml

from digitalmodel.drilling_riser import operability_atlas as oa


@pytest.fixture
def basis(tmp_path):
    """Minimal UTF-8 basis; no engineering coefficients are needed."""
    config = tmp_path / "config.yml"
    config.write_bytes(b"provenance:\n  standards: []\nsetting: a\n# caf\xc3\xa9\n")
    for rel in oa.SOURCE_FILES:
        path = tmp_path / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(b"# caf\xc3\xa9\nVALUE = 'a'\n")
    return config, tmp_path


@pytest.mark.parametrize("rel", ("config.yml", *oa.SOURCE_FILES))
@pytest.mark.parametrize("ending", (b"\r\n", b"\r"), ids=("CRLF", "CR"))
def test_each_text_input_is_newline_portable(basis, rel, ending):
    config, root = basis
    expected = oa.content_fingerprint(config, root)
    path = root / rel
    path.write_bytes(path.read_bytes().replace(b"\n", ending))
    assert oa.content_fingerprint(config, root) == expected


@pytest.mark.parametrize("rel", ("config.yml", *oa.SOURCE_FILES))
def test_one_byte_content_edit_invalidates(basis, rel):
    config, root = basis
    before = oa.content_fingerprint(config, root)
    path = root / rel
    old, new = ((b"setting: a", b"setting: b") if rel == "config.yml"
                else (b"VALUE = 'a'", b"VALUE = 'b'"))
    path.write_bytes(path.read_bytes().replace(old, new))
    assert oa.content_fingerprint(config, root) != before


def test_lf_contract_matches_independent_legacy_hash(basis):
    """H(JSON({text, standards, path:H(bytes), spec})), sorted JSON keys.

    This is the pre-normalization operability_atlas.content_fingerprint contract;
    hashlib is applied directly to fixture bytes, without the production helper.
    """
    config, root = basis
    text = config.read_bytes().decode("utf-8")
    payload = {
        "config_yaml": text,
        "standards": yaml.safe_load(text)["provenance"]["standards"],
        "source_sha256": {
            rel: hashlib.sha256((root / rel).read_bytes()).hexdigest()
            for rel in oa.SOURCE_FILES
        },
        "spec": {"basename": oa.BASENAME, "response": oa.RESPONSE, "physics": oa.PHYSICS},
    }
    expected = hashlib.sha256(json.dumps(payload, sort_keys=True).encode()).hexdigest()
    assert oa.content_fingerprint(config, root) == expected


@pytest.mark.parametrize("rel", ("config.yml", *oa.SOURCE_FILES))
@pytest.mark.parametrize("bad_bytes", (b"\xff", b"\x00"), ids=("non-UTF8", "binary-NUL"))
def test_invalid_text_fails_closed(basis, rel, bad_bytes):
    config, root = basis
    with (root / rel).open("ab") as stream:
        stream.write(bad_bytes)
    with pytest.raises(ValueError, match="UTF-8 text"):
        oa.content_fingerprint(config, root)


@pytest.mark.parametrize("rel", oa.SOURCE_FILES)
def test_missing_source_names_typed_failure(basis, rel):
    config, root = basis
    # Rename within the disposable fixture, leaving the source evidence available.
    (root / rel).rename(root / "withheld-source")
    with pytest.raises(oa.SourceBasisUnavailable, match="source basis unavailable") as error:
        oa.content_fingerprint(config, root)
    assert error.value.missing_files == (rel,)


def test_installed_layout_names_all_missing_sources(tmp_path):
    config = tmp_path / "installed-config.yml"
    config.write_bytes(b"provenance:\n  standards: []\n")
    with pytest.raises(oa.SourceBasisUnavailable) as error:
        oa.content_fingerprint(config, tmp_path)
    assert error.value.missing_files == oa.SOURCE_FILES


def test_missing_config_is_typed(basis):
    config, root = basis
    config.rename(root / "withheld-config")
    with pytest.raises(oa.SourceBasisUnavailable) as error:
        oa.content_fingerprint(config, root)
    assert error.value.missing_files == ("config_yaml",)
