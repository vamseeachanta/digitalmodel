"""Hash oracle: hashlib applied directly to the specified canonical bytes."""
import hashlib

import pytest

from digitalmodel.parametric import refresh
from digitalmodel.parametric.text_basis import normalized_text_bytes


@pytest.mark.parametrize("ending", (b"\n", b"\r\n", b"\r"))
def test_shared_normalizer_and_source_digest(tmp_path, ending):
    path = tmp_path / "input.yml"
    path.write_bytes(b"# caf\xc3\xa9" + ending + b"value: a" + ending)
    canonical = b"# caf\xc3\xa9\nvalue: a\n"
    assert normalized_text_bytes(path) == canonical
    expected = hashlib.sha256(path.name.encode() + canonical).hexdigest()
    assert refresh._sha_files([path.name], tmp_path) == expected


@pytest.mark.parametrize("ending", (b"\n", b"\r\n", b"\r"))
def test_parametric_config_reuses_normalizer(tmp_path, monkeypatch, ending):
    monkeypatch.setattr(refresh, "_row", lambda _: {
        "basename": "fixture", "input": "input.yml",
        "parametric": {"physics": "linear", "response": "v", "axes": []},
    })
    path = tmp_path / "input.yml"
    path.write_bytes(b"value: a\n")
    expected = refresh.content_fingerprint("fixture", tmp_path)
    path.write_bytes(b"value: a" + ending)
    assert refresh.content_fingerprint("fixture", tmp_path) == expected
