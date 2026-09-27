# ABOUTME: Tests for the #2157 weldolet visualisation decks and committed figure set: the
# ABOUTME: plotted model is the receipt model, receipts are untouched, PNGs are host-free.
"""Tests for ``digitalmodel.ansys.weldolet_figures`` (#2157 P4 Rev B, owner comment 2).

Comparator classes:

- identity: each visualisation deck starts with the exact receipt deck, whose SHA-256
  equals the digest recorded in the committed receipt (the picture shows the model that
  produced the numbers, and no receipt deck is changed);
- contract: the plotting block is appended after the receipt deck, writes PNG only and
  carries no system call; committed PNGs carry no text or time chunks and no host token;
- manifest: every committed figure is listed with its SHA-256, state, generator, level
  and solver release, and the digests match the files.
"""

from __future__ import annotations

import hashlib
import json
import struct
import zlib
from pathlib import Path

import pytest

from digitalmodel.ansys import weldolet_figures as wf

REPO = Path(__file__).resolve().parents[2]
EXAMPLE = REPO / "examples" / "workflows" / "crack-fe-weldolet"
FE_STATES = EXAMPLE / "fe_states"
FIGURES = EXAMPLE / "figures"
_HOST_TOKENS = (b"acma", b"ace-win", b"ace-linux", b"rds0", b"vamsee", b"c:\\users", b"d:\\ws")


def _png(chunks: list[tuple[bytes, bytes]]) -> bytes:
    out = [b"\x89PNG\r\n\x1a\n"]
    for kind, data in chunks:
        out.append(struct.pack(">I", len(data)) + kind + data
                   + struct.pack(">I", zlib.crc32(kind + data) & 0xFFFFFFFF))
    return b"".join(out)


def _chunk_types(blob: bytes) -> list[bytes]:
    assert blob[:8] == b"\x89PNG\r\n\x1a\n"
    pos, kinds = 8, []
    while pos < len(blob):
        (n,) = struct.unpack(">I", blob[pos:pos + 4])
        kinds.append(blob[pos + 4:pos + 8])
        pos += 12 + n
    return kinds


@pytest.mark.parametrize("run", sorted(wf.RUNS))
def test_visual_deck_starts_with_the_receipt_deck(run):
    spec = wf.RUNS[run]
    receipt = json.loads((FE_STATES / f"{spec.state}.receipt.json").read_text("utf-8"))
    base = wf.receipt_deck(spec.state, spec.level, FE_STATES)
    mesh = next(m for m in receipt["meshes"] if m["level"] == spec.level)
    assert hashlib.sha256(base.encode("utf-8")).hexdigest() == mesh["deck_sha256"]
    deck = wf.visual_deck(run, FE_STATES)
    assert deck.startswith(base)
    block = deck[len(base):]
    assert "/SHOW,PNG" in block and "/POST1" in block
    assert block.count("EPLOT") + block.count("PLNSOL") == len(spec.plots)
    low = block.lower()
    for bad in ("/sys", "*cfopen", "/copy", "/delete", "/rename"):
        assert bad not in low, bad


def test_visual_deck_fails_closed_on_a_changed_receipt(tmp_path):
    spec = wf.RUNS["crotch"]
    receipt = json.loads((FE_STATES / f"{spec.state}.receipt.json").read_text("utf-8"))
    for m in receipt["meshes"]:
        m["deck_sha256"] = "0" * 64
    (tmp_path / f"{spec.state}.receipt.json").write_text(json.dumps(receipt), "utf-8")
    with pytest.raises(ValueError, match="deck"):
        wf.receipt_deck(spec.state, spec.level, tmp_path)


def test_sanitise_png_drops_ancillary_chunks():
    ihdr = struct.pack(">IIBBBBB", 1, 1, 8, 2, 0, 0, 0)
    idat = zlib.compress(b"\x00\xff\xff\xff")
    raw = _png([(b"IHDR", ihdr), (b"tEXt", b"Author\x00someone@host"),
                (b"tIME", b"\x07\xea\x09\x1b\x0c\x00\x00"), (b"IDAT", idat), (b"IEND", b"")])
    clean = wf.sanitise_png(raw)
    assert _chunk_types(clean) == [b"IHDR", b"IDAT", b"IEND"]
    assert b"someone" not in clean


def test_sanitise_png_rejects_non_png():
    with pytest.raises(ValueError):
        wf.sanitise_png(b"GIF89a")


def test_committed_figures_match_the_manifest():
    manifest = json.loads((FIGURES / wf.MANIFEST).read_text("utf-8"))
    ids = {f for run in wf.RUNS.values() for f, _ in run.plots}
    assert set(manifest["figures"]) == ids
    for fid, rec in manifest["figures"].items():
        blob = (FIGURES / rec["file"]).read_bytes()
        assert hashlib.sha256(blob).hexdigest() == rec["sha256"], fid
        assert set(_chunk_types(blob)) <= {b"IHDR", b"PLTE", b"IDAT", b"IEND"}, fid
        low = blob.lower()
        assert not any(t in low for t in _HOST_TOKENS), fid
        for key in ("state", "level", "generator", "mapdl_release", "view", "receipt_deck_sha256"):
            assert rec.get(key) not in (None, ""), (fid, key)
    blob = json.dumps(manifest).lower()
    assert not any(t.decode() in blob for t in _HOST_TOKENS)
    assert "argv" not in blob
