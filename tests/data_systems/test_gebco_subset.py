"""Offline subset contracts; mutated values are synthetic, not survey data."""

import hashlib
from importlib import import_module
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess

import numpy as np
import pytest

PACKAGE = "digitalmodel.data_systems.data_procurement.metocean.api_clients"
import_module(PACKAGE)
ROOT = Path(__file__).resolve().parents[2]
FIXTURE = Path(__file__).parent / "fixtures/gebco_2025"


def load(path=FIXTURE / "subset.asc", provenance=FIXTURE / "provenance.json"):
    return import_module(f"{PACKAGE}.gebco_subset").load_gebco_2025_subset(path, provenance)


@pytest.fixture(autouse=True)
def no_network(monkeypatch):
    def deny(*args, **kwargs):
        raise AssertionError("Loading must not create a socket")
    monkeypatch.setattr(socket, "socket", deny)


@pytest.fixture
def subset_copy(tmp_path):
    def write(text=None, **changes):
        data = (FIXTURE / "subset.asc").read_bytes() if text is None else text.encode("ascii")
        meta = json.loads((FIXTURE / "provenance.json").read_bytes())
        meta.update(subset_sha256=hashlib.sha256(data).hexdigest(), subset_byte_count=len(data))
        try:
            meta["subset_header"] = {k.lower(): float(v) for k, v in
                                     (line.split() for line in data.decode().splitlines()[:6])}
        except ValueError:
            pass  # Malformed headers must still reach the production parser.
        meta.update(changes)
        meta = {key: value for key, value in meta.items() if value is not None}
        path, provenance = tmp_path / "subset.asc", tmp_path / "provenance.json"
        path.write_bytes(data)
        provenance.write_text(json.dumps(meta), encoding="utf-8")
        return path, provenance
    return write


def test_source_comparators_cell_centres_and_provenance():
    grid = load()
    assert grid.elevation_m.shape == grid.valid_mask.shape == (4, 4)
    np.testing.assert_array_equal(grid.elevation_m, np.repeat([[-4226], [-4224], [-4223], [-4224]], 4, axis=1))
    assert grid.valid_mask.all()
    # Independently use the source tile's north edge and the extraction indices.
    step = 0.004166666667
    np.testing.assert_allclose(grid.longitude, [-90 + (c + .5) * step for c in range(4)], rtol=0, atol=1e-12)
    np.testing.assert_allclose(grid.latitude, [(21600 - r - .5) * step for r in range(4)], rtol=0, atol=1e-12)
    np.testing.assert_array_equal(grid.depth_m[:, 0], [4226, 4224, 4223, 4224])
    meta = json.loads((FIXTURE / "provenance.json").read_bytes())
    assert grid.provenance == meta
    assert meta["source_header"]["nrows"] == meta["source_header"]["ncols"] == 21600
    assert meta["source_row_indices_zero_based"] == meta["source_column_indices_zero_based"] == [0, 1, 2, 3]
    assert meta["doi"] == "10.5285/37c52e96-24ea-67ce-e063-7086abc05f29"
    assert meta["units"] == {"elevation": "metres", "positive": "up", "coordinates": "degrees"}
    assert meta["product_variant"] == "ice_surface_elevation"


def test_land_zero_underwater_and_missing_stay_distinct(subset_copy):
    text = (FIXTURE / "subset.asc").read_text().replace("-4226 -4226 -4226 -4226", "12.5 0 -1.25 -32767")
    grid = load(*subset_copy(text))
    np.testing.assert_array_equal(grid.elevation_m[0, :3], [12.5, 0, -1.25])
    np.testing.assert_array_equal(grid.depth_m[0, :3], [0, 0, 1.25])
    np.testing.assert_array_equal(grid.valid_mask[0], [True, True, True, False])
    assert np.isnan(grid.elevation_m[0, 3]) and np.isnan(grid.depth_m[0, 3])


@pytest.mark.parametrize("step,offset", [(1 / 240, 0), (1 / 240 + 1e-8, 1e-8), (1 / 240 - 1e-8, -1e-8)])
def test_other_subset_geometry(subset_copy, step, offset):
    grid = load(*subset_copy(f"ncols 2\nnrows 2\nxllcorner {5 + offset}\nyllcorner {-2 + offset}\n"
                             f"cellsize {step}\nNODATA_value -32767\n-1 -2\n-3 -4\n"))
    np.testing.assert_array_equal(grid.longitude, [5 + offset + .5 * step, 5 + offset + 1.5 * step])
    np.testing.assert_array_equal(grid.latitude, [-2 + offset + 1.5 * step, -2 + offset + .5 * step])
    np.testing.assert_array_equal(grid.elevation_m, [[-1, -2], [-3, -4]])


@pytest.mark.parametrize("field,value,reason", [
    ("cellsize", 0.5, "cellsize"), ("cellsize", 1 / 120, "cellsize"),
    ("cellsize", 1 / 240 + 3e-8, "cellsize"),
    ("xllcorner", 5 + 1 / 480, "origin"),
    ("yllcorner", -2 + 1 / 480, "origin"),
    ("xllcorner", 5 - 3e-8, "origin"), ("yllcorner", -2 + 3e-8, "origin"),
])
def test_non_gebco_geometry_rejected(subset_copy, field, value, reason):
    header = dict(ncols=2, nrows=2, xllcorner=5, yllcorner=-2,
                  cellsize=1 / 240, NODATA_value=-32767)
    header[field] = value
    text = "".join(f"{key} {value}\n" for key, value in header.items()) + "-1 -2\n-3 -4\n"
    with pytest.raises(ValueError, match=f"GEBCO_2025 {reason}"):
        load(*subset_copy(text))


@pytest.mark.parametrize("old,new,reason", [
    ("ncols 4", "ncols 0", "positive integers"), ("nrows 4", "nrows -1", "positive integers"),
    ("ncols 4", "ncols 4.5", "header value"), ("ncols 4", "ncols inf", "header value"),
    ("nrows 4", "ncols 4", "unique corner-registered"), ("ncols 4\n", "", "Malformed ESRI ASCII header"),
    ("ncols 4", "ncols " + "9" * 400, "250000 cells"), ("nrows 4", "nrows " + "9" * 400, "250000 cells"),
    ("xllcorner -90", "xllcenter -90", "unique corner-registered"), ("yllcorner", "yllcenter", "unique corner-registered"),
    ("cellsize 0.004166666667", "cellsize 0", "geographic extent"),
    ("cellsize 0.004166666667", "cellsize -1", "geographic extent"),
    ("cellsize 0.004166666667", "cellsize nan", "Missing or nonfinite"),
    ("xllcorner -90", "xllcorner 179.999", "geographic extent"), ("xllcorner -90", "xllcorner -181", "geographic extent"),
    ("yllcorner 89.983333340532", "yllcorner 90", "geographic extent"),
    ("yllcorner 89.983333340532", "yllcorner -91", "geographic extent"),
    ("NODATA_value -32767", "NODATA_value inf", "Missing or nonfinite"),
    ("-4223 -4223 -4223 -4223\n", "", "declared shape"),
    ("-4226 -4226 -4226 -4226", "-4226 -4226 -4226", "declared shape"),
    ("-4226 -4226 -4226 -4226", "-4226 -4226 -4226 -4226 -4226", "declared shape"),
    *(("-4223", token, "must be finite") for token in ("nan", "inf", "-inf", "1e309")),
    ("-4223", "unknown", "could not convert string to float"), ("ncols 4", "other 4", "unique corner-registered"),
])
def test_malformed_ascii_rejected(subset_copy, old, new, reason):
    text = (FIXTURE / "subset.asc").read_text().replace(old, new)
    with pytest.raises(ValueError, match=reason):
        load(*subset_copy(text))


@pytest.mark.parametrize("changes,reason", [
    ({"edition": "GEBCO_2024"}, "edition"), ({"subset_sha256": "0" * 64}, "digest or byte count"),
    ({"subset_byte_count": 0}, "digest or byte count"), ({"subset_header": {}}, "subset header mismatch"),
    ({"units": {"elevation": "feet", "positive": "down"}}, "units"), ({"units": None}, "units"),
    ({"units": {"elevation": "metres", "positive": "down", "coordinates": "degrees"}}, "units"),
    ({"units": {"elevation": "metres", "positive": "up", "coordinates": "radians"}}, "units"),
    *(({field: value}, field) for field in ("source_url", "doi", "attribution")
      for value in (None, "", " ", [])),
])
def test_provenance_admission(subset_copy, changes, reason):
    with pytest.raises(ValueError, match=reason):
        load(*subset_copy(**changes))


@pytest.mark.parametrize("payload,reason", [
    (b"{}", "edition"), (b"[]", "edition"), (b"not JSON", "Malformed GEBCO provenance JSON"),
    pytest.param(b"[" * 100_000 + b"0" + b"]" * 100_000, "Malformed GEBCO provenance JSON", id="deep-json"),
    (None, "Cannot read GEBCO input"),
])
def test_missing_or_malformed_provenance(subset_copy, payload, reason):
    path, provenance = subset_copy()
    if payload is None:
        provenance.unlink()
    else:
        provenance.write_bytes(payload)
    with pytest.raises(ValueError, match=reason):
        load(path, provenance)


@pytest.mark.parametrize("overflow", ["subset", "provenance", "cells"])
def test_limits_checked_before_cell_allocation(subset_copy, monkeypatch, overflow):
    text = (FIXTURE / "subset.asc").read_text()
    if overflow == "cells":
        text = text.replace("ncols 4", "ncols 250001").replace("nrows 4", "nrows 1")
    path, provenance = subset_copy(text)
    if overflow != "cells":
        with (path if overflow == "subset" else provenance).open("wb") as stream:
            stream.truncate(8 * 1024 * 1024 + 1)
    def deny(*args, **kwargs):
        raise AssertionError("Oversized inputs must fail before allocating cells")
    monkeypatch.setattr(np, "array", deny)
    with pytest.raises(ValueError, match="250000 cells" if overflow == "cells" else "input size"):
        load(path, provenance)


def test_fixture_digest_survives_autocrlf_checkout(tmp_path, monkeypatch):
    # Poison inherited repository bindings; the child must still be isolated.
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    def git(*args, cwd=tmp_path):
        return subprocess.run(["git", "-c", "core.autocrlf=true", *args],
                              cwd=cwd, env=env, check=True, capture_output=True)
    caller = [ROOT / ".gitattributes", *FIXTURE.iterdir()]
    for name in ("HEAD", "index"):
        caller.append(ROOT / git("rev-parse", "--git-path", name, cwd=ROOT).stdout.decode().strip())
    before = {p: p.read_bytes() for p in caller}
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        monkeypatch.setenv(key, str(ROOT / ".git"))
    git("init")
    shutil.copyfile(ROOT / ".gitattributes", tmp_path / ".gitattributes")
    relative = FIXTURE.relative_to(ROOT)
    target = tmp_path / relative
    shutil.copytree(FIXTURE, target)
    git("add", ".gitattributes", str(relative))
    shutil.rmtree(target)
    git("checkout-index", "--all", "--force")
    for name in ("subset.asc", "provenance.json"):
        assert b"\r\n" not in (target / name).read_bytes()
        assert (target / name).read_bytes() == (FIXTURE / name).read_bytes()
    meta = json.loads((target / "provenance.json").read_bytes())
    payload = (target / "subset.asc").read_bytes()
    assert hashlib.sha256(payload).hexdigest() == meta["subset_sha256"]
    assert len(payload) == meta["subset_byte_count"]
    grid = load(target / "subset.asc", target / "provenance.json")
    assert grid.elevation_m.shape == (4, 4)
    assert all(p.read_bytes() == data for p, data in before.items())
