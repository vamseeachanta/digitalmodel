# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Mutation regressions for the accelerogram's retained source provenance.

Independent hand example (SI): times [0, 0.5, 1, 1.5] and accelerations
[0, 1, -2, 1]. Counting gives npts = 4; subtracting the first two times
gives dt = 0.5 s; max(abs(acceleration)) gives PGA = 2 m/s². These are
synthetic inputs, with exact equality and no empirical acceptance limit.
"""

from copy import copy, deepcopy
from dataclasses import asdict, replace
from hashlib import sha256
import pickle

import numpy as np
import pytest

from digitalmodel.seismic.readers.ascii import AccelerogramRecord, read_ascii_accelerogram


@pytest.fixture
def record(tmp_path):
    """Use the hand example above, with the digest of its literal source bytes."""
    path = tmp_path / "synthetic.asc"
    path.write_bytes(b"0 0\n0.5 1\n1 -2\n1.5 1\n")
    return read_ascii_accelerogram(path, units="m_s2")


def _assert_original(record):
    np.testing.assert_array_equal(record.time_s, [0, 0.5, 1, 1.5])
    np.testing.assert_array_equal(record.acceleration_m_s2, [0, 1, -2, 1])
    assert record.time_s.dtype == np.dtype(float)
    assert record.acceleration_m_s2.dtype == np.dtype(float)
    assert record.npts == 4
    assert record.dt_s == 0.5
    assert record.pga_m_s2 == 2
    assert record.units == "m_s2"
    assert record.source_digest == sha256(b"0 0\n0.5 1\n1 -2\n1.5 1\n").hexdigest()


def test_constructor_snapshots_caller_arrays(record):
    time = record.time_s.copy()
    acceleration = record.acceleration_m_s2.copy()
    retained = replace(record, time_s=time, acceleration_m_s2=acceleration)
    time.fill(0)
    acceleration.fill(0)
    _assert_original(retained)


@pytest.mark.parametrize("field", ["time_s", "acceleration_m_s2"])
def test_public_array_and_every_base_refuse_write_restoration(record, field):
    """NumPy ndarray.flags: WRITEABLE needs writable ultimate backing storage."""
    array = getattr(record, field)
    with pytest.raises(ValueError):
        array.flat[0] = 0
    while isinstance(array, np.ndarray):
        assert not array.flags.writeable
        with pytest.raises(ValueError):
            array.setflags(write=True)
        array = array.base
    assert isinstance(array, bytes)
    _assert_original(record)


@pytest.mark.parametrize("field", ["time_s", "acceleration_m_s2"])
@pytest.mark.parametrize("metadata", ["shape", "dtype", "strides"])
def test_public_metadata_changes_do_not_reach_retained_samples(record, field, metadata):
    """The four-sample oracle must survive valid ndarray header edits."""
    array = getattr(record, field)
    if metadata == "shape":
        array.shape = (2, 2)  # Four samples can be reshaped into two pairs.
    elif metadata == "dtype":
        array.dtype = np.uint8  # Reinterpret bytes without changing any byte.
    else:
        array.strides = (0,)  # A legal header can read the first sample repeatedly.
    _assert_original(record)


@pytest.mark.parametrize("field", ["time_s", "acceleration_m_s2"])
def test_public_base_headers_are_not_retained(record, field):
    """Edit every exposed ndarray header, including any reachable bases."""
    array = getattr(record, field)
    while isinstance(array, np.ndarray):
        parent = array.base
        array.shape = (array.size,)
        array.dtype = np.uint8
        array.strides = (0,)
        array = parent
    _assert_original(record)


@pytest.mark.parametrize("reconstruct", [copy, deepcopy])
def test_copy_reconstructs_immutable_samples(record, reconstruct):
    restored = reconstruct(record)
    _assert_reconstructed(restored, record)


@pytest.mark.parametrize("protocol", range(pickle.HIGHEST_PROTOCOL + 1))
def test_pickle_reconstructs_immutable_samples(record, protocol):
    """Python pickle reference: all supported protocol numbers are exercised."""
    restored = pickle.loads(pickle.dumps(record, protocol=protocol))
    _assert_reconstructed(restored, record)


def _assert_reconstructed(restored, original):
    assert restored is not original
    assert restored.source_path == original.source_path
    _assert_original(restored)
    for field in ("time_s", "acceleration_m_s2"):
        test_public_array_and_every_base_refuse_write_restoration(restored, field)
        test_public_base_headers_are_not_retained(restored, field)
    _assert_original(original)


def test_dataclass_read_and_replace_apis_preserve_snapshot(record):
    """asdict returns ordinary caller data; replace rebuilds a protected record."""
    exported = asdict(record)
    exported["time_s"].fill(0)
    exported["acceleration_m_s2"].fill(0)
    _assert_original(record)
    _assert_reconstructed(replace(record), record)


def test_legacy_default_pickle_state_is_snapshotted(record, monkeypatch):
    """Emulate the former default dict-state pickle with writable array fields."""
    legacy = object.__new__(AccelerogramRecord)
    legacy.__dict__.update(asdict(record))
    with monkeypatch.context() as patch:
        patch.setattr(AccelerogramRecord, "__reduce__", object.__reduce__, raising=False)
        payload = pickle.dumps(legacy)
    _assert_reconstructed(pickle.loads(payload), record)


def test_noncontiguous_constructor_inputs_are_snapshotted(record):
    """Reversing a reversed view gives the hand example with negative strides."""
    time = np.array([1.5, 1, 0.5, 0])
    acceleration = np.array([1, -2, 1, 0], dtype=float)
    retained = replace(record, time_s=time[::-1], acceleration_m_s2=acceleration[::-1])
    time.fill(0)
    acceleration.fill(0)
    _assert_original(retained)


def test_derived_acceleration_output_cannot_change_samples(record):
    derived = record.acceleration_g
    derived.fill(0)
    _assert_original(record)


@pytest.mark.parametrize("field", ["time_s", "acceleration_m_s2"])
def test_object_references_cannot_be_retained_as_raw_bytes(record, field):
    """Object buffers contain references rather than immutable sample values."""
    with pytest.raises(TypeError, match="object references"):
        replace(record, **{field: getattr(record, field).astype(object)})
