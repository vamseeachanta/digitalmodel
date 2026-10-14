"""Synthetic regressions for persistent RAO identity; no solver data."""

import json
from datetime import datetime, timedelta, timezone
from pathlib import Path
from tempfile import TemporaryDirectory

import numpy as np
import pytest
import yaml

from digitalmodel.hydrodynamics.hull_library import rao_registry
from digitalmodel.hydrodynamics.hull_library.analysis_setup import _attempt_rao_lookup
from digitalmodel.hydrodynamics.hull_library.panel_catalog import RaoReference
from digitalmodel.hydrodynamics.hull_library.rao_registry import RaoRegistry


@pytest.fixture
def inputs():
    """One frequency, one heading and six DOFs, as in the issue reproduction."""
    return dict(
        hull_id="synthetic-box",
        solver="synthetic",
        draft_m=5.0,
        headings_deg=[0.0],
        frequencies=np.array([1.0]),
        amplitudes=np.ones((1, 1, 6)),
        phases=np.zeros((1, 1, 6)),
        loading_condition="condition-A",
        benchmark_revision="revision-A",
    )


@pytest.fixture
def registry(tmp_path):
    return RaoRegistry(tmp_path / "data" / "raos")


def test_issue_reproduction(tmp_path):
    """Run the issue's reproduction inside the test's temporary directory."""
    with TemporaryDirectory(dir=tmp_path) as d:
        r = RaoRegistry(Path(d) / "data" / "raos")
        kw = dict(
            hull_id="synthetic-box",
            solver="synthetic",
            draft_m=5.0,
            headings_deg=[0.0],
            frequencies=np.array([1.0]),
            phases=np.zeros((1, 1, 6)),
        )
        a = r.register_rao(
            **kw,
            amplitudes=np.ones((1, 1, 6)),
            loading_condition="condition-A",
            benchmark_revision="revision-A",
        )
        b = r.register_rao(
            **kw,
            amplitudes=np.full((1, 1, 6), 2.0),
            loading_condition="condition-B",
            benchmark_revision="revision-B",
        )
        assert (
            a.file_path == b.file_path,
            len(r.get_raos("synthetic-box")),
            a.loading_condition,
            r.load_rao_data(a)["loading_condition"],
        ) == (False, 2, "condition-A", "condition-A")
        np.testing.assert_array_equal(
            r.load_rao_data(a)["amplitudes"], np.ones((1, 1, 6))
        )


@pytest.mark.parametrize(
    "change",
    [
        {"loading_condition": "condition-B"},
        {"benchmark_revision": "revision-B"},
        {"amplitudes": np.full((1, 1, 6), 2.0)},
        {"draft_m": 5.01},  # Both drafts rounded to one decimal formerly gave 5.0.
        {"draft_m": 5.3},  # 5.3 - 5.0 = 0.3, inside the existing 0.5 m lookup.
    ],
)
def test_distinct_content_survives_same_day_and_reload(registry, inputs, change):
    first = registry.register_rao(**inputs)
    original = registry.load_rao_data(first)
    second = registry.register_rao(**(inputs | change))
    reopened = RaoRegistry(registry.index_path.parent)
    assert len(reopened.get_raos(inputs["hull_id"], draft_m=5.0)) == 2
    assert first.file_path != second.file_path
    assert first.date == second.date
    assert reopened.load_rao_data(RaoReference(**first.to_dict())) == original
    assert reopened.get_raos(inputs["hull_id"])[0] == second
    assert reopened.load_rao_data(second) != original


def test_identical_registration_across_dates_preserves_bytes(
    registry, inputs, monkeypatch
):
    class Clock:
        value = datetime(2026, 2, 13, tzinfo=timezone.utc)

        @classmethod
        def now(cls, tz):
            return cls.value

    monkeypatch.setattr(rao_registry, "datetime", Clock)
    first = registry.register_rao(**inputs)
    before = {
        p.name: p.read_bytes()
        for p in registry.index_path.parent.rglob("*")
        if p.is_file()
    }
    Clock.value += timedelta(days=1)
    second = RaoRegistry(registry.index_path.parent).register_rao(**inputs)
    after = {
        p.name: p.read_bytes()
        for p in registry.index_path.parent.rglob("*")
        if p.is_file()
    }
    assert second == first
    assert after == before


@pytest.mark.parametrize(
    "field,value",
    [
        ("amplitudes", [[[2.0] * 6]]),
        ("phases_deg", [[[1.0] * 6]]),
        ("frequencies_rad_s", [2.0]),
        ("loading_condition", "condition-B"),
        ("benchmark_revision", "revision-B"),
        ("hull_id", "other-synthetic"),
        ("date", "changed"),
    ],
)
def test_payload_tampering_fails_integrity(registry, inputs, field, value):
    ref = registry.register_rao(**inputs)
    data = registry.load_rao_data(ref)
    data[field] = value
    path = registry.index_path.parent.parent.parent / ref.file_path
    path.write_text(json.dumps(data), encoding="utf-8")
    with pytest.raises(ValueError, match="integrity"):
        registry.load_rao_data(ref)


@pytest.mark.parametrize(
    "field,value",
    [
        ("solver", "other-synthetic"),
        ("draft_m", 5.01),
        ("loading_condition", "condition-B"),
        ("benchmark_revision", "revision-B"),
        ("headings_deg", [90.0]),
        ("n_frequencies", 2),
        ("date", "changed"),
    ],
)
def test_reference_metadata_mismatch_fails(registry, inputs, field, value):
    ref = registry.register_rao(**inputs)
    wrong = ref.model_copy(update={field: value})
    with pytest.raises(ValueError, match="integrity"):
        registry.load_rao_data(wrong)


def test_reregistration_does_not_repair_corrupt_history(registry, inputs):
    ref = registry.register_rao(**inputs)
    path = registry.index_path.parent.parent.parent / ref.file_path
    data = registry.load_rao_data(ref)
    data["amplitudes"] = [[[2.0] * 6]]
    corrupt = json.dumps(data)
    path.write_text(corrupt, encoding="utf-8")
    with pytest.raises(ValueError, match="integrity"):
        registry.register_rao(**inputs)
    assert path.read_text(encoding="utf-8") == corrupt


def test_redirected_file_cannot_replace_reference_digest(registry, inputs, monkeypatch):
    """Simulate an in-registry symlink without requiring OS symlink privileges."""
    first = registry.register_rao(**inputs)
    second = registry.register_rao(
        **(inputs | {"amplitudes": inputs["amplitudes"] * 2})
    )
    root = registry.index_path.parent.parent.parent
    original_path = root / first.file_path
    target_path = root / second.file_path
    resolve, open_file = Path.resolve, Path.open

    def redirected_resolve(path, *args, **kwargs):
        return resolve(target_path if path == original_path else path, *args, **kwargs)

    def redirected_open(path, *args, **kwargs):
        return open_file(
            target_path if path == original_path else path, *args, **kwargs
        )

    monkeypatch.setattr(Path, "resolve", redirected_resolve)
    monkeypatch.setattr(Path, "open", redirected_open)
    with pytest.raises(ValueError, match="integrity"):
        registry.load_rao_data(first)


def _write_legacy_index(registry, inputs):
    """Build version 1.0 index/JSON exactly in the former writer's shape."""
    directory = registry.index_path.parent
    hull = directory / inputs["hull_id"]
    hull.mkdir(parents=True)
    path = hull / "synthetic_5p0m_2026-02-13.json"
    ref = RaoReference(
        solver=inputs["solver"],
        draft_m=inputs["draft_m"],
        headings_deg=inputs["headings_deg"],
        n_frequencies=1,
        date="2026-02-13",
        loading_condition=inputs["loading_condition"],
        benchmark_revision=inputs["benchmark_revision"],
        file_path=path.relative_to(directory.parent.parent).as_posix(),
    )
    payload = dict(
        hull_id=inputs["hull_id"],
        solver=ref.solver,
        draft_m=ref.draft_m,
        headings_deg=ref.headings_deg,
        frequencies_rad_s=inputs["frequencies"].tolist(),
        amplitudes=inputs["amplitudes"].tolist(),
        phases_deg=inputs["phases"].tolist(),
        date=ref.date,
        loading_condition=ref.loading_condition,
        benchmark_revision=ref.benchmark_revision,
    )
    path.write_text(json.dumps(payload), encoding="utf-8")
    legacy_bytes = path.read_bytes()
    registry.index_path.write_text(
        yaml.safe_dump(
            {"version": "1.0", "hulls": {inputs["hull_id"]: [ref.to_dict()]}}
        ),
        encoding="utf-8",
    )
    return ref, path, legacy_bytes


def test_legacy_index_is_discoverable_but_not_promoted(registry, inputs):
    ref, path, legacy_bytes = _write_legacy_index(registry, inputs)
    directory = registry.index_path.parent
    reopened = RaoRegistry(directory)
    assert reopened.get_raos(inputs["hull_id"]) == [ref]
    with pytest.raises(ValueError, match="integrity.*legacy"):
        reopened.load_rao_data(ref)
    fresh = reopened.register_rao(**inputs)
    assert fresh.file_path != ref.file_path
    assert reopened.get_raos(inputs["hull_id"]) == [fresh, ref]
    assert path.read_bytes() == legacy_bytes
    assert _attempt_rao_lookup(inputs["hull_id"], inputs["draft_m"], directory) == (
        True,
        reopened.load_rao_data(fresh),
    )


def test_caller_uses_newest_retained_revision(registry, inputs):
    first = registry.register_rao(**inputs)
    second = registry.register_rao(
        **(inputs | {"amplitudes": inputs["amplitudes"] * 2})
    )
    assert registry.get_raos(inputs["hull_id"]) == [second, first]
    available, data = _attempt_rao_lookup(
        inputs["hull_id"], inputs["draft_m"], registry.index_path.parent
    )
    assert available
    np.testing.assert_array_equal(data["amplitudes"], inputs["amplitudes"] * 2)
