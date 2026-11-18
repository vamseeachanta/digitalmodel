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


@pytest.mark.parametrize(
    "filters,expected_labels",
    [
        ({"loading_condition": "condition-A"}, ["ar", "a"]),
        ({"benchmark_revision": "revision-A"}, ["b", "a"]),
        (
            {
                "loading_condition": "condition-A",
                "benchmark_revision": "revision-A",
            },
            ["a"],
        ),
        (
            {
                "loading_condition": "condition-B",
                "benchmark_revision": "revision-B",
            },
            [],
        ),
        ({"loading_condition": "condition-a"}, []),
        ({"benchmark_revision": "revision-a"}, []),
        ({"loading_condition": " condition-A "}, []),
        ({"benchmark_revision": " revision-A "}, []),
        ({"loading_condition": ""}, ["empty"]),
        ({"benchmark_revision": ""}, ["empty"]),
        (
            {"loading_condition": None, "benchmark_revision": None},
            ["empty", "ar", "b", "a"],
        ),
    ],
)
def test_lookup_selects_exact_condition_and_revision(
    registry, inputs, filters, expected_labels
):
    """Issue #2079: enumerate expected matches independently of the lookup.

    Register (A,A), (B,A), (A,B), then ('',''). A condition-A query must
    return (A,B),(A,A); a revision-A query (B,A),(A,A); their intersection
    only (A,A). Empty strings match only ('',''); None omits a filter.
    Lowercase or space-padded labels match nothing. (B,B) does not exist.
    Order reverses first registration order.
    All numeric fixture inputs are the synthetic issue reproduction's values.
    """
    refs = {
        "a": registry.register_rao(**inputs),
        "b": registry.register_rao(**(inputs | {"loading_condition": "condition-B"})),
        "ar": registry.register_rao(
            **(inputs | {"benchmark_revision": "revision-B"})
        ),
        "empty": registry.register_rao(
            **(inputs | {"loading_condition": "", "benchmark_revision": ""})
        ),
    }
    reopened = RaoRegistry(registry.index_path.parent)
    selected = reopened.get_raos(inputs["hull_id"], **filters)
    assert selected == [refs[label] for label in expected_labels]
    for ref in selected:
        assert reopened.load_rao_data(ref)["loading_condition"] == ref.loading_condition


def test_condition_filters_intersect_solver_and_draft(registry, inputs):
    """Independent set intersection: only the original record matches all fields.

    The other records differ in solver or have twice the issue fixture's draft:
    5 * 2 - 5 = 5 metres, outside the existing lookup rule documented in
    rao_registry.py. No new engineering acceptance limit is introduced.
    """
    first = registry.register_rao(**inputs)
    registry.register_rao(**(inputs | {"solver": "other-synthetic"}))
    registry.register_rao(**(inputs | {"draft_m": inputs["draft_m"] * 2}))
    assert registry.get_raos(
        inputs["hull_id"],
        inputs["solver"],
        inputs["draft_m"],
        loading_condition=inputs["loading_condition"],
        benchmark_revision=inputs["benchmark_revision"],
    ) == [first]


def test_filtered_history_preserves_corrections_and_bytes(registry, inputs):
    """Issue #2079: changed content retains both revisions in the same condition.

    The correction doubles the synthetic amplitude. Hand calculation: every
    element is 1 * 2 = 2; the original remains 1. Repeating the original must
    neither reorder history nor rewrite any stored bytes.
    """
    original = registry.register_rao(**inputs)
    corrected = registry.register_rao(
        **(inputs | {"amplitudes": inputs["amplitudes"] * 2})
    )
    registry.register_rao(**(inputs | {"loading_condition": "condition-B"}))
    before = {
        p: p.read_bytes() for p in registry.index_path.parent.rglob("*") if p.is_file()
    }
    assert registry.register_rao(**inputs) == original
    assert registry.get_raos(
        inputs["hull_id"],
        loading_condition=inputs["loading_condition"],
        benchmark_revision=inputs["benchmark_revision"],
    ) == [corrected, original]
    np.testing.assert_array_equal(
        registry.load_rao_data(corrected)["amplitudes"], inputs["amplitudes"] * 2
    )
    np.testing.assert_array_equal(
        registry.load_rao_data(original)["amplitudes"], inputs["amplitudes"]
    )
    assert {p: p.read_bytes() for p in before} == before


def test_filtered_legacy_lookup_does_not_imply_integrity(registry, inputs):
    """Issue #2079: matching legacy labels do not supply a missing content digest.

    Expected matches are enumerated: A selects only the legacy A entry, B only
    the newly registered B entry. Loading A must still fail explicitly.
    """
    legacy, path, original_bytes = _write_legacy_index(registry, inputs)
    reopened = RaoRegistry(registry.index_path.parent)
    fresh = reopened.register_rao(**(inputs | {"loading_condition": "condition-B"}))
    assert reopened.get_raos(
        inputs["hull_id"],
        loading_condition=inputs["loading_condition"],
        benchmark_revision=inputs["benchmark_revision"],
    ) == [legacy]
    assert reopened.get_raos(
        inputs["hull_id"],
        loading_condition="condition-B",
        benchmark_revision=inputs["benchmark_revision"],
    ) == [fresh]
    with pytest.raises(ValueError, match="integrity.*legacy"):
        reopened.load_rao_data(legacy)
    assert path.read_bytes() == original_bytes


@pytest.mark.parametrize("include_null_labels", [False, True])
def test_unlabelled_legacy_metadata_is_not_a_label_match(
    registry, inputs, include_null_labels
):
    """Enumerated legacy expectation: absent or null labels match no string.

    Both unlabelled forms remain visible with omitted filters or explicit None.
    Empty strings are labels, so neither unlabelled form may match them.
    """
    legacy, _, _ = _write_legacy_index(registry, inputs)
    index = yaml.safe_load(registry.index_path.read_text(encoding="utf-8"))
    entry, = index["hulls"][inputs["hull_id"]]
    for field in ("loading_condition", "benchmark_revision"):
        if include_null_labels:
            entry[field] = None
        else:
            del entry[field]
    registry.index_path.write_text(yaml.safe_dump(index), encoding="utf-8")
    reopened = RaoRegistry(registry.index_path.parent)
    expected = legacy.model_copy(
        update={"loading_condition": None, "benchmark_revision": None}
    )
    assert reopened.get_raos(inputs["hull_id"]) == [expected]
    assert reopened.get_raos(
        inputs["hull_id"], loading_condition=None, benchmark_revision=None
    ) == [expected]
    for field in ("loading_condition", "benchmark_revision"):
        assert reopened.get_raos(inputs["hull_id"], **{field: inputs[field]}) == []
        assert reopened.get_raos(inputs["hull_id"], **{field: ""}) == []
