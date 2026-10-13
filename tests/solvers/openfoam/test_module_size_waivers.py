# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Dated module-size waivers on synthetic temporary files."""

from datetime import date

import pytest
import yaml

from tests.solvers.openfoam import test_module_size_limits as guard

RUN_DATE = date(2026, 10, 12)


@pytest.fixture
def module_tree(tmp_path):
    (tmp_path / "module.py").write_text("# line\n" * 410, encoding="utf-8")
    (tmp_path / "scripts/ci").mkdir(parents=True)
    return tmp_path


def write_waivers(root, entries):
    (root / "scripts/ci/module_size_waivers.yml").write_text(
        yaml.safe_dump(entries), encoding="utf-8"
    )


def entry(**changes):
    return {"path": "module.py", "ceiling": 410,
            "expires": "2026-10-13",
            "issue": 2052, **changes}


def load(root, entries, today=RUN_DATE, governed=("module.py",)):
    write_waivers(root, entries)
    from tests.solvers.openfoam.module_size_waivers import load_waivers

    return load_waivers(root, governed, guard.MAX_MODULE_LINES, today=today)


@pytest.mark.parametrize("lines", [399, 400])
def test_under_limit_regression_pin(module_tree, monkeypatch, lines):
    """Pin the original unlisted-module verdict at and below the limit."""
    (module_tree / "module.py").write_text("# line\n" * lines, encoding="utf-8")
    write_waivers(module_tree, [])
    monkeypatch.setattr(guard, "_REPO", module_tree)
    guard.test_module_is_within_the_line_limit("module.py")


def test_unlisted_over_limit_regression_pin(module_tree, monkeypatch):
    """Pin the original oversized-module failure and message prefix."""
    write_waivers(module_tree, [])
    monkeypatch.setattr(guard, "_REPO", module_tree)
    with pytest.raises(AssertionError, match="module.py is 410 lines"):
        guard.test_module_is_within_the_line_limit("module.py")


@pytest.mark.parametrize("lines", [410, 420])
def test_active_waiver_at_or_below_ceiling(module_tree, lines):
    (module_tree / "module.py").write_text("# line\n" * lines, encoding="utf-8")
    if lines == 410:
        with pytest.raises(
            ValueError, match=r"module.py.*stale ceiling; lower it to 410"
        ):
            load(module_tree, [entry(ceiling=420)])
    else:
        assert load(module_tree, [entry(ceiling=420)]) == {"module.py": 420}


def test_waiver_over_ceiling(module_tree):
    with pytest.raises(ValueError, match=r"entry 1.*module.py.*410.*ceiling 409"):
        load(module_tree, [entry(ceiling=409)])


@pytest.mark.parametrize("today", [date(2026, 10, 13), date(2026, 10, 14)])
def test_waiver_expired_on_or_after_date(module_tree, today):
    with pytest.raises(
        ValueError, match=r"entry 1.*module.py.*expired.*2026-10-13.*2052"
    ):
        load(module_tree, [entry()], today=today)


@pytest.mark.parametrize("lines", [399, 400])
def test_waiver_stale_at_or_below_limit(module_tree, lines):
    (module_tree / "module.py").write_text("# line\n" * lines, encoding="utf-8")
    with pytest.raises(ValueError, match=r"entry 1.*module.py.*stale"):
        load(module_tree, [entry()])


def test_waiver_missing_file_is_stale(module_tree):
    with pytest.raises(ValueError, match=r"entry 1.*missing.py.*stale.*missing"):
        load(module_tree, [entry(path="missing.py")], governed=("missing.py",))


def test_waiver_ungoverned_file(module_tree):
    with pytest.raises(ValueError, match=r"entry 1.*module.py.*not governed"):
        load(module_tree, [entry()], governed=())


def test_waiver_duplicate_path(module_tree):
    with pytest.raises(ValueError, match=r"entry 2.*module.py.*duplicate"):
        load(module_tree, [entry(), entry()])


@pytest.mark.parametrize("field", ["path", "ceiling", "expires", "issue"])
def test_waiver_requires_each_field(module_tree, field):
    record = entry()
    del record[field]
    with pytest.raises(ValueError, match=rf"entry 1.*{field}"):
        load(module_tree, [record])


@pytest.mark.parametrize("changes", [
    {"path": "../module.py"}, {"path": "./module.py"}, {"path": "module\\file.py"},
    {"ceiling": True}, {"ceiling": "420"}, {"ceiling": 0},
    {"expires": "2026-13-12"}, {"expires": "20261013"}, {"expires": False},
    {"issue": True}, {"issue": "2052"}, {"issue": 0},
])
def test_waiver_rejects_invalid_field(module_tree, changes):
    with pytest.raises(ValueError, match=r"entry 1"):
        load(module_tree, [entry(**changes)])


@pytest.mark.parametrize("entries", [None, {}, "invalid", ["invalid"]])
def test_waiver_rejects_invalid_schema(module_tree, entries):
    with pytest.raises(ValueError, match="waiver"):
        load(module_tree, entries)


def test_waiver_accepts_yaml_date(module_tree):
    (module_tree / "module.py").write_text("# line\n" * 420, encoding="utf-8")
    assert load(
        module_tree, [entry(ceiling=420, expires=date(2026, 10, 13))]
    ) == {"module.py": 420}


def test_all_entries_are_validated(module_tree):
    with pytest.raises(ValueError, match=r"entry 2.*missing.py.*stale"):
        load(module_tree, [entry(), entry(path="missing.py")],
             governed=("module.py", "missing.py"))


def test_guard_consults_active_waiver(module_tree, monkeypatch):
    from tests.solvers.openfoam.module_size_waivers import load_waivers

    write_waivers(module_tree, [entry()])
    monkeypatch.setattr(guard, "_REPO", module_tree)
    monkeypatch.setattr(guard, "GOVERNED_MODULES", ("module.py",))
    monkeypatch.setattr(guard, "load_waivers", lambda root, governed, limit:
                        load_waivers(root, governed, limit, today=RUN_DATE))
    guard.test_module_is_within_the_line_limit("module.py")


def test_missing_waiver_file_fails(module_tree):
    from tests.solvers.openfoam.module_size_waivers import load_waivers

    with pytest.raises(ValueError, match="waiver file.*missing"):
        load_waivers(
            module_tree, ("module.py",), guard.MAX_MODULE_LINES, today=RUN_DATE
        )


def test_malformed_yaml_fails(module_tree):
    (module_tree / "scripts/ci/module_size_waivers.yml").write_text(
        "[", encoding="utf-8"
    )
    from tests.solvers.openfoam.module_size_waivers import load_waivers

    with pytest.raises(ValueError, match="waiver file.*invalid YAML"):
        load_waivers(
            module_tree, ("module.py",), guard.MAX_MODULE_LINES, today=RUN_DATE
        )


def test_unknown_field_fails(module_tree):
    with pytest.raises(ValueError, match=r"entry 1.*unexpected"):
        load(module_tree, [entry(unexpected=1)])


def test_duplicate_yaml_field_fails(module_tree):
    (module_tree / "scripts/ci/module_size_waivers.yml").write_text(
        "- path: module.py\n  ceiling: 420\n  ceiling: 430\n"
        "  expires: 2026-10-13\n  issue: 2052\n", encoding="utf-8"
    )
    from tests.solvers.openfoam.module_size_waivers import load_waivers

    with pytest.raises(ValueError, match="waiver file.*duplicate field.*ceiling"):
        load_waivers(
            module_tree, ("module.py",), guard.MAX_MODULE_LINES, today=RUN_DATE
        )


def test_waiver_cannot_escape_root_through_symlink(module_tree):
    outside = module_tree.parent / "outside.py"
    outside.write_text("# line\n" * 410, encoding="utf-8")
    (module_tree / "link.py").symlink_to(outside)
    with pytest.raises(ValueError, match=r"entry 1.*link.py.*outside repository"):
        load(module_tree, [entry(path="link.py")], governed=("link.py",))


def test_waiver_one_line_under_ceiling(module_tree):
    with pytest.raises(ValueError, match=r"module.py.*stale ceiling; lower it to 410"):
        load(module_tree, [entry(ceiling=411)])


def test_committed_waiver_list_is_empty():
    from tests.solvers.openfoam.module_size_waivers import _read_entries

    assert _read_entries(guard._REPO) == []


def test_waiver_far_future_expiry_is_valid(module_tree):
    assert load(module_tree, [entry(expires="2099-10-13")]) == {"module.py": 410}


def test_waiver_added_field_is_rejected(module_tree):
    with pytest.raises(ValueError, match=r"unexpected fields.*added"):
        load(module_tree, [entry(added="2026-10-12")])
