"""Solver-free effective-control examples for OpenCFD v2312 (2023).

All numbers below are synthetic inputs, not recommended CFD limits. The hand
calculation for a control-only edit is simple equality: changing controlDict
maxCo from 0.8 to 0.4 leaves PIMPLE/maxCo = 0.8 unchanged. The active value is
therefore still 0.8. Source: VoF/setRDeltaT.H, v2312, lines 4-14.
"""

from __future__ import annotations

import importlib.util
import json
import sys
from pathlib import Path

import pytest

# Load the standalone evidence API without the unrelated numerical package
# imports in openfoam.__init__; this suite needs only the standard library.
_PATH = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/solvers/openfoam"
    / "effective_time_controls.py"
)
_SPEC = importlib.util.spec_from_file_location("effective_time_controls", _PATH)
controls = importlib.util.module_from_spec(_SPEC)
sys.modules[_SPEC.name] = controls
_SPEC.loader.exec_module(controls)

CD = "system/controlDict"
FV = "system/fvSolution"
SC = "system/fvSchemes"
LOCAL = f"{FV}/PIMPLE"
IDENTITY = controls.SolverIdentity("OpenCFD", "v2312", "interFoam")


def snapshot(
    *,
    scheme="localEuler",
    adjust="no",
    overrides=None,
    identity=IDENTITY,
    dictionaries=None,
    input_files=None,
    restart_files=None,
):
    """Synthetic explicit settings; no solver-default values are assumed."""
    readback = {
        f"{CD}/application": "interFoam",
        f"{SC}/ddtSchemes/default": scheme,
        f"{CD}/adjustTimeStep": adjust,
        f"{CD}/maxCo": "0.8",
        f"{CD}/maxAlphaCo": "0.2",
        f"{CD}/maxDeltaT": "0.1",
        f"{CD}/deltaT": "0.01",
        f"{LOCAL}/maxCo": "0.8",
        f"{LOCAL}/maxAlphaCo": "0.2",
        f"{LOCAL}/maxDeltaT": "0.1",
    }
    readback.update(overrides or {})
    readback = {key: value for key, value in readback.items() if value is not None}
    raw = {
        CD: 'application interFoam;\n#include "timeSettings"\n',
        FV: "PIMPLE { maxCo 0.8; maxAlphaCo 0.2; maxDeltaT 0.1; }\n",
        SC: f"ddtSchemes {{ default {scheme}; }}\n",
        "system/timeSettings": "maxCo 0.8;\n",
    }
    raw.update(dictionaries or {})
    return controls.capture_snapshot(
        identity=identity,
        dictionaries=raw,
        resolved=readback,
        input_files={"constant/g": b"synthetic input"}
        if input_files is None
        else input_files,
        restart_files={"0/U": b"synthetic restart"}
        if restart_files is None
        else restart_files,
    )


def test_local_euler_uses_pimple_even_with_adjust_disabled():
    result = controls.resolve_time_controls(snapshot())
    assert result["maxCo"].path == f"{LOCAL}/maxCo"
    assert result["maxCo"].value == 0.8
    assert result["maxAlphaCo"].status == "ACTIVE"
    assert result["deltaT"].status == "NOT_EVALUATED"


@pytest.mark.parametrize("key", ["maxCo", "maxAlphaCo", "maxDeltaT"])
def test_global_euler_fixed_step_leaves_caps_inactive(key):
    """VoF/setDeltaT.H v2312 lines 36-53 skips all caps when adjustment is off."""
    result = controls.resolve_time_controls(snapshot(scheme="Euler"))
    assert result[key].status == "INACTIVE"
    assert result["deltaT"].path == f"{CD}/deltaT"
    assert result["deltaT"].value == 0.01


@pytest.mark.parametrize("key", ["maxCo", "maxAlphaCo", "maxDeltaT"])
def test_global_adaptive_step_uses_control_dict(key):
    result = controls.resolve_time_controls(snapshot(scheme="Euler", adjust="yes"))
    assert result[key].path == f"{CD}/{key}"
    assert result[key].status == "ACTIVE"


@pytest.mark.parametrize("alpha", ["0.8", "1.0"])
def test_local_alpha_cap_is_inactive_unless_less_than_max_co(alpha):
    """v2312 VoF/setRDeltaT.H line 66 requires maxAlphaCo < maxCo."""
    result = controls.resolve_time_controls(
        snapshot(overrides={f"{LOCAL}/maxAlphaCo": alpha})
    )
    assert result["maxAlphaCo"].status == "INACTIVE"


@pytest.mark.parametrize("bad", [None, "nan", "inf", "0", "-1", "yes", "1e999"])
def test_missing_or_invalid_active_value_is_not_evaluated(bad):
    result = controls.resolve_time_controls(snapshot(overrides={f"{LOCAL}/maxCo": bad}))
    assert result["maxCo"].status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "identity",
    [
        controls.SolverIdentity("Foundation", "v2312", "interFoam"),
        controls.SolverIdentity("OpenCFD", "v2406", "interFoam"),
        controls.SolverIdentity("OpenCFD", "v2312", "pimpleFoam"),
    ],
)
def test_unknown_solver_contract_is_not_evaluated(identity):
    result = controls.resolve_time_controls(snapshot(identity=identity))
    assert all(item.status == "NOT_EVALUATED" for item in result.values())


def test_unknown_scheme_and_omitted_switch_are_not_evaluated():
    for snap in (
        snapshot(scheme="backward"),
        snapshot(scheme="Euler", overrides={f"{CD}/adjustTimeStep": None}),
    ):
        assert controls.resolve_time_controls(snap)["maxCo"].status == "NOT_EVALUATED"


def test_snapshot_preserves_raw_and_resolved_dictionaries_independently():
    snap = snapshot()
    record = snap.as_dict()
    assert record["dictionaries"][CD]["text"].startswith("application interFoam;")
    assert record["resolved"][f"{LOCAL}/maxCo"] == "0.8"
    with pytest.raises(TypeError):
        snap.resolved[f"{LOCAL}/maxCo"] = "0.4"
    assert json.loads(json.dumps(record)) == record


def verify(before, after, edits, **kwargs):
    return controls.verify_experiment(
        requested_identity=IDENTITY,
        before=before,
        after=after,
        edits=edits,
        **kwargs,
    )


def check(receipt, name):
    return next(item for item in receipt["checks"] if item["name"] == name)


def test_local_control_dict_edit_is_rejected():
    """0.8 -> 0.4 in controlDict leaves the consumed PIMPLE value at 0.8."""
    before = snapshot()
    after = snapshot(overrides={f"{CD}/maxCo": "0.4"})
    receipt = verify(before, after, {f"{CD}/maxCo": 0.4})
    assert receipt["status"] == "REJECTED"
    assert check(receipt, f"target:{CD}/maxCo")["reason"] == "inactive_target_path"


def test_fixed_global_courant_edit_is_rejected():
    before = snapshot(scheme="Euler")
    after = snapshot(scheme="Euler", overrides={f"{CD}/maxCo": "0.4"})
    receipt = verify(before, after, {f"{CD}/maxCo": 0.4})
    assert receipt["status"] == "REJECTED"
    assert check(receipt, f"target:{CD}/maxCo")["reason"] == "inactive_target"


def test_included_fragment_edit_does_not_override_resolved_parent():
    """Expanded parent remains 0.8; former fragment changes to 0.4: no effect."""
    before = snapshot()
    after = snapshot(dictionaries={"system/timeSettings": "maxCo 0.4;\n"})
    receipt = verify(before, after, {f"{LOCAL}/maxCo": 0.4})
    assert receipt["status"] == "REJECTED"
    assert check(receipt, f"target:{LOCAL}/maxCo")["reason"] == "readback_mismatch"
    assert (
        receipt["after"]["dictionaries"]["system/timeSettings"]["text"]
        == "maxCo 0.4;\n"
    )


def test_effective_edit_without_measured_response_is_not_executed():
    before = snapshot()
    after = snapshot(overrides={f"{LOCAL}/maxCo": "0.4"})
    receipt = verify(before, after, {f"{LOCAL}/maxCo": 0.4})
    assert check(receipt, f"target:{LOCAL}/maxCo")["status"] == "PASS"
    assert receipt["status"] == "NOT_EVALUATED"
    assert receipt["executed"] is False


@pytest.mark.parametrize(
    "value,reason", [(0.8, "unchanged_active_value"), (0.4, "readback_mismatch")]
)
def test_noop_and_wrong_actual_value_are_rejected(value, reason):
    receipt = verify(snapshot(), snapshot(), {f"{LOCAL}/maxCo": value})
    assert receipt["status"] == "REJECTED"
    assert check(receipt, f"target:{LOCAL}/maxCo")["reason"] == reason


def test_requested_actual_version_mismatch_is_rejected():
    after = snapshot(identity=controls.SolverIdentity("OpenCFD", "v2406", "interFoam"))
    receipt = verify(snapshot(), after, {f"{LOCAL}/maxCo": 0.4})
    assert check(receipt, "solver_identity")["status"] == "FAIL"
    assert receipt["status"] == "REJECTED"


def test_application_readback_mismatch_is_rejected():
    after = snapshot(overrides={f"{CD}/application": "pimpleFoam"})
    receipt = verify(snapshot(), after, {f"{LOCAL}/maxCo": 0.4})
    assert check(receipt, "solver_identity")["status"] == "FAIL"


def test_time_scheme_change_and_unrequested_changes_are_rejected():
    for after in (
        snapshot(scheme="Euler"),
        snapshot(overrides={f"{LOCAL}/maxCo": "0.4", f"{LOCAL}/maxDeltaT": "0.2"}),
    ):
        receipt = verify(snapshot(), after, {f"{LOCAL}/maxCo": 0.4})
        assert receipt["status"] == "REJECTED"


def test_empty_edits_and_unsupported_controls_are_not_evaluated():
    for edits in ({}, {f"{LOCAL}/rDeltaTDampingCoeff": 0.4}):
        receipt = verify(snapshot(), snapshot(), edits)
        assert receipt["executed"] is False
        assert receipt["status"] == "NOT_EVALUATED"


def test_unknown_active_value_and_missing_parent_dictionary_are_not_evaluated():
    before = snapshot()
    after = snapshot(overrides={f"{LOCAL}/maxCo": None})
    receipt = verify(before, after, {f"{LOCAL}/maxCo": 0.4})
    assert check(receipt, f"target:{LOCAL}/maxCo")["status"] == "NOT_EVALUATED"
    assert receipt["executed"] is False
    missing = controls.capture_snapshot(
        identity=IDENTITY,
        dictionaries={},
        resolved=before.resolved,
        input_files={"constant/g": b"input"},
        restart_files={"0/U": b"restart"},
    )
    receipt = verify(missing, before, {f"{LOCAL}/maxCo": 0.8})
    assert check(receipt, "dictionaries")["status"] == "NOT_EVALUATED"


def response(before, after, **overrides):
    """Synthetic observations: max(|20-10|, |40-20|) = 20 reciprocal seconds.

    A caller-specified threshold of 1 reciprocal second is exceeded. This is
    arithmetic on observations, not a prediction of the solver response.
    VoF/setRDeltaT.H v2312 (2023), lines 58-88 defines reciprocal local time.
    """
    data = dict(
        metric="rDeltaT",
        before_values=(10.0, 20.0),
        after_values=(20.0, 40.0),
        before_snapshot_sha256=before.sha256(),
        after_snapshot_sha256=after.sha256(),
        minimum_change=1.0,
        criterion_source="Synthetic hand calculation in test docstring",
    )
    data.update(overrides)
    return controls.TimeStepResponse(**data)


def effective_pair(**kwargs):
    return snapshot(**kwargs), snapshot(overrides={f"{LOCAL}/maxCo": "0.4"}, **kwargs)


def test_genuine_local_change_is_executed_with_bound_response_and_provenance():
    before, after = effective_pair()
    receipt = verify(
        before, after, {f"{LOCAL}/maxCo": 0.4}, response=response(before, after)
    )
    assert receipt["status"] == "EXECUTED"
    assert receipt["executed"] is True
    assert receipt["response"]["maximum_absolute_change"] == 20.0
    assert receipt["requested_edits"] == {f"{LOCAL}/maxCo": 0.4}
    assert receipt["before"]["input_hashes"] == receipt["after"]["input_hashes"]
    assert json.loads(json.dumps(receipt, allow_nan=False)) == receipt


@pytest.mark.parametrize("adjust,key", [("no", "deltaT"), ("yes", "maxCo")])
def test_genuine_global_change_uses_delta_t_response(adjust, key):
    """Hand calculation: |0.005 - 0.01| = 0.005 s > caller threshold 0.001 s."""
    value = 0.005 if key == "deltaT" else 0.4
    before = snapshot(scheme="Euler", adjust=adjust)
    after = snapshot(
        scheme="Euler", adjust=adjust, overrides={f"{CD}/{key}": str(value)}
    )
    measured = response(
        before,
        after,
        metric="deltaT",
        before_values=(0.01,),
        after_values=(0.005,),
        minimum_change=0.001,
    )
    receipt = verify(before, after, {f"{CD}/{key}": value}, response=measured)
    assert receipt["status"] == "EXECUTED"
    assert receipt["response"]["maximum_absolute_change"] == 0.005


@pytest.mark.parametrize("scheme,origin", [("localEuler", LOCAL), ("Euler", CD)])
@pytest.mark.parametrize("key", ["maxCo", "maxAlphaCo", "maxDeltaT"])
@pytest.mark.parametrize("bad", [None, "nan", "inf", "0", "-1", "yes", "1e999"])
def test_incomplete_untargeted_active_readbacks_prevent_execution(
    scheme, origin, key, bad
):
    """A passing target and bound response cannot qualify incomplete controls."""
    missing_path = f"{origin}/{key}"
    target = f"{origin}/{'maxDeltaT' if key == 'maxCo' else 'maxCo'}"
    overrides = {missing_path: bad}
    before = snapshot(scheme=scheme, adjust="yes", overrides=overrides)
    after = snapshot(
        scheme=scheme, adjust="yes", overrides={**overrides, target: "0.4"}
    )
    receipt = verify(
        before,
        after,
        {target: 0.4},
        response=response(
            before, after, metric="rDeltaT" if scheme == "localEuler" else "deltaT"
        ),
    )
    assert check(receipt, f"target:{target}")["status"] == "PASS"
    assert check(receipt, "response")["status"] == "PASS"
    assert receipt["status"] == "NOT_EVALUATED"
    assert receipt["executed"] is False
    for phase in ("before", "after"):
        assert receipt[f"effective_{phase}"][key]["status"] == "NOT_EVALUATED"
        assert (
            check(receipt, f"active_control:{phase}:{key}")["status"] == "NOT_EVALUATED"
        )
    assert not any(item["status"] == "FAIL" for item in receipt["checks"])


@pytest.mark.parametrize("scheme,origin", [("localEuler", LOCAL), ("Euler", CD)])
def test_rejection_takes_precedence_over_incomplete_active_readbacks(scheme, origin):
    target = f"{origin}/maxCo"
    before = snapshot(
        scheme=scheme, adjust="yes", overrides={f"{origin}/maxDeltaT": None}
    )
    after = snapshot(
        scheme=scheme,
        adjust="yes",
        overrides={f"{origin}/maxDeltaT": None, target: "0.4"},
    )
    receipt = verify(
        before,
        after,
        {target: 0.4},
        response=response(before, after, before_snapshot_sha256="stale"),
    )
    assert check(receipt, "response")["reason"] == "response_snapshot_mismatch"
    assert receipt["status"] == "REJECTED"
    assert receipt["executed"] is False


@pytest.mark.parametrize("scheme,origin", [("localEuler", LOCAL), ("Euler", CD)])
def test_unsupported_delta_t_readback_does_not_block_supported_cap_edit(scheme, origin):
    target = f"{origin}/maxCo"
    before = snapshot(scheme=scheme, adjust="yes", overrides={f"{CD}/deltaT": None})
    after = snapshot(
        scheme=scheme,
        adjust="yes",
        overrides={f"{CD}/deltaT": None, target: "0.4"},
    )
    receipt = verify(
        before,
        after,
        {target: 0.4},
        response=response(
            before, after, metric="rDeltaT" if scheme == "localEuler" else "deltaT"
        ),
    )
    assert receipt["effective_before"]["deltaT"]["status"] == "NOT_EVALUATED"
    assert receipt["effective_after"]["deltaT"]["status"] == "NOT_EVALUATED"
    assert receipt["status"] == "EXECUTED"
    assert receipt["executed"] is True


def test_inactive_local_interface_cap_does_not_block_execution():
    before = snapshot(overrides={f"{LOCAL}/maxAlphaCo": "1.0"})
    after = snapshot(overrides={f"{LOCAL}/maxAlphaCo": "1.0", f"{LOCAL}/maxCo": "0.4"})
    receipt = verify(
        before, after, {f"{LOCAL}/maxCo": 0.4}, response=response(before, after)
    )
    assert receipt["effective_before"]["maxAlphaCo"]["status"] == "INACTIVE"
    assert receipt["effective_after"]["maxAlphaCo"]["status"] == "INACTIVE"
    assert receipt["status"] == "EXECUTED"
    assert receipt["executed"] is True


@pytest.mark.parametrize("bad", [None, "nan", "0", "-1"])
def test_incomplete_inactive_fixed_step_caps_do_not_block_execution(bad):
    overrides = {f"{CD}/{key}": bad for key in ("maxCo", "maxAlphaCo", "maxDeltaT")}
    before = snapshot(scheme="Euler", overrides=overrides)
    after = snapshot(scheme="Euler", overrides={**overrides, f"{CD}/deltaT": "0.005"})
    receipt = verify(
        before,
        after,
        {f"{CD}/deltaT": 0.005},
        response=response(before, after, metric="deltaT"),
    )
    assert receipt["status"] == "EXECUTED"
    assert receipt["executed"] is True


@pytest.mark.parametrize(
    "overrides,reason",
    [
        ({"after_values": (10.0, 20.0)}, "response_change_not_above_threshold"),
        ({"minimum_change": 20.0}, "response_change_not_above_threshold"),
        ({"metric": "deltaT"}, "wrong_time_step_metric"),
        ({"before_snapshot_sha256": "stale"}, "response_snapshot_mismatch"),
        ({"after_values": ()}, "invalid_response_samples"),
        ({"after_values": (20.0,)}, "invalid_response_samples"),
        ({"after_values": (float("nan"), 40.0)}, "invalid_response_samples"),
        ({"after_values": (float("inf"), 40.0)}, "invalid_response_samples"),
        ({"after_values": (0.0, 40.0)}, "invalid_response_samples"),
        ({"after_values": (-1.0, 40.0)}, "invalid_response_samples"),
        ({"after_values": (True, 40.0)}, "invalid_response_samples"),
    ],
)
def test_inadequate_response_never_establishes_execution(overrides, reason):
    """A strict threshold excludes equality; malformed observations fail closed."""
    before, after = effective_pair()
    receipt = verify(
        before,
        after,
        {f"{LOCAL}/maxCo": 0.4},
        response=response(before, after, **overrides),
    )
    assert receipt["status"] == "REJECTED"
    assert check(receipt, "response")["reason"] == reason
    # Invalid raw numbers must not leak into a non-standard JSON receipt.
    json.dumps(receipt, allow_nan=False)


@pytest.mark.parametrize(
    "overrides",
    [
        {"criterion_source": ""},
        {"minimum_change": float("nan")},
        {"minimum_change": -1.0},
        {"minimum_change": True},
    ],
)
def test_response_requires_an_explicit_finite_criterion(overrides):
    before, after = effective_pair()
    receipt = verify(
        before,
        after,
        {f"{LOCAL}/maxCo": 0.4},
        response=response(before, after, **overrides),
    )
    assert receipt["status"] == "NOT_EVALUATED"
    assert check(receipt, "response")["status"] == "NOT_EVALUATED"
    json.dumps(receipt, allow_nan=False)


@pytest.mark.parametrize("field", ["input_files", "restart_files"])
def test_missing_provenance_is_not_evaluated(field):
    before, after = effective_pair(**{field: {}})
    receipt = verify(
        before, after, {f"{LOCAL}/maxCo": 0.4}, response=response(before, after)
    )
    assert receipt["status"] == "NOT_EVALUATED"
    assert (
        check(receipt, field.replace("_files", "_hashes"))["status"] == "NOT_EVALUATED"
    )


@pytest.mark.parametrize("field", ["input_files", "restart_files"])
def test_changed_trial_inputs_or_restart_source_are_rejected(field):
    before = snapshot()
    after = snapshot(
        overrides={f"{LOCAL}/maxCo": "0.4"},
        **{field: {"constant/changed": b"changed content"}},
    )
    receipt = verify(
        before, after, {f"{LOCAL}/maxCo": 0.4}, response=response(before, after)
    )
    assert receipt["status"] == "REJECTED"
    assert check(receipt, field.replace("_files", "_hashes"))["status"] == "FAIL"


def test_dictionary_and_input_hashes_use_exact_captured_bytes():
    import hashlib

    before = snapshot()
    record = before.as_dict()
    assert (
        record["dictionaries"][CD]["sha256"]
        == hashlib.sha256(
            record["dictionaries"][CD]["text"].encode("utf-8")
        ).hexdigest()
    )
    assert (
        record["input_hashes"]["constant/g"]
        == hashlib.sha256(b"synthetic input").hexdigest()
    )
    assert (
        record["restart_hashes"]["0/U"]
        == hashlib.sha256(b"synthetic restart").hexdigest()
    )
    assert (
        before.sha256()
        == hashlib.sha256(
            json.dumps(
                record,
                sort_keys=True,
                separators=(",", ":"),
                ensure_ascii=False,
                allow_nan=False,
            ).encode("utf-8")
        ).hexdigest()
    )


def test_snapshot_copies_caller_mappings_and_response_copies_samples():
    raw = {CD: "application interFoam;"}
    resolved = {f"{CD}/application": "interFoam"}
    snap = controls.capture_snapshot(
        identity=IDENTITY,
        dictionaries=raw,
        resolved=resolved,
        input_files={},
        restart_files={},
    )
    raw[CD] = "changed"
    resolved[f"{CD}/application"] = "changed"
    assert snap.dictionaries[CD] == "application interFoam;"
    assert snap.resolved[f"{CD}/application"] == "interFoam"
    values = [20.0, 40.0]
    measured = response(snap, snap, after_values=values)
    values[0] = 100.0
    assert measured.after_values == (20.0, 40.0)


def test_multiple_targets_all_have_to_be_effective():
    before, after = effective_pair()
    receipt = verify(
        before,
        after,
        {f"{LOCAL}/maxCo": 0.4, f"{CD}/maxCo": 0.8},
        response=response(before, after),
    )
    assert receipt["status"] == "REJECTED"


@pytest.mark.parametrize("path", ["../input", "constant/../input", "", "C:\\input"])
def test_capture_rejects_non_relative_evidence_paths(path):
    with pytest.raises(ValueError, match="relative paths"):
        controls.capture_snapshot(
            identity=IDENTITY,
            dictionaries={},
            resolved={},
            input_files={path: b"input"},
            restart_files={},
        )


def test_oversized_response_criterion_is_not_evaluated():
    """A synthetic integer outside float range is invalid evidence, not a crash."""
    before, after = effective_pair()
    receipt = verify(
        before,
        after,
        {f"{LOCAL}/maxCo": 0.4},
        response=response(before, after, minimum_change=10**1000),
    )
    assert receipt["status"] == "NOT_EVALUATED"


def test_public_snapshot_constructor_also_copies_mappings():
    original = snapshot()
    resolved = dict(original.resolved)
    copied = controls.SettingsSnapshot(
        original.identity,
        original.dictionaries,
        resolved,
        original.input_hashes,
        original.restart_hashes,
    )
    resolved[f"{LOCAL}/maxCo"] = "0.4"
    assert copied.resolved[f"{LOCAL}/maxCo"] == "0.8"


def test_capture_rejects_runtime_absolute_paths_and_drive_paths(tmp_path):
    for path in (str(tmp_path / "input"), "C:/input"):
        with pytest.raises(ValueError, match="relative paths"):
            controls.capture_snapshot(
                identity=IDENTITY,
                dictionaries={},
                resolved={},
                input_files={path: b"input"},
                restart_files={},
            )


def test_same_restart_path_with_different_bytes_is_rejected():
    before = snapshot()
    after = snapshot(
        overrides={f"{LOCAL}/maxCo": "0.4"}, restart_files={"0/U": b"changed restart"}
    )
    receipt = verify(
        before, after, {f"{LOCAL}/maxCo": 0.4}, response=response(before, after)
    )
    assert check(receipt, "restart_hashes")["status"] == "FAIL"


def test_snapshot_digest_is_independent_of_mapping_order():
    before = snapshot()
    reordered = controls.SettingsSnapshot(
        before.identity,
        dict(reversed(list(before.dictionaries.items()))),
        dict(reversed(list(before.resolved.items()))),
        before.input_hashes,
        before.restart_hashes,
    )
    assert before.sha256() == reordered.sha256()
