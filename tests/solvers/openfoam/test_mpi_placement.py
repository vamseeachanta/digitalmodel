# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline placement contracts; all numbers below are synthetic inputs.

Hand basis: two ranks numbered 0 and 1 occupy CPUs {0, 2} and {1, 3},
on sockets {0} and {1}. Reordering those sets preserves placement; moving
rank 1 to socket 0 changes it. No physical acceptance limit is assumed.
The digest strings are SHA-256 of the literal fixture labels, not run data.
"""

import hashlib
import importlib.util
import json
from dataclasses import replace
from pathlib import Path
import sys
import tracemalloc

import pytest


PATH = (
    Path(__file__).resolve().parents[3]
    / "src/digitalmodel/solvers/openfoam/mpi_placement.py"
)
SPEC = importlib.util.spec_from_file_location("mpi_placement", PATH)
mpi = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = mpi
SPEC.loader.exec_module(mpi)


def digest(label):
    return hashlib.sha256(label.encode()).hexdigest()


def baseline(**changes):
    launcher = mpi.Launcher("openmpi", "mpirun", "fixture-version")
    receipt = mpi.PlacementReceipt(
        launcher=launcher,
        ranks=2,
        flags=("--map-by", "socket", "--bind-to", "core"),
        case_sha256=digest("mesh-and-dictionaries"),
        checkpoint_sha256=digest("checkpoint"),
        topology_sha256=digest("single-execution-domain"),
        observations=(
            mpi.RankPlacement(0, (0, 2), (0,)),
            mpi.RankPlacement(1, (1, 3), (1,)),
        ),
        validation_reference="synthetic-validation-evidence",
    )
    return replace(receipt, **changes)


def plan(receipt=None, **changes):
    receipt = receipt or baseline()
    request = dict(
        launcher=receipt.launcher,
        ranks=receipt.ranks,
        case_sha256=receipt.case_sha256,
        checkpoint_sha256=receipt.checkpoint_sha256,
        topology_sha256=receipt.topology_sha256,
    )
    request.update(changes)
    return mpi.plan_continuation(receipt, **request)


def test_omitted_flags_reuse_exact_order_and_bind_restart_receipt():
    receipt = baseline()
    result = plan(receipt)
    assert result.flags == receipt.flags
    assert result.source == "validated_baseline"
    assert (
        result.baseline_sha256 == hashlib.sha256(receipt.to_json().encode()).hexdigest()
    )
    assert result.checkpoint_sha256 == receipt.checkpoint_sha256
    assert result.override_reason is None


@pytest.mark.parametrize("flags", [(), ("--bind-to", "none")])
def test_explicit_override_including_empty_is_recorded(flags):
    result = plan(flags=flags, override_reason="deliberate comparison")
    assert result.flags == flags
    assert result.source == "explicit_override"
    assert result.override_reason == "deliberate comparison"


@pytest.mark.parametrize("reason", [None, "", "   "])
def test_override_requires_reason(reason):
    with pytest.raises(ValueError, match="reason"):
        plan(flags=(), override_reason=reason)


def test_reason_without_override_is_rejected():
    with pytest.raises(ValueError, match="override"):
        plan(override_reason="unused")


@pytest.mark.parametrize(
    "field,value",
    [
        ("case_sha256", digest("changed-mesh")),
        ("checkpoint_sha256", digest("unrelated-checkpoint")),
        ("topology_sha256", digest("different-execution-domain")),
        ("ranks", 3),
        ("launcher", mpi.Launcher("msmpi", "mpiexec", "fixture-version")),
        ("launcher", mpi.Launcher("openmpi", "mpirun", "different-version")),
    ],
)
def test_incompatible_restart_refuses_even_with_override(field, value):
    with pytest.raises(ValueError, match=field):
        plan(**{field: value}, flags=(), override_reason="cannot fix lineage")


@pytest.mark.parametrize(
    "changes",
    [
        {"validation_reference": None},
        {"observations": None},
        {"observations": (mpi.RankPlacement(0, (0,), None),)},
        {"observations": (mpi.RankPlacement(0, (0,), (0,)),)},
    ],
)
def test_unvalidated_or_incomplete_baseline_refuses(changes):
    with pytest.raises(ValueError, match="baseline"):
        plan(baseline(**changes))


def test_receipt_round_trip_and_digest_bind_all_evidence():
    receipt = baseline()
    assert mpi.PlacementReceipt.from_json(receipt.to_json()) == receipt
    assert (
        plan(receipt).baseline_sha256
        != plan(replace(receipt, validation_reference="other-evidence")).baseline_sha256
    )
    payload = json.loads(receipt.to_json())
    payload["unexpected"] = "must not be silently dropped"
    with pytest.raises((ValueError, TypeError)):
        mpi.PlacementReceipt.from_json(json.dumps(payload))


def test_msmpi_flags_stay_opaque_and_ordered():
    # Microsoft Learn mpiexec, /affinity and /affinity_layout options.
    flags = ("/affinity", "/affinity_layout", "spread:P")
    receipt = baseline(
        launcher=mpi.Launcher("msmpi", "mpiexec", "fixture"), flags=flags
    )
    assert plan(receipt).flags == flags


def test_effective_affinity_compares_sets_by_rank():
    receipt = baseline()
    observed = (mpi.RankPlacement(1, (3, 1), (1,)), mpi.RankPlacement(0, (2, 0), (0,)))
    result = mpi.check_effective_placement(receipt, observed, receipt.topology_sha256)
    assert result.status == "PASS"
    changed = (observed[0], mpi.RankPlacement(0, (2, 0), (1,)))
    assert (
        mpi.check_effective_placement(receipt, changed, receipt.topology_sha256).status
        == "FAIL"
    )


@pytest.mark.parametrize(
    "observed",
    [
        None,
        (),
        (mpi.RankPlacement(0, (0,), (0,)),),
        (mpi.RankPlacement(0, (0,), None), mpi.RankPlacement(1, (1,), (1,))),
    ],
)
def test_missing_topology_or_rank_is_not_evaluated(observed):
    receipt = baseline()
    assert (
        mpi.check_effective_placement(receipt, observed, receipt.topology_sha256).status
        == "NOT_EVALUATED"
    )


@pytest.mark.parametrize(
    "observed",
    [
        (),
        (mpi.RankPlacement(0, (0,), (0,)), mpi.RankPlacement(99_999, (1,), (1,))),
    ],
    ids=["empty", "sparse"],
)
def test_large_rank_count_with_incomplete_evidence_has_bounded_allocation(observed):
    """Sparse evidence must not allocate all 100,000 synthetic rank IDs.

    One MiB is a generous regression budget for these two tiny receipts,
    not a rank cap or an engineering acceptance limit.
    """
    tracemalloc.start()
    try:
        receipt = baseline(ranks=100_000, observations=observed)
        result = mpi.check_effective_placement(
            receipt, observed, receipt.topology_sha256
        )
        _, peak_bytes = tracemalloc.get_traced_memory()
    finally:
        tracemalloc.stop()
    assert result.status == "NOT_EVALUATED"
    assert peak_bytes < 1024 * 1024


def test_unknown_or_different_topology_cannot_pass():
    receipt = baseline()
    for topology in (None, digest("other-domain")):
        assert (
            mpi.check_effective_placement(
                receipt, receipt.observations, topology
            ).status
            == "NOT_EVALUATED"
        )


def test_receipt_can_record_unavailable_topology_but_cannot_reuse_it():
    receipt = baseline(topology_sha256=None)
    assert mpi.PlacementReceipt.from_json(receipt.to_json()) == receipt
    with pytest.raises(ValueError, match="topology"):
        plan(receipt)
    assert (
        mpi.check_effective_placement(receipt, receipt.observations, None).status
        == "NOT_EVALUATED"
    )


@pytest.mark.parametrize(
    "observed",
    [
        (mpi.RankPlacement(0, (0,), (0,)),) * 2,
        (mpi.RankPlacement(2, (0,), (0,)),),
    ],
)
def test_duplicate_or_out_of_range_ranks_reject(observed):
    with pytest.raises(ValueError, match="rank"):
        mpi.check_effective_placement(baseline(), observed, digest("topology"))


@pytest.mark.parametrize(
    "rank,cpus,sockets",
    [
        (True, (0,), (0,)),
        (-1, (0,), (0,)),
        (0, (), (0,)),
        (0, (True,), (0,)),
        (0, (-1,), (0,)),
        (0, (0, 0), (0,)),
        (0, (0,), ()),
        (0, (0,), (False,)),
    ],
)
def test_malformed_identifiers_reject(rank, cpus, sockets):
    with pytest.raises(ValueError):
        mpi.RankPlacement(rank, cpus, sockets)


@pytest.mark.parametrize(
    "changes",
    [
        {"ranks": True},
        {"ranks": 0},
        {"case_sha256": "unknown"},
        {"flags": ("",)},
        {"flags": ("bad\x00token",)},
        {"validation_reference": "   "},
    ],
)
def test_invalid_receipts_reject(changes):
    with pytest.raises(ValueError):
        baseline(**changes)


def test_timing_uses_only_bracketed_samples():
    """Hand calculation: (9 - 5)/(12 - 10) = 2 seconds/iteration.

    Then (12 - 9)/(13 - 12) = 3 seconds/iteration. Iteration 10 is an
    anchor, so no fabricated duration is attributed to its preceding work.
    """
    result = mpi.iteration_timing(((10, 5.0), (12, 9.0), (13, 12.0)))
    assert result.status == "PASS"
    assert result.intervals == ((10, 12, 2.0), (12, 13, 3.0))


def test_receipt_retains_timing_samples_and_binds_them_in_digest():
    """Synthetic elapsed change 9 - 5 = 4 seconds for 12 - 10 = 2 iterations."""
    receipt = baseline(timing_samples=((10, 5.0), (12, 9.0)))
    loaded = mpi.PlacementReceipt.from_json(receipt.to_json())
    assert loaded == receipt
    assert mpi.iteration_timing(loaded.timing_samples).intervals == ((10, 12, 2.0),)
    assert plan(loaded).baseline_sha256 != plan().baseline_sha256


def test_duplicate_receipt_fields_reject():
    text = baseline().to_json().replace('"ranks":2', '"ranks":2,"ranks":2')
    with pytest.raises(ValueError, match="duplicate"):
        mpi.PlacementReceipt.from_json(text)


@pytest.mark.parametrize("samples", [None, (), ((10, 5.0),)])
def test_missing_timing_is_not_evaluated(samples):
    assert mpi.iteration_timing(samples).status == "NOT_EVALUATED"


@pytest.mark.parametrize(
    "samples",
    [
        ((0, 5.0), (1, 4.0)),
        ((0, 5.0), (0, 6.0)),
        ((0, float("nan")), (1, 6.0)),
        ((0, -1.0), (1, 6.0)),
        ((True, 0.0), (1, 1.0)),
        ((0, 0.0), (1, float("inf"))),
    ],
)
def test_malformed_timing_rejects(samples):
    with pytest.raises(ValueError):
        mpi.iteration_timing(samples)


@pytest.mark.parametrize(
    "samples",
    [
        False,
        "",
        [],
        ((0,),),
        ((0, 10**1000), (1, 1.0)),
        ((0, 0.0), (10**1000, 1.0)),
    ],
)
def test_malformed_timing_container_or_oversized_number_rejects(samples):
    """A synthetic oversized integer exercises numeric error normalization."""
    with pytest.raises(ValueError):
        mpi.iteration_timing(samples)
