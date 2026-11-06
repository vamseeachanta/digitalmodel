# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Numeric-only collection intake for issue #1865.

Source: issue #1865, Handling and Minimum context; no physical constants or
empirical acceptance limits are supplied. Positive dimensions/rates and
nonnegative production/day are schema domains, not engineering thresholds.
Private originals, permissions and evidence must be retained by the custodian.
Pseudonymisation is not a publication approval or a proof of anonymity.
"""

from __future__ import annotations

import copy
import csv
import hashlib
import hmac
import io
import json
import math
from collections import Counter, defaultdict
from datetime import date

from .benchmark.test_set_builder import (
    LabelledSet,
    load_labelled_set,
    validate_card,
)
from .card_generators import ALL_GENERATORS
from .models import CardData

CONFIRMED_BASES = {"pulling_report", "workover_report", "independent_inspection"}


def _number(value: object, *, positive: bool = False) -> float:
    if type(value) not in (int, float) or not math.isfinite(value):
        raise ValueError("A finite numeric value is required")
    if value < 0 or (positive and value == 0):
        raise ValueError("A numeric value is outside its schema domain")
    return float(value)


def _text(value: object) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("A nonempty evidence or identity reference is required")
    return value


def _samples(record: dict) -> dict:
    try:
        # Repository validate_card defines the minimum paired-sample contract.
        # Reject strings and booleans before Pydantic can coerce them.
        for field in ("position", "load"):
            values = record[field]
            if not isinstance(values, list) or any(
                type(v) not in (int, float) or not math.isfinite(v) for v in values
            ):
                raise ValueError("Card samples must be finite numeric lists")
        card = CardData(position=record["position"], load=record["load"])
        validate_card(card)
        result = {"position": list(card.position), "load": list(card.load)}
        if record.get("time_s") is not None:
            times = record["time_s"]
            if not isinstance(times, list) or len(times) != len(card.position):
                raise ValueError("Time samples must pair with the card")
            times = [_number(v) for v in times]
            if any(b <= a for a, b in zip(times, times[1:])):
                raise ValueError("Time samples must increase strictly")
            result["time_s"] = times
        return result
    except (KeyError, TypeError, OverflowError):
        raise ValueError("Incomplete or invalid card samples") from None


def read_card_csv(text: str) -> dict:
    """Read elapsed seconds, inches and lbf; discard all other CSV columns.

    No unit conversion or inference is performed. Proprietary binary exports
    require an authorised external conversion; they are not parsed here.
    Exceptions never include a private cell, filename or identifier.
    """
    try:
        reader = csv.DictReader(io.StringIO(text.removeprefix("\ufeff")), strict=True)
        columns = {"time_s": "time_s", "position_in": "position", "load_lbf": "load"}
        if (
            reader.fieldnames is None
            or len(set(reader.fieldnames)) != len(reader.fieldnames)
            or not columns.keys() <= set(reader.fieldnames)
        ):
            raise ValueError("Required unique numeric CSV columns are missing")
        result = {key: [] for key in columns.values()}
        for row in reader:
            if None in row or any(row[col] is None for col in columns):
                raise ValueError("CSV row does not match its header")
            for source, target in columns.items():
                result[target].append(float(row[source]))
        return _samples(result)
    except (ValueError, TypeError, csv.Error):
        raise ValueError("Invalid timestamped card CSV") from None


def _context(record: dict) -> dict:
    """Project the issue's required context without copying text metadata."""
    rods = record["rod_string"]
    if not isinstance(rods, list) or not rods:
        raise ValueError("A nonempty rod string is required")
    result = {
        "rod_string": [
            {
                key: _number(rod[key], positive=True)
                for key in ("diameter_in", "length_ft")
            }
            for rod in rods
        ]
    }
    for key in ("pump_diameter_in", "pump_setting_depth_ft", "stroke_in", "spm"):
        result[key] = _number(record[key], positive=True)
    result["production_bpd"] = _number(record["production_bpd"])
    return result


def prepare_intake(records: list[dict], *, alias_key: bytes) -> IntakeBatch:
    """Validate all records before returning a projection with no raw identities.

    A custodian supplies a private, unpredictable key, retained outside public
    storage, at least 32 bytes long (RFC 2104, 1997, section 3).
    HMAC-SHA256 (section 2) namespaces wells, cards, events and evidence
    separately. The key and identity mapping are not stored.
    Confirmed review dates cannot follow the local intake date.
    Retain originals before intake; this function does not write or send data.
    Unknown input keys are discarded, including nested context metadata.
    """
    if (
        not isinstance(alias_key, bytes)
        or len(alias_key) < hashlib.sha256().digest_size
    ):
        raise ValueError(
            "A private alias key of at least 32 bytes is required "
            "(RFC 2104, section 3)"
        )
    intake_date = date.today()

    def alias(namespace: str, value: object) -> str:
        message = (namespace + "\0" + _text(value)).encode("utf-8")
        return (
            namespace + "-" + hmac.new(alias_key, message, hashlib.sha256).hexdigest()
        )

    projected, ids, geometries = [], set(), set()
    for record in records:
        try:
            kind, space = record["kind"], record["card_space"]
            if kind not in {"real", "synthetic", "plate"} or space not in {
                "surface",
                "pump",
            }:
                raise ValueError("Explicit card kind and space are required")
            acquisition = record["acquisition"]
            if acquisition not in {"recorded", "generated"}:
                raise ValueError(
                    "Only recorded or generated numeric arrays are supported"
                )
            if kind == "real" and acquisition != "recorded":
                raise ValueError("Real cards require recorded acquisition")
            label, rights = record["label"], record["rights"]
            mode, basis = label["mode"], label["basis"]
            if mode is not None and mode not in ALL_GENERATORS:
                raise ValueError("Condition must be a current mode or null")
            if basis not in CONFIRMED_BASES | {"unconfirmed"}:
                raise ValueError("Label basis is not supported")
            if basis == "unconfirmed" and mode is not None:
                raise ValueError("Unconfirmed labels must have a null mode")
            evidence = {}
            if basis in CONFIRMED_BASES:
                reviewed_on = date.fromisoformat(label["reviewed_on"])
                if reviewed_on > intake_date:
                    raise ValueError("A review date cannot follow the intake date")
                evidence = {
                    "evidence_ref": alias("evidence", label["evidence_ref"]),
                    "reviewer_ref": alias("reviewer", label["reviewer_ref"]),
                    "reviewed_on": reviewed_on.isoformat(),
                }
            for flag in (
                rights["analysis"],
                rights["publication"],
                record["original_retained"],
            ):
                if type(flag) is not bool:
                    raise ValueError(
                        "Permissions and retention require explicit booleans"
                    )
            samples = _samples(record)
            card_id = alias("card", record["card_id"])
            # Exact geometry duplicates cannot add independent evidence, even
            # under different well IDs, sampling times or labels.
            geometry = json.dumps(
                [[v if v else 0.0 for v in samples[k]] for k in ("position", "load")],
                allow_nan=False,
            )
            if card_id in ids or geometry in geometries:
                raise ValueError("Duplicate card identity or exact sample geometry")
            ids.add(card_id)
            geometries.add(geometry)
            stage = record.get("stage", "standalone")
            if stage not in {"standalone", "before", "after"}:
                raise ValueError("Card stage is not supported")
            event = (
                None if stage == "standalone" else alias("event", record["event_id"])
            )
            projected.append(
                {
                    "card_id": card_id,
                    "well_id": alias("well", record["well_id"]),
                    "kind": kind,
                    "acquisition": acquisition,
                    "card_space": space,
                    **samples,
                    "relative_day": _number(record["relative_day"]),
                    "units": {"position": "in", "load": "lbf", "time": "s"},
                    "context": _context(record["context"]),
                    "label": {"mode": mode, "basis": basis, **evidence},
                    "rights": {
                        "analysis": rights["analysis"],
                        "publication": rights["publication"],
                        "evidence_ref": alias("rights", rights["evidence_ref"]),
                    },
                    "original_retained": record["original_retained"],
                    "stage": stage,
                    "event_id": event,
                }
            )
        except (KeyError, TypeError, ValueError, OverflowError):
            # Avoid private values and chained exceptions in intake logs.
            raise ValueError(
                "Invalid intake record; check the documented schema"
            ) from None
    result = IntakeBatch(projected)
    result._pairs()  # Validate pair consistency before returning any projection.
    return result


class IntakeBatch:
    """Prepared collection; copies are returned so callers cannot mutate it."""

    def __init__(self, records: list[dict]) -> None:
        """Use prepare_intake to construct a validated collection."""
        self._records = copy.deepcopy(records)

    @staticmethod
    def _eligible(record: dict) -> bool:
        return (
            record["kind"] == "real"
            and record["card_space"] == "pump"
            and record["label"]["mode"] is not None
            and record["label"]["basis"] in CONFIRMED_BASES
            and record["rights"]["analysis"]
            and record["original_retained"]
        )

    def card_ids(self) -> list[str]:
        """Return pseudonymous selection identifiers without releasing samples."""
        return [r["card_id"] for r in self._records]

    def publication_document(self) -> dict:
        """Return a numeric projection only when publication grants are recorded.

        A separate human disclosure review is still required before release.
        Numerical geometry and context can remain indirectly identifying.
        """
        if any(
            not r["rights"]["publication"]
            or (r["kind"] == "real" and not r["original_retained"])
            for r in self._records
        ):
            raise ValueError("Publication permission or original retention is missing")
        return {"cards": copy.deepcopy(self._records)}

    def _pairs(self) -> list[dict]:
        pairs = defaultdict(dict)
        for r in self._records:
            if r["event_id"] is None:
                continue
            key = (r["well_id"], r["event_id"])
            if r["stage"] in pairs[key]:
                raise ValueError("An event has duplicate before or after cards")
            pairs[key][r["stage"]] = r
        complete = []
        for stages in pairs.values():
            if set(stages) != {"before", "after"}:
                continue
            before, after = stages["before"], stages["after"]
            if before["relative_day"] >= after["relative_day"]:
                raise ValueError("An after card must follow its before card")
            complete.append(stages)
        return complete

    def summary(self) -> dict:
        """Count real cards, wells and evidence coverage, never claim accuracy.

        Repeat counts include all days; no month-length cutoff is invented.
        A complete pair records chronology, not proof of a causal intervention.
        Synthetic and plate cards are reported separately from field evidence.
        """
        real = [r for r in self._records if r["kind"] == "real"]
        wells = Counter(r["well_id"] for r in real)
        eligible = [r for r in real if self._eligible(r)]
        return {
            "cards": len(real),
            "wells": len(wells),
            "non_real_cards": len(self._records) - len(real),
            "repeat_wells": sum(n > 1 for n in wells.values()),
            "complete_pairs": sum(
                all(r["kind"] == "real" for r in p.values()) for p in self._pairs()
            ),
            "healthy_wells": len(
                {r["well_id"] for r in eligible if r["label"]["mode"] == "NORMAL"}
            ),
            "eligible_cards": len(eligible),
            "eligible_wells": len({r["well_id"] for r in eligible}),
            "per_mode": dict(Counter(r["label"]["mode"] for r in eligible)),
            "benchmark_status": "NOT_EVALUATED",
            "reason": "Select one eligible pump card per held-out well and run BenchmarkRunner",
        }

    def benchmark_set(self, card_ids: list[str]) -> LabelledSet:
        """Build an analysis-only set with one explicitly chosen card per well.

        The caller must also establish separation from training wells and set
        the confidence and minimum per-class count in BenchmarkRunner. This
        guard only prevents within-batch repetition from inflating the score.
        Publication permission is separate from private analysis permission.
        """
        if not card_ids or len(set(card_ids)) != len(card_ids):
            raise ValueError("Select unique card identifiers explicitly")
        records = {r["card_id"]: r for r in self._records}
        selected, wells = [], set()
        for card_id in card_ids:
            r = records.get(card_id)
            if r is None or not self._eligible(r) or r["well_id"] in wells:
                raise ValueError(
                    "Selection requires eligible pump cards from distinct wells"
                )
            wells.add(r["well_id"])
            selected.append(r)
        modes = sorted({r["label"]["mode"] for r in selected})
        return load_labelled_set(
            {
                "card_space": "pump",
                "units": {"position": "in", "load": "lbf"},
                "crosswalk": [
                    {
                        "external_label": m,
                        "mode": m,
                        "reason": "Explicit intake review of independent outcome evidence",
                    }
                    for m in modes
                ],
                "cards": [
                    {
                        "position": r["position"],
                        "load": r["load"],
                        "label": r["label"]["mode"],
                        "provenance": {
                            "source": r["label"]["evidence_ref"],
                            "labeller": r["label"]["reviewer_ref"],
                            "date": r["label"]["reviewed_on"],
                            "licence": r["rights"]["evidence_ref"],
                            "kind": "real",
                            "acquisition": "recorded",
                        },
                    }
                    for r in selected
                ],
            }
        )
