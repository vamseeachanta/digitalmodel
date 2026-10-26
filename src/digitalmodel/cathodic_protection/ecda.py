# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Four-survey correlation requiring explicit criteria and basis references.

No standard compliance is asserted. Every threshold is an input: user to confirm
against the governing code. Correlation prioritizes assessment, not excavation
authorization or a remaining-strength calculation.
"""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any, Literal

import pandas as pd
from pydantic import BaseModel, Field, model_validator

from digitalmodel.cathodic_protection.cp_survey import (
    CRITERIA_NOTE,
    SEVERITIES,
    ACVGBands,
    ACVGIndication,
    CISSurveyPoint,
    CriteriaProvenance,
    DCVGIndication,
    PCMConfig,
    PCMSurveyPoint,
    SurveyInput,
    analyze_pcm_survey,
    classify_acvg_indication,
)

SURVEYS = ("cis", "dcvg", "pcm", "acvg")
CLASS_ORDER = {"high priority": 0, "further assessment": 1, "conflicting signals": 2}
ECDAClass = Literal["high priority", "further assessment", "conflicting signals"]


class ECDAConfig(SurveyInput):
    """Required assessment criteria with no implicit engineering or example values."""

    alignment_tolerance_m: float = Field(ge=0, description=CRITERIA_NOTE)
    cis_potential: Literal["off", "on"]
    cis_protection_criterion_V: float = Field(description=CRITERIA_NOTE)
    cis_overprotection_limit_V: float = Field(description=CRITERIA_NOTE)
    dcvg_min_ir_percentage: float = Field(ge=0, le=100, description=CRITERIA_NOTE)
    pcm: PCMConfig
    acvg: ACVGBands
    acvg_min_severity: Literal["minor", "moderate", "severe", "very_severe"]
    high_priority_min_surveys: int = Field(ge=2, le=4, description=CRITERIA_NOTE)
    require_cis_for_high: bool = Field(strict=True)
    provenance: CriteriaProvenance

    @model_validator(mode="after")
    def ordered_potentials(self) -> ECDAConfig:
        """Require overprotection limit below protection criterion."""
        if self.cis_overprotection_limit_V >= self.cis_protection_criterion_V:
            raise ValueError("CIS overprotection limit must be below protection criterion")
        if self.provenance.kind == "engineering" and any(
            criteria.provenance.kind != "engineering" for criteria in (self.pcm, self.acvg)
        ):
            raise ValueError("engineering correlation requires engineering PCM and ACVG criteria")
        return self


class ECDAAnomaly(BaseModel):
    """A ranked bounded-chainage group with every supporting observation."""

    rank: int
    chainage_m: float
    classification: ECDAClass
    rule: str
    positive_surveys: int
    evidence: dict[str, list[dict[str, Any]]]


class ECDAResult(BaseModel):
    """Serializable assessment plus DataFrame and CSV evidence exports."""

    criteria: dict[str, Any]
    criteria_note: str = CRITERIA_NOTE
    anomalies: list[ECDAAnomaly]
    surveys: dict[str, list[dict[str, Any]]]

    def to_dataframe(self) -> pd.DataFrame:
        """Return one row per dig, including JSON evidence and exact criteria."""
        columns = ["rank", "chainage_m", "classification", "rule", "positive_surveys"]
        columns += [f"{name}_evidence" for name in SURVEYS] + ["criteria", "criteria_note"]
        rows = []
        for anomaly in self.anomalies:
            row = anomaly.model_dump(exclude={"evidence"})
            row.update({f"{name}_evidence": json.dumps(anomaly.evidence[name], allow_nan=False)
                        for name in SURVEYS})
            row.update(criteria=json.dumps(self.criteria, sort_keys=True, allow_nan=False),
                       criteria_note=self.criteria_note)
            rows.append(row)
        return pd.DataFrame(rows, columns=columns)

    def to_csv(self, path: str | Path) -> None:
        """Write the ranked DataFrame with stable headers, including when empty."""
        self.to_dataframe().to_csv(path, index=False, encoding="utf-8", lineterminator="\n")


def _finite(value: float, name: str, non_negative: bool = False) -> None:
    if not math.isfinite(value) or (non_negative and value < 0):
        raise ValueError(f"{name} must be finite" + (" and non-negative" if non_negative else ""))


def _survey_evidence(
    cis: list[CISSurveyPoint], dcvg: list[DCVGIndication],
    pcm: list[PCMSurveyPoint], acvg: list[ACVGIndication], config: ECDAConfig,
) -> dict[str, list[dict[str, Any]]]:
    surveys: dict[str, list[dict[str, Any]]] = {name: [] for name in SURVEYS}
    for point in cis:
        _finite(point.distance_m, "CIS chainage", True)
        _finite(point.on_potential_V, "CIS ON potential")
        if point.off_potential_V is not None:
            _finite(point.off_potential_V, "CIS OFF potential")
        potential = point.off_potential_V if config.cis_potential == "off" else point.on_potential_V
        state = "unknown"
        if potential is not None:
            state = ("positive" if potential > config.cis_protection_criterion_V
                     or potential <= config.cis_overprotection_limit_V else "normal")
        surveys["cis"].append(dict(point.model_dump(), potential_V=potential, state=state))
    for dcvg_point in dcvg:
        _finite(dcvg_point.distance_m, "DCVG chainage", True)
        _finite(dcvg_point.voltage_gradient_mV, "DCVG gradient")
        _finite(dcvg_point.ir_drop_percentage, "DCVG IR percentage", True)
        if dcvg_point.ir_drop_percentage > 100:
            raise ValueError("DCVG IR percentage must be <= 100")
        surveys["dcvg"].append(dict(
            dcvg_point.model_dump(),
            state="positive" if dcvg_point.ir_drop_percentage >= config.dcvg_min_ir_percentage else "normal",
        ))
    for segment in analyze_pcm_survey(pcm, config.pcm):
        surveys["pcm"].append(dict(segment.model_dump(), state="positive" if segment.flagged else "normal"))
    minimum = SEVERITIES.index(config.acvg_min_severity)
    for acvg_point in acvg:
        classified = classify_acvg_indication(acvg_point, config.acvg)
        surveys["acvg"].append(dict(
            classified.model_dump(),
            state="positive" if SEVERITIES.index(classified.severity) >= minimum else "normal",
        ))
    for points in surveys.values():
        points.sort(key=lambda item: (item["distance_m"], json.dumps(item, sort_keys=True)))
    return surveys


def _classify(evidence: dict[str, list[dict[str, Any]]], config: ECDAConfig) -> tuple[ECDAClass, str, int]:
    positive = {name for name, points in evidence.items() if any(p["state"] == "positive" for p in points)}
    normal_cis = any(p["state"] == "normal" for p in evidence["cis"])
    if normal_cis:
        return "conflicting signals", "cis_disagrees", len(positive)
    if len(positive) >= config.high_priority_min_surveys and (
        not config.require_cis_for_high or "cis" in positive
    ):
        return "high priority", "corroborated_anomaly", len(positive)
    return "further assessment", "isolated_anomaly", len(positive)


def correlate_surveys(
    *, cis: list[CISSurveyPoint] | None = None, dcvg: list[DCVGIndication] | None = None,
    pcm: list[PCMSurveyPoint] | None = None, acvg: list[ACVGIndication] | None = None,
    config: ECDAConfig | None = None,
) -> ECDAResult:
    """Align four surveys once each in bounded groups, then rank anomalies.

    A group starts at the earliest remaining chainage and includes observations
    no farther than the inclusive tolerance from that anchor. It cannot grow
    transitively. PCM uses segment midpoints, retaining both endpoints. Missing
    evidence stays unknown. Groups without positive evidence are omitted.
    Priority is class order, decreasing positive survey count, then chainage.
    """
    if config is None:
        raise ValueError("correlation requires explicit criteria and provenance")
    config = ECDAConfig.model_validate(config.model_dump())
    surveys = _survey_evidence(cis or [], dcvg or [], pcm or [], acvg or [], config)
    events = sorted(
        [(point["distance_m"], name, point) for name, points in surveys.items() for point in points],
        key=lambda event: (event[0], event[1], json.dumps(event[2], sort_keys=True)),
    )
    anomalies = []
    index = 0
    while index < len(events):
        anchor = events[index][0]
        evidence: dict[str, list[dict[str, Any]]] = {name: [] for name in SURVEYS}
        while index < len(events) and events[index][0] - anchor <= config.alignment_tolerance_m:
            distance, name, point = events[index]
            evidence[name].append(dict(point, offset_m=distance - anchor))
            index += 1
        if not any(point["state"] == "positive" for points in evidence.values() for point in points):
            continue
        classification, rule, count = _classify(evidence, config)
        anomalies.append(ECDAAnomaly(
            rank=0, chainage_m=anchor, classification=classification,
            rule=rule, positive_surveys=count, evidence=evidence,
        ))
    anomalies.sort(key=lambda a: (CLASS_ORDER[a.classification], -a.positive_surveys, a.chainage_m))
    for rank, anomaly in enumerate(anomalies, 1):
        anomaly.rank = rank
    # Include original PCM amplitudes as well as the segment evidence for plotting.
    surveys["pcm_current"] = [p.model_dump() for p in sorted(pcm or [], key=lambda p: p.distance_m)]
    kinds = sorted({config.provenance.kind, config.pcm.provenance.kind, config.acvg.provenance.kind})
    note = f"{'/'.join(kinds)} criteria selected explicitly; {CRITERIA_NOTE}"
    return ECDAResult(criteria=config.model_dump(), criteria_note=note, anomalies=anomalies, surveys=surveys)
