# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""ECDA route for the existing cathodic-protection YAML adapter."""

from pathlib import Path
from typing import Any

from pydantic import Field

from digitalmodel.cathodic_protection.cp_survey import (
    ACVGIndication,
    CISSurveyPoint,
    DCVGIndication,
    PCMSurveyPoint,
    SurveyInput,
)
from digitalmodel.cathodic_protection.ecda import ECDAConfig, correlate_surveys
from digitalmodel.cathodic_protection.ecda_reporting import write_chainage_html


class ECDAInput(SurveyInput):
    """Four survey lists and explicitly configurable assessment criteria."""

    criteria: ECDAConfig
    cis: list[CISSurveyPoint] = Field(default_factory=list)
    dcvg: list[DCVGIndication] = Field(default_factory=list)
    pcm: list[PCMSurveyPoint] = Field(default_factory=list)
    acvg: list[ACVGIndication] = Field(default_factory=list)


def run_ecda(cfg: dict[str, Any]) -> dict[str, Any]:
    """Store serializable evidence and optionally write CSV/HTML to result_folder.

    ASSESSMENT describes correlation completion and never a design PASS/FAIL.
    No artifacts are written unless Analysis.result_folder is supplied.
    """
    data = ECDAInput.model_validate(cfg["inputs"].get("ecda", {}))
    result = correlate_surveys(
        cis=data.cis, dcvg=data.dcvg, pcm=data.pcm, acvg=data.acvg, config=data.criteria,
    )
    results = {
        "ecda": result.model_dump(),
        "status": {
            "result": "ASSESSMENT",
            "governing_case": "integrated_surveys",
            "reason": result.criteria_note,
            "use_status": "engineering-validation-required",
            "checks": {"anomalies": len(result.anomalies)},
        },
    }
    directory = cfg.get("Analysis", {}).get("result_folder")
    if directory:
        output = Path(directory)
        output.mkdir(parents=True, exist_ok=True)
        result.to_csv(output / "ecda_dig_list.csv")
        write_chainage_html(result, output / "ecda_chainage.html")
        results["artifacts"] = {"dig_list": "ecda_dig_list.csv", "chart": "ecda_chainage.html"}
    cfg["results"] = results
    return cfg
