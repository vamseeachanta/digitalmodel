"""Shared structure-to-electrolyte potential conversion, in volts at 25 deg C.

E_target = E_source + E_ref,source(SHE) - E_ref,target(SHE), from the
definition E_measured = E_structure - E_reference. ASTM G215-17, Standard
Guide for Electrode Potential Measurement (2017), is cited for this sign
convention only; no offset from that standard is reproduced.
No temperature, junction-potential or IR-drop correction is inferred.

Selected offsets, not a reproduction of a source table:
* CSE +0.316, Ag/AgCl/seawater nominal +0.250, Ag/AgCl/saturated KCl
  +0.197: CEOCOR Commission 2, Rev 11 (November 2018), section 3.4,
  Table 1, p12. Seawater chloride activity is not fixed by this nominal value.
* SHE is zero by definition. SCE (saturated KCl) and zinc offsets require
  finite caller-supplied calibration values vs SHE. No nominal SCE or
  universal zinc offset is embedded.

Each lookup carries a validated public-source citation sidecar. These public
references do not claim qualification through the separate wiki resolver.
"""

from __future__ import annotations

import math
from enum import Enum

from pydantic import BaseModel, ConfigDict

from digitalmodel.reporting.spec import PublicWebCitation

CEOCOR_URL = (
    "https://ceocor.lu/wp-content/uploads/2018/11/"
    "Reference-Electrodes-for-Monitoring-of-Cathodic-Protection-on-Buried-Pipelines-"
    "R11-Approved-November-2018.pdf"
)
ASTM_PREVIEW_URL = (
    "https://standards.iteh.ai/catalog/standards/astm/"
    "ccf20561-4d48-4fcd-8ce7-0025addda1d2/astm-g215-17"
)


class ReferenceElectrode(str, Enum):
    """Explicit electrolyte names; bare Ag/AgCl is deliberately ambiguous."""

    SHE = "SHE"
    CSE = "CSE"
    AG_AGCL_SEAWATER = "Ag/AgCl/seawater"
    AG_AGCL_SATURATED_KCL = "Ag/AgCl/saturated KCl"
    SCE = "SCE"
    ZINC = "zinc"


class ElectrodeReference(BaseModel):
    """Nominal offset with conditions and a source, not field calibration."""

    model_config = ConfigDict(frozen=True, allow_inf_nan=False, extra="forbid")
    electrode: ReferenceElectrode
    potential_she_V: float | None
    condition: str
    temperature_c: float = 25.0
    citation: PublicWebCitation


def electrode_reference(electrode: ReferenceElectrode | str) -> ElectrodeReference:
    """Return the selected offset; SCE and zinc require supplied calibration.

    Sources and locators are listed in this module's docstring. The nominal
    seawater value is approximate; calibration is needed for field accuracy.
    """
    key = ReferenceElectrode(electrode)
    offsets = {
        ReferenceElectrode.SHE: (0.0, "standard hydrogen electrode"),
        ReferenceElectrode.CSE: (0.316, "saturated CuSO4"),
        ReferenceElectrode.AG_AGCL_SEAWATER: (0.250, "seawater; nominal chloride activity"),
        ReferenceElectrode.AG_AGCL_SATURATED_KCL: (0.197, "saturated KCl"),
        ReferenceElectrode.SCE: (None, "saturated KCl calomel; calibrated offset required"),
        ReferenceElectrode.ZINC: (None, "pseudo-reference; calibrated offset required"),
    }
    ceocor = key in (
        ReferenceElectrode.CSE, ReferenceElectrode.AG_AGCL_SEAWATER,
        ReferenceElectrode.AG_AGCL_SATURATED_KCL,
    )
    source = PublicWebCitation(
        code_id="ceocor-reference-electrodes" if ceocor else "astm-g215",
        publisher="CEOCOR" if ceocor else "ASTM International",
        revision="2018-11 Rev 11" if ceocor else "2017",
        section=("section 3.4, Table 1, p12" if ceocor else "sign convention only"),
        url=CEOCOR_URL if ceocor else ASTM_PREVIEW_URL,
        retrieved="2026-11-02",
        note=(offsets[key][1] if ceocor else
              offsets[key][1] + "; no numerical offset sourced from ASTM"),
    )
    return ElectrodeReference(
        electrode=key, potential_she_V=offsets[key][0],
        condition=offsets[key][1], citation=source,
    )


def convert_potential(
    potential_V: float,
    source: ReferenceElectrode | str,
    target: ReferenceElectrode | str,
    *,
    temperature_c: float = 25.0,
    zinc_potential_she_V: float | None = None,
    sce_potential_she_V: float | None = None,
) -> float:
    """Convert V using E_target = E_source + offset_source - offset_target.

    Basis: CEOCOR Rev 11 Table 1 and the sign convention above.
    Only the 25 deg C condition is supported. SCE and zinc conversions
    require their respective finite, site-calibrated potentials vs SHE in volts.
    Example: convert_potential(-0.850, "CSE", "SHE") returns -0.534 V.
    """
    if not math.isfinite(potential_V):
        raise ValueError("potential_V must be finite and in volts")
    if temperature_c != 25.0:
        raise ValueError("tabulated offsets support only 25 deg C")
    if zinc_potential_she_V is not None and not math.isfinite(zinc_potential_she_V):
        raise ValueError("zinc calibration must be finite")
    if sce_potential_she_V is not None and not math.isfinite(sce_potential_she_V):
        raise ValueError("SCE calibration must be finite")
    references = (electrode_reference(source), electrode_reference(target))
    offsets = []
    for reference in references:
        value = reference.potential_she_V
        if value is None:
            value = (sce_potential_she_V if reference.electrode == ReferenceElectrode.SCE
                     else zinc_potential_she_V)
            if value is None:
                raise ValueError(f"{reference.electrode.value} requires a site-calibrated potential vs SHE")
        offsets.append(value)
    if references[0].electrode == references[1].electrode:
        return potential_V
    return potential_V + (offsets[0] - offsets[1])
