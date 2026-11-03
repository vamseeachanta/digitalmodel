"""Aboveground tank-bottom CP screening in the context of API RP 651.

API RP 651, 5th edition (2024), scope:
https://www.apiwebstore.org/standards/651 . No paid-standard design values
are embedded and no API compliance verdict is produced.

Public engineering basis: DoD TSEWG Electrical TP-16 (March 2017), section
5.11, pp128-131, https://www.wbdg.org/FFC/DOD/STC/tsewg_tp16.pdf .
It describes ribbon grids above an HDPE liner, area/current sizing, multiple
feeds to reduce metallic drop, and the dependence of sand resistivity on
moisture. Section 5.11.2.2 prints a multiplication sign for ribbon quantity;
dimensional analysis and its numerical example require division I/q.

Resistance per length, rated ribbon output at the intended service life,
current density, foundation threshold and spacing limit are project inputs.
Actual electrolyte conduction and steel polarization are not solved here.
"""

from __future__ import annotations

import math
from typing import Literal

from pydantic import BaseModel, ConfigDict, Field, model_validator
from digitalmodel.reporting.spec import PublicWebCitation

from digitalmodel.cathodic_protection._kernels import (
    _require_non_negative, _require_positive, current_demand,
)
from digitalmodel.cathodic_protection.reference_electrodes import (
    ReferenceElectrode, convert_potential,
)


class _FiniteModel(BaseModel):
    model_config = ConfigDict(allow_inf_nan=False, extra="forbid", frozen=True)


class RibbonGrid(_FiniteModel):
    """Centred parallel chords, optionally repeated at 90 degrees."""

    nominal_spacing_m: float
    offsets_m: tuple[float, ...]
    chord_lengths_m: tuple[float, ...]
    directions: Literal[1, 2]
    total_length_m: float


def ribbon_grid(diameter_m: float, spacing_m: float, *, directions: Literal[1, 2] = 1) -> RibbonGrid:
    """Lay out equal-pitch chords; each length = 2 sqrt((D/2)^2 - y^2).

    Derivation: circle equation x^2+y^2=R^2; TP-16 (2017), section 5.11.2.2
    uses evenly spaced parallel ribbons clipped by the circular perimeter.
    n=ceil(D/requested_pitch); actual_pitch=D/n; row centres have half-pitch
    margins in the normal direction. This geometry does not establish the
    largest electrochemical distance at the curved edge or current uniformity.
    """
    _require_positive("diameter_m", diameter_m)
    _require_positive("spacing_m", spacing_m)
    if type(directions) is not int or directions not in (1, 2):
        raise ValueError("directions must be 1 or 2")
    ratio = diameter_m / spacing_m
    if not math.isfinite(ratio) or ratio == 0:
        raise ValueError("grid dimensions are not numerically representable")
    count = math.ceil(ratio)
    if count > 100_000:
        raise ValueError("grid exceeds 100000 rows; check metre units")
    pitch = diameter_m / count
    radius = diameter_m / 2
    offsets = tuple(-radius + (i + 0.5) * pitch for i in range(count))
    lengths = tuple(2 * math.sqrt((radius - y) * (radius + y)) for y in offsets)
    total_length = math.fsum(lengths) * directions
    _require_positive("total ribbon length", total_length)
    return RibbonGrid(
        nominal_spacing_m=pitch, offsets_m=offsets, chord_lengths_m=lengths,
        directions=directions, total_length_m=total_length,
    )


def conductor_voltage_drop(
    current_A: float, length_m: float, resistance_ohm_m: float, *,
    loading: Literal["end_load", "uniform_one_end", "uniform_both_ends"] = "end_load",
) -> float:
    """Metallic drop in volts; excludes electrolyte, joints and feed cables.

    Ohm's law V=IR: TP-16 (2017), section 5.11.2.4. R=r*L is the definition
    of resistance per length. For total uniformly discharged current I,
    integration of r*I*(1-x/L) dx gives I*r*L/2 with one-end feed.
    Equal-voltage feeds at both ends give I*r*L/8 at midspan. Factors 1/2
    and 1/8 are analytic geometry, not API coefficients. A lumped end load
    gives I*r*L. Uniform discharge is an assumption, not a predicted result.
    """
    _require_non_negative("current_A", current_A)
    _require_positive("length_m", length_m)
    _require_non_negative("resistance_ohm_m", resistance_ohm_m)
    factors = {"end_load": 1.0, "uniform_one_end": 0.5, "uniform_both_ends": 0.125}
    if loading not in factors:
        raise ValueError("unknown conductor loading")
    drop = current_A * resistance_ohm_m * length_m * factors[loading]
    if not math.isfinite(drop):
        raise ValueError("voltage drop overflow; check SI units")
    return drop


class TankBottomInput(_FiniteModel):
    """SI inputs; all engineering ratings and limits require a project basis."""

    diameter_m: float = Field(gt=0)
    current_density_mA_m2: float = Field(gt=0)
    ribbon_output_A_m: float = Field(gt=0, description="Rating at intended life and electrolyte")
    ribbon_spacing_m: float = Field(gt=0)
    max_spacing_m: float = Field(gt=0, description="Project limit, not an API default")
    grid_directions: Literal[1, 2] = 1
    ribbon_resistance_ohm_m: float = Field(ge=0)
    ribbon_feed_span_m: float = Field(gt=0, description="Longest physical span between feeds")
    ribbon_loading: Literal["uniform_one_end", "uniform_both_ends"] = "uniform_both_ends"
    busbar_resistance_ohm_m: float = Field(ge=0)
    busbar_feed_length_m: float = Field(gt=0)
    busbar_current_A: float = Field(gt=0, description="Current assigned to this busbar feed path")
    foundation_resistivity_ohm_m: float = Field(gt=0)
    max_foundation_resistivity_ohm_m: float = Field(gt=0, description="Project warning threshold")
    hdpe_barrier: bool = Field(default=True, strict=True)
    anodes_above_liner: bool = Field(default=True, strict=True)
    dry_sand: bool = Field(default=False, strict=True)
    oil_contaminated_sand: bool = Field(default=False, strict=True)
    voids_present: bool = Field(default=False, strict=True)

    @model_validator(mode="after")
    def span_fits_tank(self) -> TankBottomInput:
        """Reject a ribbon feed span exceeding the longest possible chord."""
        if self.ribbon_feed_span_m > self.diameter_m:
            raise ValueError("ribbon feed span cannot exceed tank diameter")
        return self


class TankBottomResult(_FiniteModel):
    """Independent sizing checks; none establishes tank polarization."""

    area_m2: float
    current_demand_A: float
    minimum_ribbon_length_m: float
    grid: RibbonGrid
    installed_ribbon_length_m: float
    ribbon_capacity_meets_demand: bool
    spacing_within_input_limit: bool
    ribbon_voltage_drop_V: float
    busbar_voltage_drop_V: float
    warnings: tuple[str, ...]
    method_source: PublicWebCitation


def assess_tank_bottom(inputs: TankBottomInput) -> TankBottomResult:
    """Compute area, demand, actual ribbon length and metallic path drops.

    A=pi*D^2/4 and I=A*j/1000: TP-16 (2017), section 5.11.2.1, with j in
    mA/m2. L_min=I/q: section 5.11.2.2 (dimensionally corrected division).
    The existing shared current-demand kernel is reused. Ribbon span current
    is (I/installed_length)*span, assuming equal discharge per metre; busbar
    drop uses the supplied path current as a lumped end load. These drops
    are separate path estimates and are not added into a rectifier voltage.
    Foundation warnings follow the conduction concerns in section 5.11 and
    section 5.11.2.3; the threshold is explicitly supplied by the user.
    """
    inputs = TankBottomInput.model_validate(inputs.model_dump())
    radius = inputs.diameter_m / 2
    area = math.pi * radius * radius
    demand = current_demand(area, inputs.current_density_mA_m2 / 1000, 1.0)
    grid = ribbon_grid(inputs.diameter_m, inputs.ribbon_spacing_m, directions=inputs.grid_directions)
    length = grid.total_length_m
    warnings = [
        "Ribbon capacity and metallic drops do not establish center or edge polarization; "
        "obtain IR-corrected local measurements and verify current distribution.",
        "Nominal pitch does not establish curved-edge coverage; review the installed layout.",
    ]
    if inputs.foundation_resistivity_ohm_m > inputs.max_foundation_resistivity_ohm_m:
        warnings.append("Foundation resistivity exceeds the supplied project threshold.")
    for condition, message in (
        (inputs.dry_sand, "Dry sand can interrupt the electrolyte current path."),
        (inputs.oil_contaminated_sand, "Oil-contaminated sand can interrupt the electrolyte current path."),
        (inputs.voids_present, "Voids can isolate the tank bottom from electrolyte contact."),
        (inputs.hdpe_barrier and not inputs.anodes_above_liner,
         "HDPE liner shields the bottom from anodes below the liner; relocate the anodes."),
    ):
        if condition:
            warnings.append(message)
    return TankBottomResult(
        area_m2=area, current_demand_A=demand, minimum_ribbon_length_m=demand / inputs.ribbon_output_A_m,
        grid=grid, installed_ribbon_length_m=length,
        ribbon_capacity_meets_demand=length * inputs.ribbon_output_A_m >= demand,
        spacing_within_input_limit=grid.nominal_spacing_m <= inputs.max_spacing_m,
        ribbon_voltage_drop_V=conductor_voltage_drop(
            demand / length * inputs.ribbon_feed_span_m, inputs.ribbon_feed_span_m,
            inputs.ribbon_resistance_ohm_m, loading=inputs.ribbon_loading,
        ),
        busbar_voltage_drop_V=conductor_voltage_drop(
            inputs.busbar_current_A, inputs.busbar_feed_length_m, inputs.busbar_resistance_ohm_m,
        ),
        warnings=tuple(warnings),
        method_source=PublicWebCitation(
            code_id="dod-electrical-tp16", publisher="DoD TSEWG", revision="2017-03",
            section="section 5.11, printed pp128-131",
            url="https://www.wbdg.org/FFC/DOD/STC/tsewg_tp16.pdf", retrieved="2026-11-02",
            note="Public method context; all ratings and limits are project inputs.",
        ),
    )


class CenterEdgeResult(_FiniteModel):
    """Local potential comparisons in the measurement electrode."""

    reference_electrode: ReferenceElectrode
    criterion_V: float
    center_potential_V: float | None
    edge_potential_V: float | None
    center_minus_edge_V: float | None
    center_meets_criterion: bool | None
    edge_meets_criterion: bool | None
    both_locations_meet_criterion: bool


def check_center_edge_polarization(
    center_potential_V: float | None, edge_potential_V: float | None, *,
    criterion_V: float, reference_electrode: ReferenceElectrode | str = "CSE",
    criterion_reference: ReferenceElectrode | str = "CSE", ir_corrected: bool = True,
    zinc_potential_she_V: float | None = None,
) -> CenterEdgeResult:
    """Compare supplied IR-corrected potentials with an explicit criterion.

    The comparison E<=E_criterion uses the negative structure-to-electrolyte
    convention in ASTM G215-17 section 6. Reference conversion follows the
    cited shared utility. No universal criterion or centre-to-edge allowable
    spread is supplied; the difference is descriptive only. Missing readings
    remain unassessed. Two locations cannot establish full bottom coverage.
    """
    if ir_corrected is not True:
        raise ValueError("IR-corrected (e.g. verified instant-off) potentials are required")
    criterion = convert_potential(
        criterion_V, criterion_reference, reference_electrode,
        zinc_potential_she_V=zinc_potential_she_V,
    )
    for value in (center_potential_V, edge_potential_V):
        if value is not None and not math.isfinite(value):
            raise ValueError("local potentials must be finite volts or None")
    center_ok = None if center_potential_V is None else center_potential_V <= criterion
    edge_ok = None if edge_potential_V is None else edge_potential_V <= criterion
    return CenterEdgeResult(
        reference_electrode=reference_electrode, criterion_V=criterion,
        center_potential_V=center_potential_V, edge_potential_V=edge_potential_V,
        center_minus_edge_V=(None if center_potential_V is None or edge_potential_V is None
                             else center_potential_V - edge_potential_V),
        center_meets_criterion=center_ok, edge_meets_criterion=edge_ok,
        both_locations_meet_criterion=center_ok is True and edge_ok is True,
    )
