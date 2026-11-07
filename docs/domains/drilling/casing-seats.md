# Bottom-up casing seat screen

The `digitalmodel.well.drilling.casing_seats` API selects bottom-up seats from
piecewise-linear pore and fracture EMW profiles and screens static kick tolerance
per section. This implements the first slice of issue #3842. The calculation
basis is the equations supplied in that issue; operational qualification and
confirmation against the cited editions remain open.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-11-05 | Add the input contract, synthetic hand example and limits for bottom-up seats and static section kick tolerance. |
| 2 | 2026-11-06 | Correct section-length-limited acceptance, reject intervals without a selected shoe, and preserve the stated trip margin across each section. |

## Inputs and selection

Supply common TVD knots in feet, pore and fracture EMW in ppg, and a nonblank
profile basis. Depths must be nonnegative and strictly increasing; both EMWs
must be positive and finite, with fracture at least pore at every knot. The
common linear segmentation then guarantees that inequality between knots.
Interpolation outside the supplied depth range is refused.

TD must lie deeper than the profile top and within the supplied range. Trip
and kick margins are explicit, nonnegative ppg inputs, each with a nonblank
basis. No design margins are supplied by default.

Starting at TD, each section uses `MW = pore(bottom) + trip margin` and requires
`FG_req = MW + kick margin`. The shoe is the shallowest point for which every
fracture value through the section bottom meets that threshold. Exact segment
crossings are used; a shallow strong layer does not hide a deeper weak layer.
The new shoe becomes the next section bottom. Selection stops when the entire
remaining fracture interval to the profile top meets the threshold, including
equality. A bottom window below the requirement or lack of upward progress
raises `ValueError`. A nonmonotone pore profile that would leave less than the
stated trip margin anywhere in a section with its bottom-based MW also raises
`ValueError`. The check includes internal knots and the interpolated shoe
boundary; equality with the full trip margin is permitted.
The computational limit `max_sections` defaults to 100 intervals and accepts a
positive integer override. It bounds near-parallel profiles that can approach a
fixed depth without reaching the top. Exceeding it raises `ValueError` and
returns no partial design; it is not a physical casing acceptance criterion.

Results retain the input bases and return sections deepest first. Each section
contains shoe and bottom TVD, MW, required fracture EMW, fracture EMW at the shoe
and the governing criterion. `seat_depths_ft` includes only seats governed by
`fracture_gradient`. The terminal `profile_top` interval is retained for
inspection; shallow strings governed by other criteria remain unspecified.
`kick_tolerance` rejects this terminal interval with `ValueError`, whether the
profile top is at surface or below it, because its boundary is not a selected
casing seat. Only sections governed by `fracture_gradient` can be checked.

## Example

All values below are synthetic example inputs, not operating recommendations.

```python
from digitalmodel.well.drilling.casing_seats import (
    CasingPressureProfile,
    kick_tolerance,
    select_casing_seats,
)

profile = CasingPressureProfile(
    depths_ft=(0.0, 6000.0, 12000.0),
    pore_ppg=(9.0, 9.0, 15.0),
    fracture_ppg=(11.0, 14.6, 17.6),
    basis="Synthetic example from issue #3842",
)
design = select_casing_seats(
    profile,
    td_ft=12000.0,
    trip_margin_ppg=0.5,
    kick_margin_ppg=0.5,
    trip_margin_basis="Synthetic trip allowance",
    kick_margin_basis="Synthetic shoe allowance",
)
check = kick_tolerance(
    design.sections[0],
    influx_density_ppg=2.0,
    hole_diameter_in=8.5,
    pipe_diameter_in=5.0,
    design_kick_intensity_ppg=0.5,
    minimum_volume_bbl=10.0,
    kick_intensity_basis="Synthetic design influx",
    minimum_volume_basis="Synthetic acceptance threshold",
)
assert check.status == "no tolerance"
assert not check.passes
```

| Section bottom (ft TVD) | Shoe (ft TVD) | MW (ppg) | Required fracture EMW (ppg) | Criterion |
|---|---|---|---|---|
| 12000.0 | 8800.0 | 15.5 | 16.0 | Fracture gradient |
| 8800.0 | 3000.0 | 12.3 | 12.8 | Fracture gradient |
| 3000.0 | 0.0 | 9.5 | 10.0 | Profile top; shallow strings unspecified |

For the deepest section, shoe pressure margin is 228.8 psi. A swabbed influx
with design kick intensity zero gives a height of 325.9 ft and a volume of
15.0 bbl. A design kick intensity of 0.5 ppg gives no tolerance, despite
satisfying the seat-margin selection rule.

## Kick calculation and acceptance

Kick intensity means formation EMW in excess of MW. It is distinct from the
selection kick margin. Supply design kick intensity and minimum required
volume explicitly, each with its own basis; both may be zero, neither may be
negative or nonfinite. Hole and pipe diameters must be positive with hole
larger than pipe. Influx density must be nonnegative and below MW.

For shoe depth `S`, bottom depth `B`, shoe fracture EMW `FG`, mud weight `MW`
and influx density `rho`, the screen uses:

```text
shoe margin (psi) = 0.052 * S * (FG - MW)
zero-volume kick intensity (ppg) = S / B * (FG - MW)
height (ft) = [S * (FG - MW) - B * design KI] / (MW - rho)
annular capacity (bbl/ft) = (hole diameter^2 - pipe diameter^2) / 1029.4
volume (bbl) = height * annular capacity
```

A nonpositive height reports `no tolerance`, zero height and zero volume, and
fails even when minimum acceptable volume is zero. Cancellation at the
zero-volume intensity boundary is treated as zero within a numerical tolerance
of one billionth of a foot times ppg. When the bubble remains below the shoe,
positive height with insufficient volume reports `below minimum volume`;
meeting the volume threshold reports `acceptable`. These outcomes refer only
to this static calculation.

When the calculated height reaches or exceeds the open section length `B - S`,
the returned height is capped at that length and `limited_by_section_length`
is true. The returned `volume_bbl` is then a **lower bound**, rather than a
maximum influx volume. Status is `open section length governs` and `passes`
is true for the stated static shoe-pressure criterion, including when that
lower bound is below the minimum volume. With the open section full of influx,
shoe pressure is `0.052 * [(MW + design KI) * B - rho * (B - S)]` and remains
unchanged as further bubble volume enters the casing above the shoe. The
section-length condition means this pressure is at or below shoe fracture
pressure; the static model therefore imposes no finite shoe-pressure volume
limit. Equality at the shoe is included, consistently with the volume
acceptance boundary.

For example, a shoe at 11900.0 ft, bottom at 12000.0 ft, shoe fracture EMW
16.0 ppg, MW 15.5 ppg, influx density 2.0 ppg, zero design kick intensity,
8.5 in hole and 5.0 in pipe give a capped height of 100.0 ft and a lower-bound
volume of 4.6 bbl. Shoe pressure is 9661.6 psi against fracture pressure
9900.8 psi, leaving 239.2 psi margin even after the bubble reaches the shoe.
A minimum of 10.0 bbl therefore passes this static criterion. This does not
establish a total influx capacity, casing geometry above the shoe, another
pressure limit, gas expansion or operational acceptance.

## Verification and source limits

The regression tests use the synthetic hand example, multiple fracture troughs,
TD interpolation, invalid inputs, acceptance boundaries, the open-section
bubble limit and its lower-bound verdict, terminal-interval rejection, and
trip-margin preservation at knots and interpolated shoes. Independent unit
geometry checks compare the rounded oilfield
conversions with 12 inches per foot, 231 cubic inches per United States gallon and
42 gallons per barrel. These dimensional checks do not confirm a textbook
edition or a well-integrity requirement.

Public-source checks on 2026-11-05 could not retrieve the cited edition content:
the publisher destination for the
[Applied Drilling Engineering DOI](https://doi.org/10.2118/9781555630010)
returned HTTP 403; the public standards information page checked returned
HTTP 404. No licensed publication values or clauses are asserted by this API.
Confirmation of the conversion constants against the cited edition and of the
NORSOK D-010 Rev. 5 (2021) section requirement remains open.

## Open remainder

String-design hand-off, differential sticking, riser-loss margins, TVD against
measured depth, variable annular geometry, gas expansion and circulation or
temperature effects are outside this slice. The existing dual-gradient screen
continues to provide its existing top-down selection. A result does not
establish a complete casing programme, barrier acceptance or standards
compliance.
