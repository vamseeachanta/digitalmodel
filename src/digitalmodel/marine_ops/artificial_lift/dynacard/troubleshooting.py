# ABOUTME: Curated symptom/mechanism/field-response catalog for dynacard phenomena.
# ABOUTME: Covers all 18 classifier modes plus archive labels and training-deck cases.
"""Troubleshooting guide for sucker-rod-pump dynamometer-card phenomena.

This is a curated field-practice catalog distilled from rod-pump automation
training material (2016-2017 card-animation decks) and kept consistent with
Beam Lift Handbook-style guidance (Bommer & Podio). Each entry pairs the card
signature and wellsite observation (symptom) with the physics that produces
the shape (mechanism) and an ordered list of concrete field responses.

Coverage:

* All 18 classifier modes from ``diagnostics.PumpDiagnostics.FAILURE_MODES``
  (excluding the legacy ``VALVE_LEAK`` alias).
* Field/archive labels used in historical card libraries: ``FULL_CARD``,
  ``INCOMPLETE_FILLAGE``, ``FILLAGE_COLLAPSE``, ``BUTTERFLY``, ``PUMPED_OFF``.
* Additional training-deck phenomena: ``FLUMPING``, ``DEVIATED_WELL_FRICTION``,
  ``HIGH_FLUID_LEVEL``, ``HEAVY_OIL``, ``BARREL_HOLE_SPLIT``.

Severity mirrors the ``field_health`` conventions: ``NORMAL`` maps to
``normal``; ``GAS_LOCK``, ``ROD_PARTING``, and ``STUCK_PUMP`` map to
``failure``; ``PUMP_TAGGING`` maps to ``critical``; every other classifier
mode maps to ``warning``. Archive and training-deck labels are assigned the
closest equivalent.
"""

from typing import Literal

from pydantic import BaseModel, Field

Severity = Literal["normal", "warning", "critical", "failure"]


class TroubleshootingEntry(BaseModel):
    """One phenomenon in the troubleshooting catalog."""

    phenomenon: str
    title: str
    symptom: str
    mechanism: str
    actions: list[str] = Field(min_length=3, max_length=4)
    severity: Severity


def _entry(
    phenomenon: str,
    title: str,
    symptom: str,
    mechanism: str,
    actions: list[str],
    severity: Severity,
) -> TroubleshootingEntry:
    return TroubleshootingEntry(
        phenomenon=phenomenon,
        title=title,
        symptom=symptom,
        mechanism=mechanism,
        actions=actions,
        severity=severity,
    )


TROUBLESHOOTING_GUIDE: dict[str, TroubleshootingEntry] = {
    entry.phenomenon: entry
    for entry in [
        # ------------------------------------------------------------------
        # Tier 1: core classifier modes
        # ------------------------------------------------------------------
        _entry(
            "NORMAL",
            "Normal operation",
            "The downhole card is a full rectangle: load transfers cleanly at "
            "the start of the upstroke and releases cleanly at the start of "
            "the downstroke. At the wellsite the unit runs smoothly, "
            "production matches the pump displacement, and the polished rod "
            "load stays within the rated range.",
            "The barrel fills completely with liquid each stroke, so the "
            "standing and traveling valves open and close crisply and the "
            "full fluid load is carried through the whole upstroke.",
            [
                "No corrective action needed; log the card as a healthy "
                "baseline for future comparison.",
                "Trend pump fillage and card area so gradual wear shows up "
                "early.",
                "Verify the pump-off controller setpoints still bracket the "
                "current card so normal operation is not falsely flagged.",
            ],
            "normal",
        ),
        _entry(
            "GAS_INTERFERENCE",
            "Gas interference",
            "The downhole card shows a rounded, sloping load release on the "
            "downstroke instead of a sharp corner, and pump fillage is "
            "reduced. At the wellsite production is erratic, the flowline "
            "may show gas heading or slugging, and fluid shots show liquid "
            "over the pump despite low pump fillage.",
            "Free gas enters the barrel with the liquid; on the downstroke "
            "the plunger must compress the gas before the traveling valve "
            "can open, so load bleeds off gradually rather than abruptly.",
            [
                "Do not treat it as fluid pound: raise the pump-off "
                "controller fillage setpoint (lower the pump-off threshold) "
                "and allow more consecutive pump-off strokes (10-15) before "
                "shutting down, so gassy strokes are ridden through.",
                "Install or service a downhole gas separator, or set the "
                "pump intake below the perforations so gravity separates "
                "free gas before it reaches the pump.",
                "Use a variable-speed drive to slow the pump through gas "
                "slugs instead of cycling the unit off.",
                "Reduce pump compression ratio issues by minimizing unswept "
                "clearance (spacing the pump close to tag) so trapped gas "
                "is compressed to opening pressure.",
            ],
            "warning",
        ),
        _entry(
            "FLUID_POUND",
            "Fluid pound",
            "The downhole card loses its lower-right corner: load holds high "
            "partway down the downstroke, then drops suddenly when the "
            "plunger strikes the liquid. At the wellsite the unit shakes or "
            "knocks once per stroke, rod couplings and the gearbox take an "
            "audible impact, and fillage is well below 100%.",
            "The pump displaces more than the well inflows, so the barrel is "
            "only partly liquid-filled; the plunger falls through the gas "
            "gap and impacts the fluid level, transferring the fluid load as "
            "a shock instead of a gradual release.",
            [
                "Enable or tune the pump-off controller so the unit shuts "
                "down or idles when fillage drops below setpoint, instead of "
                "pounding continuously.",
                "Slow the pumping speed (SPM) with a variable-speed drive so "
                "displacement matches well inflow.",
                "Shorten the stroke length or downsize the plunger if speed "
                "reduction alone cannot balance displacement with inflow.",
                "Inspect rods, couplings, and the gearbox for damage if the "
                "well has been pounding for an extended period.",
            ],
            "warning",
        ),
        _entry(
            "PUMP_TAGGING",
            "Pump tagging",
            "The downhole card shows a sharp load spike at the bottom (or "
            "top) of the stroke where the plunger contacts the pump. At the "
            "wellsite a distinct metallic tap is heard or felt each stroke "
            "and the polished rod load trace shows a repeating impact.",
            "The pump is spaced too close, or rod stretch/overtravel lets "
            "the plunger strike the standing valve or top of the pump, "
            "producing a mechanical impact load every stroke.",
            [
                "Re-space the pump: raise the rod string at the wellhead so "
                "the plunger clears the standing valve at bottom of stroke.",
                "Check for changed operating conditions (fluid level, SPM, "
                "rod overtravel) that increased downhole stroke since the "
                "pump was last spaced.",
                "Slow the unit temporarily if re-spacing must wait, to "
                "reduce impact energy.",
                "Inspect the pump and standing valve for impact damage once "
                "pulled; repeated tagging cracks cages and seats.",
            ],
            "critical",
        ),
        _entry(
            "TUBING_MOVEMENT",
            "Tubing movement (unanchored tubing)",
            "The downhole card is sheared into a parallelogram: the load "
            "pickup and release ramps are stretched over a long position "
            "range and net plunger stroke is short. At the wellsite "
            "production is lower than pump displacement predicts and the "
            "tubing may be seen breathing at the hanger.",
            "Without a tubing anchor the tubing stretches when the fluid "
            "load transfers to the rods and recoils when it transfers back, "
            "so part of every stroke is spent moving tubing instead of "
            "displacing fluid.",
            [
                "Install a tubing anchor/catcher set near the pump depth to "
                "eliminate tubing stretch.",
                "Until anchored, account for the stroke loss when sizing "
                "displacement so the well is not misdiagnosed as pumped off.",
                "Verify an existing anchor has not unset or slipped if this "
                "signature appears on a previously anchored well.",
            ],
            "warning",
        ),
        _entry(
            "VALVE_LEAK_TV",
            "Traveling valve leak",
            "The upstroke load line sags: load bleeds off during the "
            "upstroke instead of holding flat, rounding the top of the card. "
            "At the wellsite production is down while the unit strokes "
            "normally, and a standing valve check (stopped upstroke) shows "
            "the polished rod load decaying.",
            "A cut or worn traveling valve or seat lets pressurized fluid "
            "leak back past the plunger on the upstroke, progressively "
            "transferring the fluid load off the rods.",
            [
                "Confirm with a traveling valve test: stop the unit on the "
                "upstroke and watch for load bleed-off at the polished rod.",
                "Flush or cycle the pump to clear trash that may be holding "
                "the valve off seat before committing to a pull.",
                "Schedule a pump pull to replace the traveling valve, ball, "
                "and seat if the leak persists.",
                "Check produced-fluid sand and solids trends; abrasive wear "
                "on valves suggests adding sand control at the same pull.",
            ],
            "warning",
        ),
        _entry(
            "VALVE_LEAK_SV",
            "Standing valve leak",
            "The downstroke load line rises: load creeps back onto the rods "
            "during the downstroke, rounding the bottom of the card. At the "
            "wellsite production is down, and a standing valve test (stopped "
            "downstroke) shows the polished rod load climbing.",
            "A leaking standing valve lets fluid drain from the barrel back "
            "into the wellbore on the downstroke, so barrel pressure falls "
            "and part of the fluid load transfers back onto the rods early.",
            [
                "Confirm with a standing valve test: stop the unit on the "
                "downstroke and watch for load gain at the polished rod.",
                "Attempt to flush trash off the seat by cycling the pump "
                "before pulling.",
                "Pull and repair or replace the standing valve, ball, and "
                "seat if the leak persists.",
            ],
            "warning",
        ),
        # ------------------------------------------------------------------
        # Tier 2: common field failures
        # ------------------------------------------------------------------
        _entry(
            "ROD_PARTING",
            "Rod parting",
            "The card collapses to a near-flat line with very low loads and "
            "near-zero area; the load never picks up the fluid column. At "
            "the wellsite the unit strokes freely with abnormally low "
            "polished rod load and production stops entirely.",
            "The rod string has separated, so the surface unit lifts only "
            "the rods above the break; no fluid load reaches the polished "
            "rod and the pump no longer strokes.",
            [
                "Shut the well in immediately; continued stroking risks "
                "damaging the parted string and complicating fishing.",
                "Compare current polished rod load to the rod-weight-only "
                "value to estimate the break depth before pulling.",
                "Move in a service rig to fish the parted string and replace "
                "the failed rods and couplings.",
                "Review the rod loading (Goodman analysis) and corrosion "
                "program so the replacement string does not repeat the "
                "failure.",
            ],
            "failure",
        ),
        _entry(
            "STUCK_PUMP",
            "Stuck pump",
            "The card shows near-zero position range downhole: the rods "
            "stretch and recoil but the plunger barely moves. At the "
            "wellsite the polished rod loads swing between high extremes, "
            "the unit labors, and production is zero.",
            "Debris, scale, sand, or mechanical damage has seized the "
            "plunger in the barrel, so all surface stroke goes into rod "
            "stretch instead of plunger travel.",
            [
                "Stop the unit before rod or structural overload; do not "
                "keep stroking against a stuck plunger.",
                "Attempt to free the pump with gentle work: hot oil or "
                "solvent circulation for paraffin/scale, or careful "
                "up-weight within rod limits.",
                "If the pump cannot be freed, pull the well and replace the "
                "pump; inspect for sand, scale, or corrosion debris.",
                "Add sand control, scale inhibition, or filtered plunger "
                "designs to prevent recurrence.",
            ],
            "failure",
        ),
        _entry(
            "WORN_BARREL",
            "Worn barrel / plunger slippage",
            "Card area shrinks gradually over weeks, with the upstroke load "
            "sagging as the plunger rises. At the wellsite production "
            "declines slowly while stroke and speed are unchanged, and pump "
            "efficiency trends steadily down.",
            "Wear increases the plunger-barrel clearance so fluid slips past "
            "the plunger during the upstroke; slippage grows with wear, "
            "bleeding load and displacement.",
            [
                "Trend pump efficiency and card area to schedule the pull "
                "before production loss becomes uneconomic.",
                "Pull and replace or re-hone the pump; select tighter "
                "clearance consistent with fluid viscosity and sand content.",
                "Check for abrasives in produced fluid; add sand control or "
                "harder plunger/barrel metallurgy if wear is abrasive.",
            ],
            "warning",
        ),
        _entry(
            "GAS_LOCK",
            "Gas lock",
            "The card collapses to near-zero area: neither valve opens and "
            "no load transfer occurs stroke after stroke. At the wellsite "
            "the unit strokes normally but produces nothing, often "
            "following a period of gas interference.",
            "The barrel is filled with gas that merely compresses and "
            "expands between the valves; barrel pressure never falls below "
            "intake pressure to open the standing valve nor rises above "
            "discharge pressure to open the traveling valve.",
            [
                "Stop and restart the unit or slow it drastically to give "
                "the pump time to bleed gas and prime with liquid.",
                "Re-space the pump closer to tag to cut unswept clearance "
                "volume so trapped gas compresses to valve-opening pressure.",
                "Install or service a downhole gas separator, or move the "
                "intake below the perforations, to stop free gas entering "
                "the pump.",
                "Tune the pump-off controller so the well is not repeatedly "
                "driven into the low-submergence condition that invites gas "
                "locking.",
            ],
            "failure",
        ),
        _entry(
            "DELAYED_TV_CLOSURE",
            "Delayed traveling valve closure",
            "Load picks up exponentially at the start of the upstroke "
            "instead of sharply, giving a rounded lower-left corner. At the "
            "wellsite output per stroke is slightly low and the card shows a "
            "consistent soft start to every upstroke.",
            "The traveling valve is slow to seat at the start of the "
            "upstroke (weak spring, viscous fluid, or worn ball/cage), so "
            "fluid load transfers to the rods gradually while the valve "
            "closes.",
            [
                "Inspect the traveling valve spring, ball, and cage at the "
                "next pump service.",
                "Check fluid viscosity: heavy or emulsified fluid delays "
                "valve action, so consider a valve/cage design rated for "
                "viscous service.",
                "Trend the signature; if the rounding grows it is "
                "progressing toward a traveling valve leak and the pull "
                "should be scheduled sooner.",
            ],
            "warning",
        ),
        # ------------------------------------------------------------------
        # Tier 3: mechanical and operational
        # ------------------------------------------------------------------
        _entry(
            "EXCESSIVE_FRICTION",
            "Excessive friction",
            "The card is fattened vertically: upstroke loads run high and "
            "downstroke loads run low, giving a large hysteresis loop. At "
            "the wellsite power draw is high for the production achieved "
            "and the gearbox may run hot.",
            "Mechanical drag between rods and tubing, in the stuffing box, "
            "or in the pump adds load in the direction opposing motion, "
            "inflating the loop on both strokes.",
            [
                "Identify the friction source: check stuffing box tightness "
                "and rod rotator first, then review deviation survey for "
                "rod-on-tubing drag intervals.",
                "Install rod guides or roller guides across doglegs and "
                "high-side-load intervals.",
                "Treat for paraffin or scale if wellbore deposits are "
                "adding drag.",
                "Verify pump clearance and fluid viscosity are compatible; "
                "tight pumps in heavy fluid generate large friction loads.",
            ],
            "warning",
        ),
        _entry(
            "PLUNGER_UNDERTRAVEL",
            "Plunger undertravel",
            "The downhole card is narrow: net plunger stroke is visibly "
            "shorter than the surface stroke should deliver. At the "
            "wellsite production is below design and inferred displacement "
            "per stroke is short.",
            "Rod stretch and dynamic effects absorb part of the surface "
            "stroke, so the plunger travels less than the polished rod; "
            "high loads, slow speeds, or an under-designed rod string make "
            "it worse.",
            [
                "Verify tubing is anchored; unanchored tubing compounds the "
                "stroke loss.",
                "Adjust speed: moderate SPM changes shift rod dynamics and "
                "can recover overtravel from the natural rod resonance.",
                "Redesign the rod string (larger diameter or stiffer taper) "
                "if stretch is structural rather than operational.",
                "Consider a longer surface stroke unit if the well needs "
                "more net displacement than dynamics allow.",
            ],
            "warning",
        ),
        _entry(
            "PARAFFIN_RESTRICTION",
            "Paraffin restriction",
            "The card shows concave dents and a growing friction signature, "
            "often worsening over days as deposits build. At the wellsite "
            "flowline pressure creeps up, the polished rod may stall or "
            "jerk, and paraffin shows in samples or at the stuffing box.",
            "Wax deposits on rods and tubing walls narrow the flow path and "
            "grip the rod string, adding position-dependent drag that dents "
            "and distorts the card.",
            [
                "Hot oil or hot water the well to melt and circulate out "
                "the paraffin.",
                "Put the well on a regular cutting/treatment schedule: rod "
                "scrapers, chemical injection, or periodic hot oiling "
                "matched to the deposition rate.",
                "Trend the card after treatment; a residual friction loop "
                "indicates incomplete cleanup or another drag source.",
            ],
            "warning",
        ),
        _entry(
            "BENT_BARREL",
            "Bent barrel",
            "The card shows asymmetric loads with the centroid shifted "
            "toward one end of the stroke, where the plunger binds in the "
            "bend. At the wellsite the load trace shows a repeatable "
            "position-dependent drag and pump efficiency is down.",
            "A bowed or bent barrel pinches the plunger over part of the "
            "stroke, adding localized friction where the bend is and "
            "letting fluid slip where clearance opens up.",
            [
                "Pull the pump and gauge/replace the barrel; a bent barrel "
                "does not recover in service.",
                "Check pump seating and hold-down alignment, and review "
                "wellbore deviation at the pump depth, to find why the "
                "barrel bent.",
                "Consider a shorter barrel or different setting depth if "
                "the pump sits across a dogleg.",
            ],
            "warning",
        ),
        _entry(
            "SAND_ABRASION",
            "Sand abrasion",
            "The card trace is jagged with high-frequency load oscillations "
            "superimposed on the loop. At the wellsite sand shows up in "
            "samples, separators, or the stuffing box, and pump life is "
            "noticeably short.",
            "Produced sand grains between plunger and barrel and through "
            "the valves create stick-slip drag and rapid abrasive wear, "
            "roughening the load signal and cutting the pump.",
            [
                "Add or service sand control: screens, filters, or a sand "
                "separator below the pump intake.",
                "Switch to sand-tolerant pump options (harder metallurgy, "
                "sand-wiper plunger designs, larger clearance where "
                "slippage is acceptable).",
                "Slow the pump and avoid pounding, which pulls sand into "
                "the pump during low-fillage strokes.",
                "Shorten the pump inspection interval while sand production "
                "persists.",
            ],
            "warning",
        ),
        _entry(
            "EXCESSIVE_VIBRATION",
            "Excessive vibration",
            "High-frequency load oscillation rings across the whole card, "
            "not just at load transfer corners. At the wellsite the unit or "
            "wellhead visibly vibrates, and structural bolts or the gearbox "
            "show accelerated wear.",
            "The operating speed is exciting a rod-string or structural "
            "resonance, or a mechanical imbalance/looseness is injecting a "
            "periodic force each revolution.",
            [
                "Change pumping speed to move away from the resonant "
                "frequency; even a small SPM shift can detune the string.",
                "Check counterbalance, crank bolts, bearings, and structural "
                "fasteners for looseness or imbalance.",
                "Verify stuffing box and polished rod alignment; misaligned "
                "surface equipment drives cyclic side loads.",
            ],
            "warning",
        ),
        # ------------------------------------------------------------------
        # Archive / field-library labels
        # ------------------------------------------------------------------
        _entry(
            "FULL_CARD",
            "Full card (full pump fillage)",
            "Archive label for a full rectangular downhole card: complete "
            "barrel fillage with crisp load transfer at both ends of the "
            "stroke. At the wellsite this is the healthy reference state "
            "with production matching displacement.",
            "The pump barrel fills completely with liquid every stroke, so "
            "the full fluid load is carried through the entire upstroke and "
            "released immediately on the downstroke.",
            [
                "No action required; retain as the baseline card for this "
                "well.",
                "Confirm the pump-off controller setpoints leave headroom "
                "around the full-card shape.",
                "Trend fillage so any drift away from full pump is caught "
                "early.",
            ],
            "normal",
        ),
        _entry(
            "INCOMPLETE_FILLAGE",
            "Incomplete fillage",
            "Archive label for partial barrel fillage: the downstroke load "
            "release happens partway through the stroke instead of at the "
            "top. At the wellsite production per stroke is low; the cause "
            "may be gas interference or a pumped-off well and needs to be "
            "distinguished.",
            "The barrel is only partly liquid-filled at the end of the "
            "upstroke, either because free gas occupies part of the barrel "
            "or because inflow cannot keep up with pump displacement.",
            [
                "Distinguish gas from pump-off: a rounded, gradual load "
                "release indicates gas compression, a late sharp drop "
                "indicates fluid pound; shoot a fluid level to confirm "
                "submergence.",
                "If gas: improve separation at the intake and tolerate more "
                "pump-off strokes before shutdown.",
                "If pumped off: slow the unit or tune the pump-off "
                "controller to balance displacement with inflow.",
            ],
            "warning",
        ),
        _entry(
            "FILLAGE_COLLAPSE",
            "Fillage collapse (pump-off progression)",
            "Archive label for a time sequence of cards in which pump "
            "fillage progressively collapses stroke over stroke, from full "
            "pump toward fluid pound. At the wellsite the fluid level is "
            "being drawn down to the pump and impact loading begins as the "
            "sequence progresses.",
            "Pump displacement exceeds inflow, so each stroke removes more "
            "liquid than the reservoir delivers; submergence and fillage "
            "fall together until the plunger starts pounding fluid.",
            [
                "Let the pump-off controller act: shut down or idle on the "
                "fillage setpoint before the well reaches hard fluid pound.",
                "Slow the unit with a variable-speed drive so displacement "
                "tracks inflow and the collapse cycle stops recurring.",
                "Review idle-time settings so the well recovers enough "
                "fluid level between run cycles.",
                "Use the collapse rate to estimate well inflow and re-size "
                "the pumping system if it is chronically oversized.",
            ],
            "warning",
        ),
        _entry(
            "BUTTERFLY",
            "Butterfly card (crossed loop)",
            "Archive label for a crossed, figure-eight downhole card in "
            "which the upstroke and downstroke traces intersect. At the "
            "wellsite this typically appears on deviated wells or on wells "
            "with erratic gas interference, and standard fillage-based "
            "interpretation becomes unreliable.",
            "Position-dependent friction in a deviated wellbore, or erratic "
            "gas compression, distorts the loop so that computed load on "
            "part of the upstroke falls below the downstroke trace, "
            "crossing the card.",
            [
                "Check the deviation survey and wave-equation friction "
                "model first; an unmodeled drag profile commonly produces "
                "crossed cards on deviated wells.",
                "Rule out erratic gas interference with a fluid level shot "
                "and by watching several consecutive cards for shape "
                "instability.",
                "Add rod guides across high-side-load intervals if "
                "deviated-well drag is confirmed.",
                "Treat the classifier output with caution while the card "
                "is crossed; re-run diagnosis after friction is corrected.",
            ],
            "warning",
        ),
        _entry(
            "PUMPED_OFF",
            "Pumped off",
            "Archive label for a well drawn down to the pump: fillage is "
            "low, the load release happens late and sharply on the "
            "downstroke, and cards resemble fluid pound. At the wellsite "
            "the fluid level sits at or near the pump intake and production "
            "per stroke is well below displacement.",
            "The pumping system removes liquid faster than the reservoir "
            "delivers it, so the working fluid level falls to the pump "
            "intake and the barrel can only partly fill each stroke.",
            [
                "Use the pump-off controller to cycle or idle the well at "
                "the fillage setpoint rather than stroking through pound.",
                "Slow the pump with a variable-speed drive to match "
                "displacement to inflow and keep the well pumped down "
                "without pounding.",
                "Re-size stroke, speed, or plunger if the system is "
                "chronically oversized for the well's inflow.",
            ],
            "warning",
        ),
        # ------------------------------------------------------------------
        # Training-deck phenomena
        # ------------------------------------------------------------------
        _entry(
            "FLUMPING",
            "Flumping (flowing while pumping)",
            "The card shape and loads fluctuate stroke to stroke while the "
            "well makes more fluid than pump displacement explains. At the "
            "wellsite the well flows up the tubing (and often the casing "
            "annulus) at the same time the unit pumps, with surging "
            "flowline rates and unstable polished rod loads.",
            "Reservoir energy is sufficient for the well to flow on its "
            "own; the pump and the flowing stream share the tubing, so "
            "pump loading varies with the flowing pressure gradient rather "
            "than a static fluid level.",
            [
                "Handle it with controller limits, not a backpressure "
                "valve: set load-span, card-area, or polished-rod-"
                "horsepower limits so the controller slows or idles the "
                "unit while the well flows.",
                "Let the well flow: allow the variable-speed drive to slow "
                "to minimum or the unit to idle, and let pumping resume "
                "when flow subsides.",
                "Do not install a backpressure valve to force the well "
                "onto pump; it wastes reservoir energy and adds load.",
                "Trend the flowing periods; persistent flumping may mean "
                "the well can be produced with less lift capacity.",
            ],
            "warning",
        ),
        _entry(
            "DEVIATED_WELL_FRICTION",
            "Deviated-well friction",
            "The card is distorted by position-dependent drag: loads shift "
            "up on the upstroke and down on the downstroke by amounts that "
            "vary along the stroke, and the loop may skew or cross. At the "
            "wellsite rod and tubing wear concentrates at dogleg depths and "
            "tubing leaks or rod-body wear failures recur at the same "
            "intervals.",
            "In a deviated wellbore the rods bear against the tubing at "
            "doglegs; Coulomb drag at these contacts opposes rod motion in "
            "each direction, adding a friction load that varies with local "
            "side force along the stroke.",
            [
                "Run the deviation survey through a wave-equation model "
                "with Coulomb friction so downhole cards are corrected for "
                "drag before diagnosis.",
                "Install rod guides or roller guides across the "
                "high-side-load intervals identified by the survey.",
                "Consider a rod rotator to distribute wear around the rod "
                "circumference.",
                "Review failure records by depth; recurring wear at one "
                "dogleg justifies re-spacing guides or redesigning the "
                "string.",
            ],
            "warning",
        ),
        _entry(
            "HIGH_FLUID_LEVEL",
            "High fluid level (under-pumped well)",
            "The card is full and healthy-looking but the fluid load is "
            "low for the pump depth, because high submergence buoys the "
            "differential across the plunger. At the wellsite fluid level "
            "shots show a tall liquid column over the pump and the well is "
            "producing below its potential.",
            "The pump is not drawing the well down: displacement is less "
            "than available inflow, so the working fluid level stays high, "
            "bottomhole pressure stays high, and reservoir inflow is "
            "choked back.",
            [
                "Increase displacement: raise SPM (or the variable-speed "
                "drive ceiling), lengthen the stroke, or upsize the "
                "plunger to draw the fluid level down.",
                "Verify the pump is actually performing (no slippage or "
                "valve leak) before adding capacity; a weak pump also "
                "leaves a high fluid level.",
                "Re-run inflow calculations with the drawn-down level to "
                "capture the production gain and confirm the new operating "
                "point.",
            ],
            "warning",
        ),
        _entry(
            "HEAVY_OIL",
            "Heavy oil (viscous friction card)",
            "The card is inflated by viscous drag: high upstroke loads, "
            "low downstroke loads, and rounded corners as valves respond "
            "slowly. At the wellsite the rods may fall too slowly for the "
            "unit on the downstroke (rod float), power draw is high, and "
            "the produced crude is visibly viscous or emulsified.",
            "Viscous fluid resists rod and plunger motion and delays valve "
            "opening and closing, adding a velocity-dependent friction "
            "load in the direction opposing motion throughout the stroke.",
            [
                "Slow the pumping speed so the rods can fall through the "
                "viscous fluid without the carrier bar outrunning them.",
                "Use viscosity-rated pump clearances (looser plunger fit) "
                "and valve/cage designs suited to heavy crude.",
                "Reduce effective viscosity where economic: diluent "
                "injection, chemical emulsion treatment, or wellbore "
                "heating.",
                "Watch for rod float on the downstroke and let the "
                "controller or variable-speed drive limit speed before "
                "rods buckle or unseat.",
            ],
            "warning",
        ),
        _entry(
            "BARREL_HOLE_SPLIT",
            "Holed or split pump barrel",
            "The card loses load-carrying ability abruptly or erratically: "
            "the upstroke load collapses partway through the stroke where "
            "the plunger passes the damaged section, and card area drops "
            "sharply. At the wellsite production falls suddenly and does "
            "not respond to speed changes.",
            "A hole or split in the barrel wall lets pressurized fluid "
            "escape past the plunger when it traverses the damaged "
            "interval, dumping the fluid load and short-circuiting "
            "displacement.",
            [
                "Confirm the pump is the problem with valve checks: both "
                "valves may test acceptably while the barrel itself leaks "
                "at a specific plunger position.",
                "Pull the pump and replace the barrel; a holed or split "
                "barrel is not repairable in the well.",
                "Inspect for the root cause: corrosion pitting, sand "
                "erosion, or a burst from pumping against a closed valve, "
                "and correct it before rerunning.",
                "Upgrade barrel metallurgy or add corrosion inhibition if "
                "wall loss caused the breach.",
            ],
            "critical",
        ),
    ]
}


def guide_for(phenomenon: str) -> TroubleshootingEntry:
    """Return the troubleshooting entry for a phenomenon key.

    Args:
        phenomenon: Catalog key, e.g. ``"GAS_INTERFERENCE"``.

    Returns:
        The matching :class:`TroubleshootingEntry`.

    Raises:
        KeyError: If the phenomenon is not in the catalog; the message
            lists all valid keys.
    """
    try:
        return TROUBLESHOOTING_GUIDE[phenomenon]
    except KeyError:
        valid = ", ".join(sorted(TROUBLESHOOTING_GUIDE))
        raise KeyError(
            f"Unknown phenomenon {phenomenon!r}. Valid keys: {valid}"
        ) from None
