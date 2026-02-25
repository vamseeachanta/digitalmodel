# Drilling Automation Capabilities

### From ROP Optimisation to Autonomous Well Delivery -- H1 Roadmap and Drillbotics Positioning

---

## The Challenge

Well delivery in complex drilling environments demands real-time decisions across multiple
interdependent physical domains simultaneously -- rate of penetration, wellbore pressure,
drillstring mechanics, formation response, and directional control. Current practice imposes
friction at every interface:

- **No integrated ROP model** -- formation-dependent WOB/RPM optimisation relies on
  manual engineer judgement rather than physics-based prediction
- **Fragmented hydraulics** -- ECD and pressure-window management is computed in
  disconnected tools with no continuous control loop
- **Manual torque and drag accounting** -- drillstring load analysis done
  post-run rather than in real time to prevent stuck pipe or buckling
- **No autonomous control layer** -- each phase of the well requires human initiation
  of the next decision cycle, breaking the continuous Sense-Plan-Act loop
- **Industry gap** -- the SPE Drillbotics Mode V competition defines the target state
  explicitly: a Python-based autonomous drilling system that drills a 3D directional well
  to blind targets within three hours, with no human initiation of any intermediate step

Engineering teams need a modular drilling automation capability that builds toward the
autonomous production vision incrementally -- with each module delivering standalone
client value before it is wired into the autonomous loop.

---

## The Solution

**ACE Engineering Drilling Automation** is a modular, phase-by-phase build-out of the
core drilling engineering computation library within `digitalmodel`, designed to close the
H1 capability gaps and establish the foundation for a full autonomous drilling workflow.

```
  Drilling Signals (WOB, RPM, ROP, ECD, Torque, Flow)
        |
        v
  +------------------------------+
  |   Formation Classification    |  Real-time ML identification
  |   (ML -- Rock type from       |  of formation from surface
  |    surface drilling signals)  |  drilling signals
  +------------------------------+
        |
        v
  +------------------------------+
  |   ROP Optimisation           |  Bourgoyne-Young 8-parameter
  |   (WOB / RPM controller)     |  model + Warren simplified
  |                              |  model; nonlinear constrained
  +------------------------------+  optimisation
        |
        v
  +------------------------------+
  |   Wellbore Hydraulics / ECD  |  Equivalent Circulating
  |   (Pressure Window Control)  |  Density, annular velocity,
  |                              |  pump pressure, MPD control
  +------------------------------+
        |
        v
  +------------------------------+
  |   Torque and Drag            |  Soft-string T&D model;
  |   (Drillstring Mechanics)    |  WOB/tension management,
  |                              |  stuck-pipe prevention
  +------------------------------+
        |
        v
  +------------------------------+
  |   Well Control               |  Kick detection (flow-in/out,
  |   (Kick Detection / Shut-in) |  pit volume, standpipe
  |                              |  pressure); automated shut-in
  +------------------------------+
        |
        v
  Autonomous Drilling Controller (H2 -- Sense-Plan-Act loop)
```

---

## Capability Roadmap

### H1 -- Now to 6 Months (Standalone Client Value)

These capabilities close real `digitalmodel` gaps that affect current client well
engineering projects, independent of the autonomous drilling goal:

| Module | Description | Method | Client Use |
|--------|-------------|--------|-----------|
| **ROP Model** | Formation-dependent rate of penetration prediction | Bourgoyne-Young 8-parameter + Warren 2-parameter | Well programme optimisation; WOB/RPM selection |
| **Wellbore Hydraulics** | ECD, pressure drop, annular velocity, pump pressure | Fanning friction + acceleration components; generalised from existing CT module | MPD window management; casing seat design |
| **Torque and Drag** | Axial and torsional loads along the wellbore trajectory | Soft-string model | Stuck-pipe risk assessment; torque limits |

The wellbore hydraulics module extends existing coiled-tubing hydraulics capability
(`digitalmodel/src/digitalmodel/marine_ops/ct_hydraulics/ct_hydraulics.py`) to
arbitrary pipe/annulus diameters and multi-phase flow -- approximately 250 lines
of generalisation on proven code.

### H2 -- 6 to 18 Months (Autonomous Workflow Assembly)

| Module | Description |
|--------|-------------|
| **3D Trajectory Planner** | Cubic Bezier / minimum-curvature 3D directional well-path computation; DLS constraints; survey uncertainty propagation |
| **Drilling Controller** | Closed-loop WOB/RPM/flow optimisation using PID transitioning to Model Predictive Control (MPC) |
| **Well Control Module** | Kick detection and automated shut-in; false-alarm rate tracking |
| **Formation Classification** | Real-time ML identification of rock type from surface drilling signals |

### H3 -- 18+ Months (Full Autonomous Loop)

| Deliverable | Description |
|-------------|-------------|
| **Mode V Capable Agent** | Full Sense-Plan-Act drilling agent: receives targets, plans 3D trajectory, autonomously drills to TD, detects and handles well control events, reports results -- with no human initiation of any intermediate step |
| **D-WIS Semantic Layer** | Drilling Well Information Schema (D-WIS) interoperability for all modules; enables integration with any D-WIS-compliant surface system |

---

## SPE Drillbotics as an External Benchmark

**SPE Drillbotics Mode V** (virtual track) is the industry's most concrete definition
of Level 4 autonomous drilling. Competing university teams build a Python-based multi-module
drilling simulator that must drill a 3D directional well to targets revealed only on
competition day -- within three hours, no pre-scripted sequences.

The Mode V required modules map exactly to ACE Engineering's H1/H2 capability roadmap:
trajectory planner, ROP optimiser, T&D model, wellbore hydraulics/ECD, well control, and
formation classification.

ACE Engineering's engagement mode is **industry mentor and open-source contributor** --
not a competitor (students-only). H1 capability builds (ROP model, hydraulics) contributed
to the Open Source Drilling Community GitHub are a natural fit and increase technical
credibility in the SPE community.

Published reference papers (IADC/SPE Drilling Conference):
- Autonomous Directional Drilling Using RSS Simulator (2022, 2023, 2024, 2025)
- All available on OnePetro

---

## Existing Capability Foundations

ACE Engineering already has the infrastructure that H1 modules plug into:

| Foundation | Location | Relevance |
|------------|----------|-----------|
| **704+ Python modules** | `digitalmodel` | Simulation infrastructure and testing harness |
| **Coiled-tubing hydraulics** | `digitalmodel/src/digitalmodel/marine_ops/ct_hydraulics/` | Generalises to open-hole hydraulics |
| **`drilling-expert` agent** | `worldenergydata` | Domain knowledge; well planning, optimisation, safety |
| **AI orchestration layer** | `workspace-hub` | Multi-agent framework for autonomous workflow assembly |
| **Interactive reporting** | `digitalmodel`, `assethold` | Plotly dashboards and HTML engineering reports |

---

## Integration with the Autonomous Production Vision

The drilling automation modules are H1 and H2 deliverables in the ACE Engineering
Sense-Plan-Act roadmap (`docs/vision/VISION.md`). A working Mode V drilling agent (H2)
would be the first fully autonomous engineering workflow in the ACE Engineering ecosystem
-- validated against externally-published scoring criteria, not just internal acceptance tests.

```
SENSE  → formation readings, WOB, RPM, torque, ECD, flow-in/out (simulator outputs)
PLAN   → trajectory controller + ROP optimiser compute next parameter set
ACT    → send WOB/RPM/flow commands; detect dysfunctions; escalate kicks to HMI
```

---

## Deliverable Format

All modules ship as:
- Python library functions with full pytest test coverage
- YAML-configurable inputs (ASCII-first, version-controlled)
- Plotly-based interactive HTML reports
- Engineering validation against industry-standard reference cases

---

*Source: WRK-375 (Drillbotics strategy), WRK-373 (autonomous-production vision) | 2026-02-24*
*Strategy detail: `docs/strategy/drillbotics-engagement.md`*
*Skill reference: `.claude/skills/engineering/drilling/drillbotics/SKILL.md`*
