"""Foam/fire-suppression system sizing — deterministic screening calc.

Pure functions and dataclasses for:

1. **Foam solution demand**: protected areas x application rate, with the
   application-rate criteria supplied as a config table keyed by
   standard+edition. Every criterion MUST carry a citation (standard,
   edition, clause) — this module refuses uncited rate values so that a
   run is always traceable to the code edition the user selected.
2. **Foam concentrate quantity**: demand x discharge time x concentration
   plus a reserve policy (e.g. 100% reserve per common flag/class
   practice — the policy value itself comes from config).
3. **Proportioner duty check**: design solution flow and concentrate
   injection rate against the proportioner's rated envelope.
4. **Hydraulic screening** of a simple tree distribution network:
   Hazen-Williams or Darcy-Weisbach (Swamee-Jain) pipe-run pressure drop,
   static head to each terminal, required pump head and margin.

SCREENING POSTURE: this is a screening/sizing sanity calc, not a detail
design. It assumes a tree (no loops), water-like solution hydraulics,
fittings via equivalent lengths, and no proportioner/eductor pressure-loss
modelling beyond the rated envelope check. Final foam-system designs are
governed by the authority having jurisdiction / classification society
review against the applicable edition of the referenced standards
(e.g. NFPA 11, SOLAS II-2 / FSS Code, ABS MODU rules).

Units: SI — m2, L/min, minutes, m, mm, bar; heads in metres of foam
solution (density from config, default 1000 kg/m3).
"""

from __future__ import annotations

import math
from dataclasses import dataclass

G_M_S2 = 9.80665
"""Standard gravity [m/s2]."""

BAR_TO_PA = 1.0e5


# -- citations ---------------------------------------------------------------


@dataclass(frozen=True)
class Citation:
    """Traceability record for a criteria value.

    All three of ``standard``, ``edition`` and ``clause`` are mandatory —
    an application rate without them is not usable in a deliverable.
    """

    standard: str
    edition: str
    clause: str
    note: str = ""

    def __post_init__(self) -> None:
        for name in ("standard", "edition", "clause"):
            if not str(getattr(self, name)).strip():
                raise ValueError(f"citation.{name} must be a non-empty string")

    def label(self) -> str:
        text = f"{self.standard} ({self.edition}) {self.clause}"
        return f"{text} — {self.note}" if self.note else text


# -- foam demand -------------------------------------------------------------


@dataclass(frozen=True)
class FoamCriterion:
    """One row of the application-rate criteria table (config-supplied)."""

    key: str
    application_rate_lpm_per_m2: float
    discharge_time_min: float
    citation: Citation

    def __post_init__(self) -> None:
        if self.application_rate_lpm_per_m2 <= 0.0:
            raise ValueError(
                f"criterion '{self.key}': application_rate_lpm_per_m2 must be > 0"
            )
        if self.discharge_time_min <= 0.0:
            raise ValueError(f"criterion '{self.key}': discharge_time_min must be > 0")


@dataclass(frozen=True)
class ProtectedArea:
    name: str
    area_m2: float
    criterion: FoamCriterion

    def __post_init__(self) -> None:
        if self.area_m2 <= 0.0:
            raise ValueError(f"protected area '{self.name}': area_m2 must be > 0")

    @property
    def solution_flow_lpm(self) -> float:
        return self.area_m2 * self.criterion.application_rate_lpm_per_m2

    @property
    def solution_volume_l(self) -> float:
        return self.solution_flow_lpm * self.criterion.discharge_time_min


@dataclass(frozen=True)
class HoseStream:
    """Supplementary hose-stream allowance running concurrently."""

    name: str
    flow_lpm: float
    count: int = 1
    duration_min: float = 0.0
    citation: Citation | None = None

    def __post_init__(self) -> None:
        if self.flow_lpm <= 0.0 or self.count < 1:
            raise ValueError(f"hose stream '{self.name}': flow_lpm > 0, count >= 1")

    @property
    def total_flow_lpm(self) -> float:
        return self.flow_lpm * self.count


@dataclass(frozen=True)
class FoamDemandResult:
    per_area: tuple[ProtectedArea, ...]
    demand_policy: str
    governing_area: str
    governing_area_flow_lpm: float
    hose_flow_lpm: float
    design_solution_flow_lpm: float


def foam_demand(
    areas: list[ProtectedArea],
    hose_streams: list[HoseStream] | None = None,
    demand_policy: str = "max",
) -> FoamDemandResult:
    """Foam solution demand from protected areas + hose-stream allowance.

    ``demand_policy``:

    - ``max``: single largest protected-area demand governs (areas are
      alternative fire scenarios, one at a time);
    - ``sum``: all protected areas discharge simultaneously.

    Hose streams are always added on top of the governing area demand.
    """
    if not areas:
        raise ValueError("at least one protected area is required")
    if demand_policy not in ("max", "sum"):
        raise ValueError("demand_policy must be 'max' or 'sum'")
    hose_streams = hose_streams or []
    if demand_policy == "max":
        governing = max(areas, key=lambda a: a.solution_flow_lpm)
        area_flow = governing.solution_flow_lpm
        governing_name = governing.name
    else:
        area_flow = sum(a.solution_flow_lpm for a in areas)
        governing_name = "all areas (simultaneous)"
    hose_flow = sum(h.total_flow_lpm for h in hose_streams)
    return FoamDemandResult(
        per_area=tuple(areas),
        demand_policy=demand_policy,
        governing_area=governing_name,
        governing_area_flow_lpm=area_flow,
        hose_flow_lpm=hose_flow,
        design_solution_flow_lpm=area_flow + hose_flow,
    )


# -- concentrate quantity ----------------------------------------------------


@dataclass(frozen=True)
class ConcentrateResult:
    concentration_percent: float
    reserve_percent: float
    area_concentrate_l: float
    hose_concentrate_l: float
    base_concentrate_l: float
    reserve_concentrate_l: float
    total_concentrate_l: float
    injection_rate_lpm: float


def concentrate_quantity(
    demand: FoamDemandResult,
    concentration_percent: float,
    reserve_percent: float = 0.0,
    hose_streams: list[HoseStream] | None = None,
) -> ConcentrateResult:
    """Foam concentrate quantity: demand x discharge time x concentration
    plus a reserve policy.

    The governing-area concentrate follows ``demand.demand_policy``: with
    ``max`` the largest single-area *volume* (flow x its own discharge
    time) is used; with ``sum`` all area volumes add. Hose-stream
    concentrate is flow x count x duration on top. Reserve applies to the
    whole base quantity (main + hose).
    """
    if not 0.0 < concentration_percent < 100.0:
        raise ValueError("concentration_percent must be in (0, 100)")
    if reserve_percent < 0.0:
        raise ValueError("reserve_percent must be >= 0")
    fraction = concentration_percent / 100.0
    volumes = [a.solution_volume_l for a in demand.per_area]
    if demand.demand_policy == "max":
        area_solution_l = max(volumes)
    else:
        area_solution_l = sum(volumes)
    area_concentrate = area_solution_l * fraction
    hose_concentrate = sum(
        hose.total_flow_lpm * hose.duration_min * fraction
        for hose in (hose_streams or [])
    )
    base = area_concentrate + hose_concentrate
    reserve = base * reserve_percent / 100.0
    return ConcentrateResult(
        concentration_percent=concentration_percent,
        reserve_percent=reserve_percent,
        area_concentrate_l=area_concentrate,
        hose_concentrate_l=hose_concentrate,
        base_concentrate_l=base,
        reserve_concentrate_l=reserve,
        total_concentrate_l=base + reserve,
        injection_rate_lpm=demand.design_solution_flow_lpm * fraction,
    )


# -- proportioner duty check -------------------------------------------------


@dataclass(frozen=True)
class Proportioner:
    """Rated envelope of the foam proportioner (vendor data)."""

    min_solution_flow_lpm: float
    max_solution_flow_lpm: float
    max_concentrate_flow_lpm: float | None = None
    rated_concentration_percent: float | None = None

    def __post_init__(self) -> None:
        if not 0.0 <= self.min_solution_flow_lpm < self.max_solution_flow_lpm:
            raise ValueError(
                "proportioner needs 0 <= min_solution_flow_lpm < max_solution_flow_lpm"
            )


@dataclass(frozen=True)
class ProportionerCheck:
    design_solution_flow_lpm: float
    injection_rate_lpm: float
    utilization_percent: float
    solution_flow_ok: bool
    concentrate_flow_ok: bool
    concentration_match_ok: bool
    ok: bool
    messages: tuple[str, ...]


def proportioner_check(
    proportioner: Proportioner,
    design_solution_flow_lpm: float,
    injection_rate_lpm: float,
    concentration_percent: float | None = None,
) -> ProportionerCheck:
    """Screen the design point against the proportioner rated envelope."""
    messages: list[str] = []
    flow_ok = (
        proportioner.min_solution_flow_lpm
        <= design_solution_flow_lpm
        <= proportioner.max_solution_flow_lpm
    )
    if not flow_ok:
        messages.append(
            f"design solution flow {design_solution_flow_lpm:.0f} L/min outside "
            f"proportioner range [{proportioner.min_solution_flow_lpm:.0f}, "
            f"{proportioner.max_solution_flow_lpm:.0f}] L/min"
        )
    conc_ok = True
    if proportioner.max_concentrate_flow_lpm is not None:
        conc_ok = injection_rate_lpm <= proportioner.max_concentrate_flow_lpm
        if not conc_ok:
            messages.append(
                f"concentrate injection {injection_rate_lpm:.1f} L/min exceeds "
                f"proportioner limit {proportioner.max_concentrate_flow_lpm:.1f} L/min"
            )
    match_ok = True
    if (
        proportioner.rated_concentration_percent is not None
        and concentration_percent is not None
    ):
        match_ok = math.isclose(
            proportioner.rated_concentration_percent,
            concentration_percent,
            rel_tol=1e-9,
            abs_tol=1e-9,
        )
        if not match_ok:
            messages.append(
                f"concentrate is {concentration_percent:g}% but proportioner is "
                f"rated for {proportioner.rated_concentration_percent:g}%"
            )
    return ProportionerCheck(
        design_solution_flow_lpm=design_solution_flow_lpm,
        injection_rate_lpm=injection_rate_lpm,
        utilization_percent=100.0
        * design_solution_flow_lpm
        / proportioner.max_solution_flow_lpm,
        solution_flow_ok=flow_ok,
        concentrate_flow_ok=conc_ok,
        concentration_match_ok=match_ok,
        ok=flow_ok and conc_ok and match_ok,
        messages=tuple(messages),
    )


# -- pipe hydraulics ---------------------------------------------------------


def pipe_velocity_m_s(flow_lpm: float, diameter_mm: float) -> float:
    if diameter_mm <= 0.0:
        raise ValueError("diameter_mm must be > 0")
    q_m3s = flow_lpm / 60000.0
    area = math.pi * (diameter_mm / 1000.0) ** 2 / 4.0
    return q_m3s / area


def hazen_williams_headloss_m(
    flow_lpm: float,
    length_m: float,
    diameter_mm: float,
    c_factor: float = 120.0,
) -> float:
    """Hazen-Williams friction loss [m], SI form.

    ``h_f = 10.67 * L * Q^1.852 / (C^1.852 * d^4.87)`` with Q in m3/s and
    d in m. Valid for water-like fluids in turbulent flow — adequate for
    screening a foam-solution (>= ~94% water) network.
    """
    if flow_lpm < 0.0 or length_m < 0.0:
        raise ValueError("flow_lpm and length_m must be >= 0")
    if diameter_mm <= 0.0 or c_factor <= 0.0:
        raise ValueError("diameter_mm and c_factor must be > 0")
    if flow_lpm == 0.0 or length_m == 0.0:
        return 0.0
    q_m3s = flow_lpm / 60000.0
    d_m = diameter_mm / 1000.0
    return 10.67 * length_m * q_m3s**1.852 / (c_factor**1.852 * d_m**4.87)


def darcy_weisbach_headloss_m(
    flow_lpm: float,
    length_m: float,
    diameter_mm: float,
    roughness_mm: float = 0.045,
    kinematic_viscosity_m2_s: float = 1.0e-6,
) -> float:
    """Darcy-Weisbach friction loss [m]; Swamee-Jain explicit friction
    factor for turbulent flow, ``f = 64/Re`` for laminar (Re < 2300)."""
    if flow_lpm < 0.0 or length_m < 0.0:
        raise ValueError("flow_lpm and length_m must be >= 0")
    if diameter_mm <= 0.0 or roughness_mm < 0.0 or kinematic_viscosity_m2_s <= 0.0:
        raise ValueError("diameter_mm > 0, roughness_mm >= 0, viscosity > 0 required")
    if flow_lpm == 0.0 or length_m == 0.0:
        return 0.0
    d_m = diameter_mm / 1000.0
    velocity = pipe_velocity_m_s(flow_lpm, diameter_mm)
    reynolds = velocity * d_m / kinematic_viscosity_m2_s
    if reynolds < 2300.0:
        friction = 64.0 / reynolds
    else:
        rel_rough = (roughness_mm / 1000.0) / d_m
        friction = 0.25 / math.log10(rel_rough / 3.7 + 5.74 / reynolds**0.9) ** 2
    return friction * (length_m / d_m) * velocity**2 / (2.0 * G_M_S2)


# -- tree-network screening --------------------------------------------------


@dataclass(frozen=True)
class PipeRun:
    from_node: str
    to_node: str
    length_m: float
    diameter_mm: float
    equivalent_length_m: float = 0.0

    def __post_init__(self) -> None:
        if self.length_m < 0.0 or self.equivalent_length_m < 0.0:
            raise ValueError(
                f"run {self.from_node}->{self.to_node}: lengths must be >= 0"
            )
        if self.diameter_mm <= 0.0:
            raise ValueError(
                f"run {self.from_node}->{self.to_node}: diameter_mm must be > 0"
            )


@dataclass(frozen=True)
class Terminal:
    """A discharge point (monitor, foam chamber, hose header)."""

    node: str
    flow_lpm: float
    required_pressure_bar: float
    elevation_m: float = 0.0

    def __post_init__(self) -> None:
        if self.flow_lpm <= 0.0 or self.required_pressure_bar < 0.0:
            raise ValueError(
                f"terminal '{self.node}': flow_lpm > 0 and "
                "required_pressure_bar >= 0 required"
            )


@dataclass(frozen=True)
class Pump:
    node: str
    rated_flow_lpm: float
    rated_head_m: float
    elevation_m: float = 0.0

    def __post_init__(self) -> None:
        if self.rated_flow_lpm <= 0.0 or self.rated_head_m <= 0.0:
            raise ValueError("pump rated_flow_lpm and rated_head_m must be > 0")


@dataclass(frozen=True)
class RunResult:
    from_node: str
    to_node: str
    flow_lpm: float
    velocity_m_s: float
    headloss_m: float


@dataclass(frozen=True)
class TerminalResult:
    node: str
    flow_lpm: float
    path: tuple[str, ...]
    friction_headloss_m: float
    static_head_m: float
    pressure_head_m: float
    required_pump_head_m: float


@dataclass(frozen=True)
class HydraulicsResult:
    method: str
    runs: tuple[RunResult, ...]
    terminals: tuple[TerminalResult, ...]
    total_flow_lpm: float
    governing_terminal: str
    required_pump_head_m: float
    pump_head_margin_m: float
    pump_head_margin_percent: float
    pump_flow_margin_lpm: float
    pump_head_ok: bool
    pump_flow_ok: bool
    max_velocity_m_s: float


def solve_tree_network(
    pump: Pump,
    runs: list[PipeRun],
    terminals: list[Terminal],
    method: str = "hazen_williams",
    hazen_williams_c: float = 120.0,
    roughness_mm: float = 0.045,
    kinematic_viscosity_m2_s: float = 1.0e-6,
    density_kg_m3: float = 1000.0,
) -> HydraulicsResult:
    """Screen a simple tree distribution network.

    Every run's flow is the sum of the terminal flows downstream of it
    (single-scenario, all listed terminals discharging simultaneously).
    Required pump head per terminal = path friction + static lift +
    terminal pressure converted to head; the worst terminal governs.
    """
    if method not in ("hazen_williams", "darcy_weisbach"):
        raise ValueError("hydraulics method must be 'hazen_williams' or 'darcy_weisbach'")
    if not runs or not terminals:
        raise ValueError("hydraulics needs at least one pipe run and one terminal")
    if density_kg_m3 <= 0.0:
        raise ValueError("density_kg_m3 must be > 0")

    incoming: dict[str, PipeRun] = {}
    children: dict[str, list[PipeRun]] = {}
    for run in runs:
        if run.to_node == pump.node:
            raise ValueError(f"run {run.from_node}->{run.to_node} feeds the pump node")
        if run.to_node in incoming:
            raise ValueError(
                f"node '{run.to_node}' has two feeds — network must be a tree"
            )
        incoming[run.to_node] = run
        children.setdefault(run.from_node, []).append(run)

    # cycle / connectivity check: walk from the pump
    reachable: set[str] = set()
    stack = [pump.node]
    while stack:
        node = stack.pop()
        if node in reachable:
            raise ValueError(f"cycle detected at node '{node}' — network must be a tree")
        reachable.add(node)
        stack.extend(run.to_node for run in children.get(node, []))
    for run in runs:
        if run.from_node not in reachable:
            raise ValueError(
                f"run {run.from_node}->{run.to_node} is not connected to the pump"
            )

    terminal_flows: dict[str, float] = {}
    for terminal in terminals:
        if terminal.node not in reachable:
            raise ValueError(f"terminal '{terminal.node}' is not connected to the pump")
        if terminal.node in terminal_flows:
            raise ValueError(f"terminal '{terminal.node}' listed twice")
        terminal_flows[terminal.node] = terminal.flow_lpm

    def subtree_flow(node: str) -> float:
        flow = terminal_flows.get(node, 0.0)
        for run in children.get(node, []):
            flow += subtree_flow(run.to_node)
        return flow

    def headloss(run: PipeRun, flow_lpm: float) -> float:
        length = run.length_m + run.equivalent_length_m
        if method == "hazen_williams":
            return hazen_williams_headloss_m(
                flow_lpm, length, run.diameter_mm, hazen_williams_c
            )
        return darcy_weisbach_headloss_m(
            flow_lpm, length, run.diameter_mm, roughness_mm, kinematic_viscosity_m2_s
        )

    run_results: dict[str, RunResult] = {}
    for run in runs:
        flow = subtree_flow(run.to_node)
        run_results[run.to_node] = RunResult(
            from_node=run.from_node,
            to_node=run.to_node,
            flow_lpm=flow,
            velocity_m_s=pipe_velocity_m_s(flow, run.diameter_mm),
            headloss_m=headloss(run, flow),
        )

    bar_to_m = BAR_TO_PA / (density_kg_m3 * G_M_S2)
    terminal_results: list[TerminalResult] = []
    for terminal in terminals:
        path: list[str] = []
        friction = 0.0
        node = terminal.node
        while node != pump.node:
            result = run_results[node]
            friction += result.headloss_m
            path.append(node)
            node = result.from_node
        path.append(pump.node)
        path.reverse()
        static = terminal.elevation_m - pump.elevation_m
        pressure_head = terminal.required_pressure_bar * bar_to_m
        terminal_results.append(
            TerminalResult(
                node=terminal.node,
                flow_lpm=terminal.flow_lpm,
                path=tuple(path),
                friction_headloss_m=friction,
                static_head_m=static,
                pressure_head_m=pressure_head,
                required_pump_head_m=friction + static + pressure_head,
            )
        )

    governing = max(terminal_results, key=lambda t: t.required_pump_head_m)
    total_flow = sum(t.flow_lpm for t in terminals)
    margin = pump.rated_head_m - governing.required_pump_head_m
    return HydraulicsResult(
        method=method,
        runs=tuple(run_results[run.to_node] for run in runs),
        terminals=tuple(terminal_results),
        total_flow_lpm=total_flow,
        governing_terminal=governing.node,
        required_pump_head_m=governing.required_pump_head_m,
        pump_head_margin_m=margin,
        pump_head_margin_percent=100.0 * margin / pump.rated_head_m,
        pump_flow_margin_lpm=pump.rated_flow_lpm - total_flow,
        pump_head_ok=margin >= 0.0,
        pump_flow_ok=pump.rated_flow_lpm >= total_flow,
        max_velocity_m_s=max(r.velocity_m_s for r in run_results.values()),
    )
