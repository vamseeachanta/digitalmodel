# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Build the offline power analysis report from calls to the public module API.

Run from the repository root with PYTHONPATH=src and NumPy available.
All worked-example values are bound to the calculation record; no result is
transcribed into the narrative. Engineering limits come from the open case.
"""

from __future__ import annotations

import argparse
import json
import math
import re
from html import escape
from pathlib import Path

from digitalmodel.power.analysis.load_flow import (
    Branch, Bus, BusType, Network, solve_power_flow,
)

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
TOL = 1e-12  # Explicit numerical criterion, not an engineering acceptance limit.
MAX_ITER = 50  # Explicit cap, matching solve_power_flow's documented default.


def read_case():
    """Read the transcribed, cited public circuit inputs."""
    return json.loads((HERE / "public_case.json").read_text(encoding="utf-8"))


def public_network(case):
    """Map the selected supported fields; zero case tap means unity ratio."""
    kinds = {1: BusType.PQ, 2: BusType.PV, 3: BusType.SLACK}
    gens = {row["bus"]: row for row in case["generators"]}
    base = case["base_mva"]
    buses = []
    for row in case["buses"]:
        gen = gens.get(row["id"], {})
        buses.append(Bus(
            row["id"], kinds[row["type"]],
            voltage_pu=gen.get("v_set", row["v_initial"]),
            angle_rad=math.radians(row["angle_deg"]),
            p_gen_pu=gen.get("p_mw", 0) / base,
            q_gen_pu=gen.get("q_initial_mvar", 0) / base,
            p_load_pu=row["p_mw"] / base, q_load_pu=row["q_mvar"] / base,
            v_min_pu=row["v_min"], v_max_pu=row["v_max"],
        ))
    branches = [Branch(row["from"], row["to"], row["r_pu"], row["x_pu"],
                       b_pu=row["b_pu"], tap_ratio=row["tap_raw"] or 1.0)
                for row in case["branches"]]
    return Network(buses, branches, base)


def hand_case():
    """Call the module on the repository's documented rectangular-voltage case."""
    network = Network(
        [Bus(1, BusType.SLACK), Bus(2, BusType.PV, p_gen_pu=2.0288),
         Bus(3, BusType.PQ, p_load_pu=1.4688, q_load_pu=0.024)],
        [Branch(1, 2, 0, 0.5), Branch(2, 3, 0, 0.25)], 100.0,
    )
    return solve_power_flow(network, tol=TOL, max_iter=MAX_ITER)


def calculate():
    """Return flattened inputs and unrounded outputs for numerical bindings."""
    case = read_case()
    net = public_network(case)
    public = solve_power_flow(net, tol=TOL, max_iter=MAX_ITER)
    hand = hand_case()
    if not public.converged or not hand.converged:
        raise ValueError("A report circuit has not converged; inspect the input basis.")
    record = {"tol": TOL, "max_iter": MAX_ITER, "base_mva": case["base_mva"]}
    for group, result in [("public", public), ("hand", hand)]:
        record[f"{group}.converged"] = result.converged
        record[f"{group}.iterations"] = result.iterations
        for kind, rows in [("bus", result.bus_results), ("branch", result.branch_results)]:
            for i, row in enumerate(rows):
                for key, value in row.items():
                    record[f"{group}.{kind}.{i}.{key}"] = value
    for kind in ["buses", "generators", "branches"]:
        for i, row in enumerate(case[kind]):
            for key, value in row.items():
                record[f"input.{kind}.{i}.{key}"] = value
    record["public.qg4_mvar"] = public.bus_results[3]["q_net_pu"] * net.base_mva + case["buses"][3]["q_mvar"]
    record["public.q_excess_mvar"] = record["public.qg4_mvar"] - case["generators"][0]["q_max_mvar"]
    record["public.q_excess_pct"] = record["public.q_excess_mvar"] / case["generators"][0]["q_max_mvar"] * 100
    record["public.qg1_mvar"] = public.bus_results[0]["q_net_pu"] * net.base_mva + case["buses"][0]["q_mvar"]
    record["public.q_excess1_mvar"] = record["public.qg1_mvar"] - case["generators"][1]["q_max_mvar"]
    record["public.q_excess1_pct"] = record["public.q_excess1_mvar"] / case["generators"][1]["q_max_mvar"] * 100
    for i, flow in enumerate(public.branch_results):
        record[f"public.branch.{i}.sending_mva"] = math.hypot(flow["p_flow_pu"], flow["q_flow_pu"]) * net.base_mva
        # Receiving power follows from the reported sum of terminal powers.
        record[f"public.branch.{i}.receiving_mva"] = math.hypot(
            flow["p_loss_pu"] - flow["p_flow_pu"],
            flow["q_loss_pu"] - flow["q_flow_pu"],
        ) * net.base_mva
    record["public.newton_updates"] = public.iterations - 1  # Final converged mismatch check has no update.
    record["public.voltage_bounds_satisfied"] = all(
        case["buses"][i]["v_min"] <= row["v_pu"] <= case["buses"][i]["v_max"]
        for i, row in enumerate(public.bus_results)
    )
    record["public.branch_ratings_satisfied"] = all(
        record[f"public.branch.{i}.{end}_mva"] <= row["rating_mva"]
        for i, row in enumerate(case["branches"]) for end in ["sending", "receiving"]
    )
    record["public.total_loss_mw"] = sum(row["p_loss_pu"] for row in public.branch_results) * net.base_mva
    return record


def format_number(value, digits):
    """Use engineering precision, true minus and no negative rounded zero."""
    if 0 < abs(value) < 1e-6:
        return f"{value:.1e}".replace("-", "−")
    rounded = round(value, digits)
    if rounded == 0 and value != 0:
        raise ValueError("A nonzero result needs more displayed digits.")
    if rounded == 0:
        rounded = 0.0
    return f"{rounded:.{digits}f}".replace("-", "−")


def build(destination=None):
    """Write the self-contained report; destinations outside the tree are allowed."""
    r = calculate()
    case = read_case()

    def n(key, digits=6):
        return f'<span data-value="{key}" data-digits="{digits}">{format_number(r[key], digits)}</span>'

    def table(headings, rows):
        return '<table><thead><tr>' + ''.join(f'<th scope="col">{x}</th>' for x in headings) + '</tr></thead><tbody>' + ''.join('<tr>' + ''.join(f'<td>{v}</td>' for v in row) + '</tr>' for row in rows) + '</tbody></table>'

    buses = table(["Bus", "Voltage (pu)", "Angle (degrees)", "Net active (pu)", "Net reactive (pu)", "Source"], [
        [n(f"public.bus.{i}.bus_id", 0), n(f"public.bus.{i}.v_pu"), n(f"public.bus.{i}.angle_deg", 4),
         n(f"public.bus.{i}.p_net_pu"), n(f"public.bus.{i}.q_net_pu"), '<a href="#ref-code">Module calculation</a>'] for i in range(4)])
    branches = table(["From → to", "Sending active (pu)", "Sending reactive (pu)", "Active loss (pu)", "Reactive loss (pu)", "Source"], [
        [n(f"public.branch.{i}.from_bus", 0) + ' → ' + n(f"public.branch.{i}.to_bus", 0),
         *[n(f"public.branch.{i}.{key}") for key in ["p_flow_pu", "q_flow_pu", "p_loss_pu", "q_loss_pu"]], '<a href="#ref-code">Module calculation</a>'] for i in range(4)])
    ratings = table(["From → to", "Sending (MVA)", "Receiving (MVA)", "Rating (MVA)", "Source"], [
        [n(f"public.branch.{i}.from_bus", 0) + ' → ' + n(f"public.branch.{i}.to_bus", 0),
         n(f"public.branch.{i}.sending_mva", 1), n(f"public.branch.{i}.receiving_mva", 1),
         n(f"input.branches.{i}.rating_mva", 0), '<a href="#ref-case">Public case</a>'] for i in range(4)])
    generator_checks = table(["Bus", "Reactive output (Mvar)", "Lower limit (Mvar)", "Upper limit (Mvar)", "Excess above upper limit (%)", "Source"], [
        [n(f"input.generators.{i}.bus", 0), n(f"public.qg{bus}_mvar", 1),
         n(f"input.generators.{i}.q_min_mvar", 0), n(f"input.generators.{i}.q_max_mvar", 0),
         n("public.q_excess_pct" if bus == 4 else "public.q_excess1_pct", 1), '<a href="#ref-case">Public case limits</a>; module outputs']
        for i, bus in [(0, 4), (1, 1)]])
    bus_inputs = table(["Bus", "Type", "Demand (MW)", "Demand (Mvar)", "Start/set voltage (pu)", "Source"], [
        [n(f"input.buses.{i}.id", 0), ["Reference", "Fixed demand", "Fixed demand", "Fixed active power and voltage"][i],
         n(f"input.buses.{i}.p_mw", 0), n(f"input.buses.{i}.q_mvar", 2),
         n({0: "input.generators.1.v_set", 3: "input.generators.0.v_set"}.get(i, f"input.buses.{i}.v_initial"), 2), '<a href="#ref-case">Public case</a>'] for i in range(4)])
    line_inputs = table(["From → to", "Resistance (pu)", "Reactance (pu)", "Charging (pu)", "Source"], [
        [n(f"input.branches.{i}.from", 0) + ' → ' + n(f"input.branches.{i}.to", 0),
         n(f"input.branches.{i}.r_pu", 5), n(f"input.branches.{i}.x_pu", 4),
         n(f"input.branches.{i}.b_pu", 4), '<a href="#ref-case">Public case</a>'] for i in range(4)])
    changes = json.loads((HERE / "changes.json").read_text(encoding="utf-8"))
    # Reuse the existing house layout verbatim, with its canonical colour tokens.
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    layout = re.search(r'<style>(.*?)</style>', exemplar, re.S).group(1)
    points = []
    for i in range(4):
        v = r[f"public.bus.{i}.v_pu"]
        x, y = 105 + i * 175, 245 - (v - 0.85) * 800
        tip = f'Public four-bus case; bus {i+1}; voltage magnitude {format_number(v, 6)} pu; limits {case["buses"][i]["v_min"]:.1f}–{case["buses"][i]["v_max"]:.1f} pu'
        points.append(f'<circle tabindex="0" data-case="{escape(tip)}" cx="{x}" cy="{y:.3f}" r="7" fill="var(--accent)"><title>{escape(tip)}</title></circle>')
    chart = '<svg viewBox="0 0 720 300" role="img" aria-label="Voltage magnitude by bus with public-case limits">'
    chart += '<path d="M65 30V245H680" fill="none" stroke="currentColor"/>'
    for value in [case["buses"][0]["v_min"], case["buses"][0]["v_max"]]:
        y = 245 - (value - 0.85) * 800
        chart += f'<g class="limit"><path d="M65 {y:.3f}H680" stroke="var(--caution)" stroke-dasharray="6 4"/><text x="70" y="{y-6:.3f}">{value:.1f} pu limit</text></g>'
    chart += ''.join(points)
    chart += ''.join(f'<text x="{105+i*175}" y="269">{i+1}</text>' for i in range(4))
    chart += '<text x="330" y="292">Bus identifier</text><text x="5" y="22">Voltage (pu)</text></svg>'
    # Round JSON values and normalize signed zero across numerical libraries.
    chart_record = {key: (round(value, 6) + 0.0) if isinstance(value, float) and key != "tol" else value for key, value in r.items()}
    voltage_position = "inside" if r["public.voltage_bounds_satisfied"] else "outside"
    voltage_status = "satisfied" if r["public.voltage_bounds_satisfied"] else "violated"
    rating_status = ("are below the supplied ratings" if r["public.branch_ratings_satisfied"]
                     else "exceed at least one supplied rating")
    body = f'''
<header><div class="kicker">Power systems analysis</div><h1>Power module analysis</h1>
<p class="sub">Steady-state alternating-current load flow</p><p>Draft for maintainer review</p></header>
<section id="s1"><h2>1. Executive summary</h2>
<p>The power module is a set of electrical-network calculation tools. Analysis is performed to determine bus voltages and power transfers for specified loads and generation; key results are summarized below.</p>
<ul><li>A converged electrical state is calculated for the public four-bus example.
<ul><li>Voltage magnitudes lie {voltage_position} the supplied case limits; the plotted limits are input data rather than a general operating standard.</li></ul></li>
<li>Both specified generator operating points exceed their supplied reactive-power limits.
<ul><li>Bus four reactive generation is {n('public.qg4_mvar', 1)} Mvar against an upper limit of {n('input.generators.0.q_max_mvar', 0)} Mvar. Reference-bus reactive generation is {n('public.qg1_mvar', 1)} Mvar against an upper limit of {n('input.generators.1.q_max_mvar', 0)} Mvar. Exclusion of the fixed-voltage operating point is conditional on the supplied case limits, pending a generator-limit study.</li></ul></li></ul>
<p>The application limits and required next analysis are stated in Sections 7 and 8. The case basis is cited in <a href="#ref-case">Reference 2</a>.</p></section>
<div id="revision-history"><h3>Revision history</h3>
<table><thead><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr></thead><tbody><tr><td>1</td><td>2026-10-21</td><td>The public circuit and hand-derived example are calculated to document the numerical workflow and its operating limits.</td></tr><tr><td>2</td><td>2026-10-22</td><td>Embedded zeros, operating-check bindings, iteration terminology and citations are corrected, and independent four-bus solution anchors are added.</td></tr></tbody></table></div>
<section id="s2"><h2>2. Introduction</h2>
<p>The alternating-current load-flow workflow is described for an engineer specifying a balanced steady-state network. The calculated state is separated from equipment acceptance.</p>
<p>Bus and branch objects are assembled into a network and passed to the documented solver. Bus voltages, angles, injections, branch flows, losses, convergence and iteration count are returned. No external engineering program is required for this workflow.</p>
<h3>Not covered</h3><p>Data-center power-path redundancy, microgrid energy management, battery dispatch, governor and voltage droop, island detection, relay coordination, communication configuration and commissioning sequences are present elsewhere in the power package. Their input models and tests are distinct; numerical conclusions in this report apply to load flow only. Fault levels, contingencies and dynamic stability are not implemented by the load-flow entry point.</p></section>
<section id="s3"><h2>3. Design data and criteria</h2>
<p>The public four-bus case is used as the numerical basis. Input values are taken from the open case tables rather than estimated from an installation.</p>
<p>The common power base is {n('base_mva', 0)} MVA. Active and reactive powers are divided by this base before entry; positive net injection means generation minus demand. The generator voltage setpoint takes precedence over the bus starting magnitude. Angles enter in radians and are returned in degrees. The supplied case input tables and their source are reproduced in Appendix 10.1.</p>
<p>At the reference bus, voltage magnitude and angle are fixed and net powers are solved. At the generator bus, active injection and voltage magnitude are fixed and reactive injection is solved. At demand buses, both net powers are fixed and voltage magnitude and angle are solved (<a href="#ref-manual">Reference 1, Section 4.1</a>).</p>
<p>The mismatch tolerance is {n('tol', 12)} pu and the iteration cap is {n('max_iter', 0)}. These explicit calculation settings are numerical controls; the tolerance is selected to support the hand-reference assertions and is not a plant acceptance criterion. Voltage bounds and generator reactive bounds come from the case tables. Branch ratings are compared at both ends outside the solver. The reactive bounds of ±100 Mvar, voltage interval of 0.9–1.1 pu and branch ratings of 250 MVA are entries in the open case; a textbook basis for these limits is not established. Exclusion is conditional on the supplied case limits. A converged state is not an acceptance decision.</p>
<figure><svg viewBox="0 0 760 320" role="img" aria-label="Electrical single-line model with reference supply, generator, four buses, line impedances and constant-power demands">
<g fill="none" stroke="var(--ink)" stroke-width="3"><path d="M140 80H560V220H140Z"/><path d="M140 58V102M560 58V102M140 198V242M560 198V242" stroke-width="7"/>
<circle cx="70" cy="80" r="23"/><path d="M93 80H140M560 220H620"/><circle cx="644" cy="220" r="23"/>
<path d="M140 102V136M560 102V136M140 242V276M560 242V276"/></g>
<g fill="var(--ink)" font-size="15"><text x="30" y="35">Reference supply</text><text x="615" y="180">Generator</text><text x="124" y="52">Bus 1</text><text x="542" y="52">Bus 2</text><text x="124" y="192">Bus 3</text><text x="542" y="192">Bus 4</text>
<text x="294" y="67">Line 1 → 2</text><text x="294" y="210">Line 3 → 4</text><text x="150" y="162">Line 1 → 3</text><text x="456" y="162">Line 2 → 4</text><text x="105" y="152">↓</text><text x="570" y="152">↓</text><text x="110" y="300">Fixed demand</text><text x="530" y="300">Fixed demand</text></g></svg>
<figcaption>Figure 1. Electrical connectivity and boundary conditions. Every bus carries a constant-power demand; the reference supply and fixed-voltage generator also supply their local demand. Branches use the pi equivalent in Section 4. Physical line lengths and conductor geometry are not specified by this case.</figcaption></figure></section>
<section id="s4"><h2>4. Methodology and theory</h2>
<p>The nodal admittance matrix is assembled from pi-equivalent branches. Power-balance equations are solved by Newton iteration in voltage magnitude and angle.</p>
<p>The reciprocal-impedance and nodal-assembly basis is given in <a href="#ref-text">Reference 3, Chapter 5, Section 3</a>. For a real sending-end tap, the branch equations are taken from <a href="#ref-manual">Reference 1, Section 3.2, equations (3.1)–(3.2)</a> with zero phase shift:</p>
<div class="eq">y = 1/(r + jx), h = jb/2; Yff = (y + h)/t²; Yft = Ytf = −y/t; Ytt = y + h.</div>
<p>Here r is series resistance, x is series reactance, b is total charging susceptance, t is real tap ratio and j is the imaginary unit. Subscripts f and t denote the sending and receiving terminals. Half the charging is placed at each pi-section end; the ideal tap precedes the entire pi section. The public case encodes a nominal tap as zero; unity is passed to the module (<a href="#ref-manual">Reference 1, Appendix B, branch data</a>).</p>
<p>The complex-voltage vector V, injected-current vector I and complex injected-power vector S satisfy <a href="#ref-text">Reference 3, Chapter 5, equations (1), (6) and (8)</a>, also expressed in <a href="#ref-manual">Reference 1, Section 3.6, equations (3.8)–(3.17)</a>:</p>
<div class="eq">I = YV; S = diag(V) conjugate(YV); S = P + jQ.</div>
<p>Y is the assembled admittance matrix; P and Q denote active and reactive injection. For terminal flows, Sf = Vf conjugate(If) and St = Vt conjugate(It); the branch complex loss is Sf + St. These are terminal power definitions in the same source. Charging can make net branch reactive loss negative without implying active generation.</p>
<p>The unknown vector u contains non-reference angles and demand-bus magnitudes. With scheduled-minus-calculated mismatch Δs, the update is J Δu = Δs, followed by u ← u + Δu (<a href="#ref-newton">Reference 4, Section 4.5.2, equations (4.5.8)–(4.5.10)</a>). J contains the derivatives of calculated active and reactive injections. Only active equations at non-reference buses and reactive equations at demand buses enter the solve (<a href="#ref-manual">Reference 1, Section 4.1, equations (4.2)–(4.5) and Newton-method paragraph</a>).</p>
<p>A result is marked converged when the maximum absolute scheduled-power mismatch is strictly less than the supplied tolerance. Voltage and equipment bounds are not part of that stopping test. The system base is stored by the module; dimensional conversion is performed explicitly in this report.</p></section>
<section id="s5"><h2>5. Detailed results and worked example</h2>
<p>The documented public circuit is calculated by calling the module with the input mapping in Section 9.2. Numerical outputs are reported below; generator and branch checks are calculated separately from those outputs.</p>
<p>The public example converges after {n('public.newton_updates', 0)} Newton updates and {n('public.iterations', 0)} mismatch evaluations. The module iteration count includes the final converged mismatch evaluation, which performs no update. Total active branch loss is {n('public.total_loss_mw', 1)} MW. Bus injection results include local demand; generator reactive output is recovered by adding that demand to net injection.</p>
<figure>{chart}<figcaption>Figure 2. Voltage versus bus identifier. Points are selected by pointer or keyboard focus; dashed lines show the case voltage bounds. The plotted bounds are {voltage_status}. Engineering acceptance remains blocked by the reactive-generation result.</figcaption></figure>
<button type="button" id="limits">Toggle voltage limits</button><p id="chart-detail" role="status">Select a voltage point for case, component, voltage magnitude, limit and units.</p>
{buses}<p>Table 1. Module bus results, with positive powers injected into the network.</p>
{branches}<p>Table 2. Module branch outputs in sending-end orientation; losses sum both terminal powers.</p>
<p>At bus four, Qg = Qnet × Sbase + Qd = {n('public.bus.3.q_net_pu')} × {n('base_mva', 0)} + {n('input.buses.3.q_mvar', 2)} = {n('public.qg4_mvar', 1)} Mvar. The supplied upper limit is {n('input.generators.0.q_max_mvar', 0)} Mvar; excess is {n('public.q_excess_mvar', 1)} Mvar, or {n('public.q_excess_pct', 1)} % relative to that limit. Exclusion of the fixed-voltage operating point is conditional on the supplied case limits, pending a generator-limit study.</p>
{ratings}<p>Table 3. Apparent terminal power magnitude compared with the public-case branch rating. These circuit values {rating_status}. Rating duration and equipment temperature basis are not supplied.</p>
{generator_checks}<p>Table 4. Both generator reactive outputs exceed their supplied upper limits. The reference-bus excess is {n('public.q_excess1_mvar', 1)} Mvar. The percentage difference is calculated against each generator’s upper limit.</p>
<p>At the reference bus, Qg = Qnet × Sbase + Qd = {n('public.bus.0.q_net_pu')} × {n('base_mva', 0)} + {n('input.buses.0.q_mvar', 2)} = {n('public.qg1_mvar', 1)} Mvar. Both regulated sources are above their reactive bounds; an acceptable revised operating state is not calculated.</p></section>
<section id="s6"><h2>6. Validation evidence</h2>
<p>Numerical evidence is provided by the existing admittance and solved-circuit tests. The public example is also compared with independently calculated pi-model Newton and Gauss-Seidel reference values in the report test; agreement with a separate execution of the source program is not assessed.</p>
<p>The existing tests exercise bus and branch construction, input rejection, admittance assembly, convergence flags, output fields, power balance and nonconvergence. The hand-reference tests additionally compare a solved state against prescribed rectangular phasors, reject a deliberately incomplete iteration, and check terminal powers for tapped branches with charging. Test locations are listed in Section 9.2.</p>
<p>For the documented hand case, V1 = 1 + j0, V2 = 0.96 + j0.28 and V3 = 0.90 − j0.12 pu. Series reactances are 0.5 and 0.25 pu, with zero resistance and charging and unity taps. The scheduled injections are obtained from S = V conjugate(I), giving generator-bus active injection 2.0288 pu and demand-bus consumption 1.4688 + j0.024 pu. These are constructed circuit data, not an installation assumption (<a href="#ref-hand">Reference 5</a>).</p>
<p>The module returns reference-bus reactive injection {n('hand.bus.0.q_net_pu', 4)} pu, generator-bus reactive injection {n('hand.bus.1.q_net_pu', 4)} pu and final demand-bus voltage {n('hand.bus.2.v_pu')} pu. Hand arithmetic gives the same values within the absolute numerical criterion of 10⁻⁸ pu. The arithmetic and literal assertions are provided in the report test; the theory is cited in Section 4.</p>
<p>The report consistency checks compare every displayed numerical binding with fresh module outputs or the cited inputs. Those consistency checks do not replace the hand-worked literals or the independent four-bus voltage, angle, generator-power, loss and terminal-loading anchors. Field measurements, certified equipment results, a separate program solution and a published solved-output comparison are not evaluated.</p></section>
<section id="s7"><h2>7. Key findings and limits of use</h2>
<p>For the documented case, the supplied operating point is limited by reactive generation at both sources. Voltage and branch acceptance are determined by the separate checks above. Numerical convergence is established only for the stated inputs.</p>
<p>Both source voltage setpoints are held even after their supplied reactive bounds are exceeded. Reactive-limit enforcement, conversion of generator buses to demand buses, tap control, voltage-dependent loads, phase-shifting transformers, explicit bus shunts and unbalanced circuits are outside the implemented load-flow model. The case has no unsupported bus shunts or phase shifts.</p>
<p>Exactly one reference bus and unique bus identifiers are required. Constructor checks reject zero branch impedance, nonpositive tap and inverted voltage bounds. The caller also needs to check finite numbers, existing branch endpoints, positive power base, a connected network, sensible starting voltages and valid iteration settings; comprehensive checks are absent. A singular linear solve can raise an exception. An iteration-limit result still contains numerical values and requires rejection when convergence is false.</p>
<p>The dense admittance and Jacobian matrices are used. Large-network performance and ill-conditioned operating points are not evaluated. Voltage bounds stored on buses do not reject a solved voltage violation. The reported branch flows omit explicit receiving-end records; receiving powers in Table 3 are reconstructed from sending powers and reported losses.</p></section>
<section id="s8"><h2>8. Recommendations</h2>
<p>A constrained operating study is required before the public example is used as an accepted generator operating point. The recommendations follow the exceeded case limit and the method boundary in <a href="#ref-manual">Reference 1, Section 4.1</a>.</p>
<p>Apply both supplied reactive bounds in a limit-aware analysis with an explicit reference-control arrangement. Release an affected voltage setpoint and solve the resulting demand-bus equations, as described in the cited source. Both regulated sources exceed their bounds in this circuit; conversion of both to fixed-demand buses leaves no voltage reference. A feasible reference-control arrangement and reactive-support or redispatch study are required before that conversion is used. Report the revised source powers, voltage deviations and both terminal branch loading checks before accepting operation. No reactive-support rating or acceptable revised operating point is inferred from this unconstrained state.</p>
<p>For an engineering network, supply the actual equipment ratings, voltage criteria and applicable operating duration. Reject a nonconverged result and any exceeded supplied limit before use. Add finite-input and connectivity validation, followed by a published solved-state comparison and field-data comparison when measured data are available.</p></section>
<section id="s9"><h2>9. References and calculation records</h2>
<p>The open textbooks, manual and circuit tables are used as the external basis. Repository records are identified separately so numerical regression evidence is not confused with an external acceptance standard.</p>
<h3>9.1 Public sources</h3><ol class="refs">
<li id="ref-manual"><a href="https://matpower.org/docs/MATPOWER-manual-8.1.pdf">MATPOWER User’s Manual, version 8.1, July 2025</a>: Section 3.2, Branches; Section 3.6, Network Equations; Section 4.1, AC Power Flow; Appendix B, case format. Branch equations, complex power definitions, Newton method and generator-limit sequence are used. No value from a licensed standard is adopted.</li>
<li id="ref-case"><a href="{escape(case['source']['url'])}">MATPOWER 8.1 public four-bus case tables</a>: system power base, bus, generator and branch tables. The source identifies Power System Analysis, McGraw-Hill, 1994, pages 337–338; that textbook is not consulted. The open case is the input authority used here. Only supported fields are mapped; no source-program execution is used.</li>
<li id="ref-text"><a href="https://ocw.mit.edu/courses/6-061-introduction-to-electric-power-systems-spring-2011/resources/mit6_061s11_ch5/">Introduction to Electric Power Systems, open textbook, Chapter 5: Introduction to Load Flow</a>, 2003 chapter in the Spring 2011 collection; Section 2, Power Flow, equation (1), and Section 3, Bus Admittance, equations (2)–(8). These provide the electrical power definitions and network-assembly basis.</li>
<li id="ref-newton"><a href="https://fncbook.com/newtonsys/">Fundamentals of Numerical Computation, online Julia edition</a>, accessed 21 October 2026; Section 4.5.2, The multidimensional Newton iteration, equations (4.5.8)–(4.5.10). This provides the Jacobian linearization and update, with mismatch sign expressed in scheduled-minus-calculated form here.</li></ol>
<h3>9.2 Repository evidence and rebuild</h3><ul>
<li id="ref-code">Calculation entry: <code>src/digitalmodel/power/analysis/load_flow.py</code>, documented classes <code>Bus</code>, <code>Branch</code>, <code>Network</code> and function <code>solve_power_flow</code>.</li>
<li id="ref-hand">Reference 5: <code>src/digitalmodel/power/analysis/docs/reference-cases.md</code>, “Solved three-bus reference” and “Tap and charging convention”. Exact rectangular phasors and circuit inputs are recorded there.</li>
<li>Existing numerical tests: <code>tests/power/analysis/test_load_flow.py</code> and <code>tests/power/analysis/test_reference_cases.py</code>. Report tests: <code>tests/power/analysis/test_module_report.py</code>.</li>
<li>Input transcription: <code>docs/reports/modules/power/public_case.json</code>. All source tables used for the solve are presented in Appendix 10.1. Generator limits and branch ratings are postprocessing inputs.</li>
</ul><pre>from digitalmodel.power.analysis.load_flow import solve_power_flow
# public_network maps the cited input tables into documented module objects.
network = public_network(read_case())
result = solve_power_flow(network, tol=TOL, max_iter=MAX_ITER)
assert result.converged</pre>
<pre>PYTHONPATH=src uv run --frozen python docs/reports/modules/power/build_report.py
PYTHONPATH=src uv run --no-project --with pytest --with numpy python -m pytest tests/power/analysis -q -p no:cacheprovider</pre>
<p>The generator and tests use explicit UTF-8 text encoding and line-feed output. The report test compares embedded JSON numerically and checks the remaining text exactly; a stale report names the rebuild command.</p></section>
<section id="s10"><h2>10. Appendices</h2>
<p>The input and quality records are provided for detailed review. Data absent from the public case is identified without assigning an operating value.</p>
<h3>10.1 Input and data register</h3>{bus_inputs}{line_inputs}
<p>Generator active power at bus four is {n('input.generators.0.p_mw', 0)} MW and its fixed voltage is {n('input.generators.0.v_set', 2)} pu. Its reactive interval is {n('input.generators.0.q_min_mvar', 0)} to {n('input.generators.0.q_max_mvar', 0)} Mvar. Bus-one generator active and reactive starting values are zero; reference voltage is {n('input.generators.1.v_set', 2)} pu. Initial angles are zero. Branch charging is total charging, with unity taps and no phase shift. Source: <a href="#ref-case">public case tables</a>.</p>
{table(['Item', 'Basis or status', 'Use'], [
['Electrical inputs and equipment bounds', 'Public case tables, Reference 2', 'Applied only to this circuit'],
['Hand-reference phasors and inputs', 'Constructed exact circuit, Reference 5', 'Numerical anchor only'],
['Mismatch tolerance and iteration cap', 'Explicit numerical settings, Section 3', 'Calculation control only'],
['Physical conductor geometry and thermal duration', 'NOT_EVALUATED: not supplied', 'Equipment approval excluded'],
['Applicable installation voltage criterion', 'NOT_EVALUATED: required input', 'No general voltage limit inferred'],
['Field data and protection settings', 'NOT_EVALUATED: not supplied', 'Field acceptance and protection excluded'],
['Assumptions', 'No estimated installation data is used', 'Public and constructed circuits only']])}
<h3>10.2 Symbols and units</h3><p>Per unit (pu) denotes a quantity divided by its consistent system base. MW is megawatt, Mvar is megavolt-ampere reactive and MVA is megavolt-ampere. Alternating current is the sinusoidal steady-state basis. A reference bus fixes voltage and angle; a fixed-active-power-and-voltage bus is conventionally called PV; a fixed-active-and-reactive-power bus is called PQ. V is complex voltage; I is complex current; Y is complex admittance; S is complex power; P is active power; Q is reactive power; Qg is generator reactive power; Qd is local reactive demand; Sbase is the power base. J is the Jacobian, u is the unknown vector and Δ is an increment. Remaining branch symbols are defined immediately after their equation.</p>
<h3>10.3 Quality pre-flight</h3><p>The report checks cover numerical bindings, hand literals, displayed-input reconstruction of generator reactive output, offline resources, source links, section count, public wording, UTF-8 and line-feed output, and fresh-build equality at displayed precision. The existing numerical tests provide broader circuit evidence. Test outcomes are recorded with the submitted change; a checklist is not a substitute for those executions.</p>
<h3>10.4 Engineering changes</h3><p>Revision 1 establishes the initial engineering basis; revision 2 corrects reproducibility, operating checks and source precision. Select the preceding revision to display subsequent changes.</p>
<label>Last read revision <select id="prior-revision"><option value="0">Before initial issue</option><option value="1">1</option><option value="2">2</option></select></label><ul id="change-list"></ul>
<h3>10.5 Report comments</h3><p>Comments are saved beside this HTML by choosing its folder in the save dialog. Where a folder picker is unavailable, select the same folder in Save As; the download fallback uses the browser destination. Load the sidecar before adding to an existing review. No comments are sent to a server.</p>
<label>Section <select id="comment-section">{''.join(f'<option value="s{i}">Section {i}</option>' for i in range(1,11))}</select></label>
<label>Comment <textarea id="comment-text" rows="3"></textarea></label>
<button type="button" id="add-comment">Add comment</button><button type="button" id="save-comments">Save comments beside report</button>
<label>Load comments <input id="load-comments" type="file" accept="application/json"></label><p id="comment-status" role="status">No unsaved comments.</p><ol id="comment-list"></ol>
</section>
'''
    script = r'''
(() => {
  'use strict';
  const byId = id => document.getElementById(id);
  for (const point of document.querySelectorAll('[data-case]')) {
    const show = () => { byId('chart-detail').textContent = point.dataset.case; };
    point.addEventListener('pointerenter', show);
    point.addEventListener('focus', show);
  }
  let limitsVisible = true;
  byId('limits').addEventListener('click', () => {
    limitsVisible = !limitsVisible;
    document.querySelectorAll('.limit').forEach(x => { x.style.display = limitsVisible ? '' : 'none'; });
  });
  const changes = JSON.parse(byId('changes-data').textContent);
  const showChanges = () => {
    byId('change-list').replaceChildren();
    const items = Number(byId('prior-revision').value) < Number(changes.revision) ? changes.changes : ['No later engineering changes.'];
    items.forEach(text => { const li = document.createElement('li'); li.textContent = text; byId('change-list').append(li); });
  };
  byId('prior-revision').addEventListener('change', showChanges);
  showChanges();
  let state = {report: 'power', revision: changes.revision, comments: []};
  const showComments = () => {
    byId('comment-list').replaceChildren();
    state.comments.forEach(c => { const li = document.createElement('li'); li.textContent = `${c.section}: ${c.text}`; byId('comment-list').append(li); });
  };
  byId('add-comment').addEventListener('click', () => {
    const text = byId('comment-text').value.trim();
    if (!text) return;
    state.comments.push({section: byId('comment-section').value, text});
    byId('comment-text').value = '';
    byId('comment-status').textContent = 'Unsaved comments. Save beside the report.';
    showComments();
  });
  byId('load-comments').addEventListener('change', async event => {
    try {
      const file = event.target.files[0];
      if (!file) return;
      const incoming = JSON.parse(await file.text());
      if (incoming.report !== 'power' || incoming.revision !== changes.revision || !Array.isArray(incoming.comments) ||
          incoming.comments.some(c => !/^s(?:[1-9]|10)$/.test(c.section) || typeof c.text !== 'string')) throw new Error('Invalid sidecar or revision.');
      if (state.comments.length) throw new Error('Save current comments before loading another sidecar.');
      state = incoming;
      showComments();
      byId('comment-status').textContent = 'Sidecar loaded. New comments require saving.';
    } catch (error) { byId('comment-status').textContent = error.message; }
  });
  byId('save-comments').addEventListener('click', async () => {
    const text = JSON.stringify(state, null, 2) + '\n';
    try {
      if (window.showDirectoryPicker) {
        const directory = await window.showDirectoryPicker({mode: 'readwrite'});
        const handle = await directory.getFileHandle('power.comments.json', {create: true});
        const writable = await handle.createWritable();
        await writable.write(text); await writable.close();
        byId('comment-status').textContent = 'Comments saved in the selected report folder.';
      } else if (window.showSaveFilePicker) {
        const handle = await window.showSaveFilePicker({suggestedName: 'power.comments.json'});
        const writable = await handle.createWritable();
        await writable.write(text); await writable.close();
        byId('comment-status').textContent = 'Comments saved to the selected file.';
      } else {
        const url = URL.createObjectURL(new Blob([text], {type: 'application/json'}));
        const link = document.createElement('a'); link.href = url; link.download = 'power.comments.json'; link.click();
        setTimeout(() => URL.revokeObjectURL(url), 1000);
        byId('comment-status').textContent = 'Download requested; place the sidecar beside the report and reload it to check the saved copy.';
      }
    } catch (error) { byId('comment-status').textContent = 'Comments remain unsaved: ' + error.message; }
  });
})();
'''
    html = '<!DOCTYPE html>\n<html lang="en" data-theme="light"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Power module analysis</title>\n'
    html += f'<style>{brand}\n{layout}</style></head><body class="hs-doc"><div class="wrap">{body}</div>\n'
    html += '<script id="calculation-data" type="application/json">' + json.dumps(chart_record, ensure_ascii=False, sort_keys=True) + '</script>\n'
    html += '<script id="changes-data" type="application/json">' + json.dumps(changes, ensure_ascii=False) + '</script>\n'
    html += f'<script>{script}</script></body></html>\n'
    target = Path(destination) if destination else HERE / 'report.html'
    target.write_text(html, encoding="utf-8", newline="\n")
    return r


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    build(parser.parse_args().output)
