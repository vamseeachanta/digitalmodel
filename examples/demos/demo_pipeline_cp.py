#!/usr/bin/env python3
"""
================================================================================
  ACE Engineer — Pipeline Cathodic Protection Design
================================================================================

  Demo: Offshore Pipeline CP System Design per DNV-RP-B401 & ISO 15589
  Run:  PYTHONPATH=src python examples/demos/demo_pipeline_cp.py

  This script showcases pipeline cathodic protection design:
    1. Design CP system for a 20 km, 24-inch offshore pipeline
    2. Calculate current demand for 3 coating types (FBE, 3LPE, bare)
    3. Size sacrificial anodes per DNV-RP-B401
    4. Generate anode depletion profile over 25-year design life
    5. CP assessment report with compliance checks

================================================================================
"""

import math

from digitalmodel.cathodic_protection.pipeline_cp import (
    PipelineCPInput,
    PipelineEnvironment,
    pipeline_current_demand,
    design_pipeline_cp,
)
from digitalmodel.cathodic_protection.coating import (
    CoatingCategory,
    coating_breakdown_factors,
    coating_life_estimate,
    COATING_CONSTANTS,
)
from digitalmodel.cathodic_protection.dnv_rp_b401 import (
    anode_mass_requirement,
    number_of_anodes,
    anode_current_output,
    anode_resistance_slender_standoff,
    equivalent_radius_from_mass,
    ANODE_CAPACITY_ALZNI,
    DESIGN_DRIVING_VOLTAGE,
)
from digitalmodel.cathodic_protection.anode_depletion import (
    generate_depletion_profile,
    calculate_remaining_life,
    AnodeStatus,
    recommend_inspection_interval,
    UTILIZATION_FACTOR_BRACELET,
)
from digitalmodel.cathodic_protection.cp_reporting import (
    generate_assessment_report,
    remaining_life_summary,
    compliance_check_potential,
    ComplianceCheck,
    ComplianceStatus,
)


def separator(title=""):
    """Print a visual separator."""
    if title:
        print(f"\n{'=' * 80}")
        print(f"  {title}")
        print(f"{'=' * 80}")
    else:
        print(f"{'─' * 80}")


def main():
    print()
    print("  ╔══════════════════════════════════════════════════════════════════════╗")
    print("  ║         ACE ENGINEER  —  Pipeline Cathodic Protection Design        ║")
    print("  ║         DNV-RP-B401 | ISO 15589 | NACE SP0169                       ║")
    print("  ╚══════════════════════════════════════════════════════════════════════╝")
    print()

    # ──────────────────────────────────────────────────────────────────────
    # Pipeline Parameters
    # ──────────────────────────────────────────────────────────────────────
    pipeline_od_inch = 24.0
    pipeline_od_m = pipeline_od_inch * 0.0254
    pipeline_wt_inch = 0.625
    pipeline_wt_m = pipeline_wt_inch * 0.0254
    pipeline_length_km = 20.0
    pipeline_length_m = pipeline_length_km * 1000.0
    design_life_years = 25.0
    water_depth_m = 120.0
    seawater_resistivity = 0.30  # ohm-m (typical offshore)
    seawater_temp_c = 15.0

    # Anode parameters (Al-Zn-In bracelet anodes, typical offshore pipeline)
    anode_mass_kg = 250.0  # net mass per anode
    anode_length_m = 1.0   # anode length
    anode_radius_m = equivalent_radius_from_mass(anode_mass_kg, anode_length_m)

    separator("1. PIPELINE DEFINITION")

    print(f"\n  Pipeline Specifications:")
    print(f"  {'─' * 55}")
    print(f"  Outer Diameter:           {pipeline_od_inch}\" ({pipeline_od_m*1000:.1f} mm)")
    print(f"  Wall Thickness:           {pipeline_wt_inch}\" ({pipeline_wt_m*1000:.1f} mm)")
    print(f"  Length:                   {pipeline_length_km:.0f} km ({pipeline_length_m:,.0f} m)")
    print(f"  Design Life:              {design_life_years:.0f} years")
    print(f"  Water Depth:              {water_depth_m:.0f} m")
    print(f"  Environment:              Subsea (seawater)")
    print(f"  Seawater Resistivity:     {seawater_resistivity:.2f} ohm-m")
    print(f"  Seawater Temperature:     {seawater_temp_c:.0f}°C")

    # Surface area
    surface_area = math.pi * pipeline_od_m * pipeline_length_m
    print(f"\n  External Surface Area:    {surface_area:,.0f} m²")

    print(f"\n  Anode Specifications (Al-Zn-In bracelet):")
    print(f"  {'─' * 55}")
    print(f"  Net Mass per Anode:       {anode_mass_kg:.0f} kg")
    print(f"  Anode Length:             {anode_length_m:.1f} m")
    print(f"  Equivalent Radius:        {anode_radius_m*1000:.1f} mm")
    print(f"  Electrochemical Capacity: {ANODE_CAPACITY_ALZNI:.0f} A-h/kg")
    print(f"  Utilization Factor:       {UTILIZATION_FACTOR_BRACELET:.2f}")
    print(f"  Driving Voltage:          {DESIGN_DRIVING_VOLTAGE:.3f} V")

    # ──────────────────────────────────────────────────────────────────────
    # 2. Current Demand for 3 Coating Types
    # ──────────────────────────────────────────────────────────────────────
    separator("2. CURRENT DEMAND COMPARISON — 3 Coating Systems")

    coating_configs = [
        ("FBE (Fusion Bonded Epoxy)", CoatingCategory.FBE),
        ("3LPE (Three-Layer PE)",     CoatingCategory.THREE_LAYER_PE),
        ("Bare Steel (No Coating)",   CoatingCategory.NONE),
    ]

    print(f"\n  Current demand over {design_life_years:.0f}-year design life:")
    print(f"  Seawater bare-steel current density: 100 mA/m² (per NACE SP0169)")
    print()
    print(f"  {'Coating System':<30} {'fc(init)':<10} {'fc(mean)':<10} {'fc(final)':<10} {'I_mean (A)':<12} {'I_final (A)'}")
    print(f"  {'─'*30} {'─'*10} {'─'*10} {'─'*10} {'─'*12} {'─'*12}")

    coating_results = {}
    for name, coat_type in coating_configs:
        if coat_type == CoatingCategory.NONE:
            fc_init = 1.0
            fc_mean = 1.0
            fc_final = 1.0
        else:
            bd = coating_breakdown_factors(
                coat_type, design_life_years,
                depth_m=water_depth_m, temperature_c=seawater_temp_c
            )
            fc_init = bd.initial_factor
            fc_mean = bd.mean_factor
            fc_final = bd.final_factor

        # Current demand: I = A * i_c * fc / 1000 (mA -> A)
        i_c_mA_m2 = 100.0  # seawater bare steel
        I_mean = surface_area * i_c_mA_m2 * fc_mean / 1000.0
        I_final = surface_area * i_c_mA_m2 * fc_final / 1000.0

        coating_results[name] = {
            "fc_init": fc_init, "fc_mean": fc_mean, "fc_final": fc_final,
            "I_mean": I_mean, "I_final": I_final, "coat_type": coat_type,
        }

        print(
            f"  {name:<30} {fc_init:<10.3f} {fc_mean:<10.3f} {fc_final:<10.3f} "
            f"{I_mean:<12.1f} {I_final:<12.1f}"
        )

    # Coating life estimates
    print(f"\n  Coating Life Estimates (to 50% breakdown):")
    print(f"  {'─' * 55}")
    for name, coat_type in coating_configs[:2]:  # skip bare
        life = coating_life_estimate(coat_type, threshold_factor=0.50)
        print(f"  {name:<30} {life.estimated_life_years:.0f} years")

    # ──────────────────────────────────────────────────────────────────────
    # 3. Anode Sizing per DNV-RP-B401
    # ──────────────────────────────────────────────────────────────────────
    separator("3. ANODE SIZING — DNV-RP-B401 (FBE Coating)")

    # Use FBE coating for primary design
    fbe = coating_results["FBE (Fusion Bonded Epoxy)"]
    I_mean = fbe["I_mean"]
    I_final = fbe["I_final"]

    # Total anode mass requirement
    total_mass = anode_mass_requirement(
        I_mean_A=I_mean,
        T_design_years=design_life_years,
        E_capacity=ANODE_CAPACITY_ALZNI,
        u_f=UTILIZATION_FACTOR_BRACELET,
    )

    # Number of anodes
    n_anodes = number_of_anodes(
        total_mass_kg=total_mass,
        anode_net_mass_kg=anode_mass_kg,
        round_to_even=True,
    )

    # Anode spacing
    spacing_m = pipeline_length_m / n_anodes if n_anodes > 0 else pipeline_length_m

    # Anode current output check
    R_a = anode_resistance_slender_standoff(
        rho=seawater_resistivity,
        L_a=anode_length_m,
        r_a=anode_radius_m,
    )
    I_anode = anode_current_output(
        rho=seawater_resistivity,
        L_a=anode_length_m,
        r_a=anode_radius_m,
    )

    # Check: total output vs final demand
    total_output = I_anode * n_anodes
    output_demand_ratio = total_output / I_final if I_final > 0 else float('inf')

    print(f"\n  Design Current Demand:")
    print(f"  {'─' * 55}")
    print(f"  Mean current demand (I_cm):     {I_mean:.2f} A")
    print(f"  Final current demand (I_cf):    {I_final:.2f} A")

    print(f"\n  Mass Requirement (DNV-RP-B401 §7.7.1):")
    print(f"  {'─' * 55}")
    print(f"  Formula: M = (I_cm × t × 8760) / (ε × u)")
    print(f"  Total anode mass required:      {total_mass:,.0f} kg")
    print(f"  Individual anode mass:          {anode_mass_kg:.0f} kg")
    print(f"  Number of anodes required:      {n_anodes}")
    print(f"  Anode spacing:                  {spacing_m:,.0f} m ({spacing_m/1000:.1f} km)")

    print(f"\n  Current Output Verification:")
    print(f"  {'─' * 55}")
    print(f"  Anode resistance (R_a):         {R_a:.4f} ohm")
    print(f"  Current per anode (I_a):        {I_anode:.3f} A")
    print(f"  Total current capacity:         {total_output:.1f} A")
    print(f"  Final demand:                   {I_final:.2f} A")
    print(f"  Capacity/Demand ratio:          {output_demand_ratio:.2f}  {'✓ OK' if output_demand_ratio >= 1.0 else '✗ INSUFFICIENT'}")

    # Total anode weight
    total_weight_tonnes = (n_anodes * anode_mass_kg) / 1000.0
    print(f"\n  Total Anode Weight:             {total_weight_tonnes:.1f} tonnes ({n_anodes} × {anode_mass_kg} kg)")

    # ──────────────────────────────────────────────────────────────────────
    # 4. Anode Depletion Profile
    # ──────────────────────────────────────────────────────────────────────
    separator("4. ANODE DEPLETION PROFILE — 25-Year Design Life")

    # Total mass installed
    total_installed_mass = n_anodes * anode_mass_kg

    profile = generate_depletion_profile(
        original_mass_kg=total_installed_mass,
        mean_current_A=I_mean,
        design_life_years=design_life_years,
        anode_capacity_Ah_kg=ANODE_CAPACITY_ALZNI,
        utilization_factor=UTILIZATION_FACTOR_BRACELET,
        time_step_years=5.0,
    )

    print(f"\n  Total installed anode mass: {total_installed_mass:,.0f} kg")
    print(f"  Projected end-of-life:     Year {profile.end_of_life_year:.1f}")
    print()
    print(f"  {'Year':<8} {'Remaining (kg)':<18} {'Depletion (%)':<16} {'Status'}")
    print(f"  {'─'*8} {'─'*18} {'─'*16} {'─'*20}")

    for yr, mass, depl in zip(profile.years, profile.remaining_mass_kg, profile.depletion_percentage):
        if depl < 50:
            status = "█████████░ Healthy"
        elif depl < 75:
            status = "██████░░░░ Monitor"
        elif depl < 90:
            status = "████░░░░░░ Priority"
        elif depl < 100:
            status = "██░░░░░░░░ Critical"
        else:
            status = "░░░░░░░░░░ Depleted"

        print(f"  {yr:<8.0f} {mass:<18,.0f} {depl:<16.1f} {status}")

    # ASCII depletion chart
    print(f"\n  Depletion Curve:")
    print(f"  100% |", end="")
    for depl in profile.depletion_percentage:
        if depl >= 100:
            print("█", end="")
        elif depl >= 75:
            print("▓", end="")
        elif depl >= 50:
            print("▒", end="")
        elif depl >= 25:
            print("░", end="")
        else:
            print(" ", end="")
    print(f"| EOL: Yr {profile.end_of_life_year:.0f}")
    print(f"    0% |{'─' * len(profile.depletion_percentage)}|")
    year_labels = "".join(f"{int(y):<1}" if y % 10 == 0 else " " for y in profile.years)
    print(f"        {year_labels}")
    print(f"        Year 0{' ' * (len(profile.years) - 5)}Year {int(profile.years[-1])}")

    # Remaining life assessment at year 10
    status_yr10 = AnodeStatus(
        anode_id="Pipeline-FBE",
        original_mass_kg=total_installed_mass,
        current_mass_kg=total_installed_mass * 0.65,  # simulated inspection
        elapsed_years=10.0,
        mean_current_A=I_mean,
        anode_capacity_Ah_kg=ANODE_CAPACITY_ALZNI,
        utilization_factor=UTILIZATION_FACTOR_BRACELET,
    )
    depl_result = calculate_remaining_life(status_yr10)
    inspection = recommend_inspection_interval(depl_result, design_life_years, 10.0)

    print(f"\n  Inspection Assessment @ Year 10:")
    print(f"  {'─' * 55}")
    print(f"  Mass consumed:          {depl_result.mass_consumed_kg:,.0f} kg")
    print(f"  Remaining mass:         {depl_result.remaining_mass_kg:,.0f} kg")
    print(f"  Depletion:              {depl_result.depletion_percentage:.1f}%")
    print(f"  Remaining life:         {depl_result.remaining_life_years:.1f} years")
    print(f"  Status:                 {'DEPLETED' if depl_result.is_depleted else 'OPERATIONAL'}")
    print(f"\n  Inspection Recommendation:")
    print(f"  Next inspection:        {inspection.next_inspection_years:.1f} years")
    print(f"  Type:                   {inspection.inspection_type}")
    print(f"  Urgency:                {inspection.urgency}")

    # ──────────────────────────────────────────────────────────────────────
    # 5. CP Assessment Report
    # ──────────────────────────────────────────────────────────────────────
    separator("5. CP ASSESSMENT REPORT")

    # Compliance checks
    pot_check = compliance_check_potential(
        measured_potential_V=-0.920,
        criterion_V=-0.850,
        reference_electrode="Ag/AgCl",
    )

    # Current demand check
    current_check = ComplianceCheck(
        check_id="CD-001",
        description="Current demand vs capacity",
        standard_reference="DNV-RP-B401 §7.7",
        criterion_value=I_final,
        measured_value=total_output,
        unit="A",
        status=ComplianceStatus.COMPLIANT if total_output >= I_final else ComplianceStatus.NON_COMPLIANT,
        notes=f"Total output {total_output:.1f} A vs demand {I_final:.2f} A",
    )

    # Anode life check
    life_check = ComplianceCheck(
        check_id="LIFE-001",
        description="Anode design life vs required life",
        standard_reference="DNV-RP-B401 §7.7.1",
        criterion_value=design_life_years,
        measured_value=profile.end_of_life_year,
        unit="years",
        status=ComplianceStatus.COMPLIANT if profile.end_of_life_year >= design_life_years else ComplianceStatus.NON_COMPLIANT,
        notes=f"EOL year {profile.end_of_life_year:.1f} vs design life {design_life_years:.0f} years",
    )

    # Remaining life summary
    rl_summary = remaining_life_summary(
        system_id="PL-CP-001",
        design_life_years=design_life_years,
        elapsed_years=0.0,
        anode_remaining_life_years=profile.end_of_life_year,
    )

    # Generate report
    report = generate_assessment_report(
        system_id="PL-CP-001",
        asset_description=f"{pipeline_length_km:.0f} km, {pipeline_od_inch}\" offshore pipeline, FBE coated",
        compliance_checks=[pot_check, current_check, life_check],
        remaining_life=rl_summary,
    )

    print(f"\n  Report: {report.report_title}")
    print(f"  Date:   {report.report_date}")
    print(f"  Status: {report.overall_status.value.upper()}")
    print()
    print(f"  EXECUTIVE SUMMARY:")
    print(f"  {report.summary_text}")

    print(f"\n  COMPLIANCE CHECKS:")
    print(f"  {'─' * 70}")
    print(f"  {'ID':<12} {'Description':<35} {'Criterion':<14} {'Measured':<14} {'Status'}")
    print(f"  {'─'*12} {'─'*35} {'─'*14} {'─'*14} {'─'*12}")

    for check in report.compliance_checks:
        print(
            f"  {check.check_id:<12} {check.description:<35} "
            f"{check.criterion_value:<14.3f} {check.measured_value:<14.3f} "
            f"{check.status.value.upper()}"
        )

    if report.recommendations:
        print(f"\n  RECOMMENDATIONS:")
        print(f"  {'─' * 70}")
        for rec in report.recommendations:
            print(f"  [{rec.priority.value.upper()}] {rec.description}")
            print(f"    Action: {rec.action_required}")
            print()

    print(f"  REMAINING LIFE ASSESSMENT:")
    print(f"  {'─' * 55}")
    print(f"  Design life:              {rl_summary.design_life_years:.0f} years")
    print(f"  Remaining anode life:     {rl_summary.remaining_anode_life_years:.1f} years")
    print(f"  Life extension feasible:  {'Yes' if rl_summary.life_extension_feasible else 'No'}")
    print(f"  Limiting factor:          {rl_summary.limiting_factor}")

    # ──────────────────────────────────────────────────────────────────────
    # Design Summary Table
    # ──────────────────────────────────────────────────────────────────────
    separator("DESIGN SUMMARY")

    print(f"""
  Pipeline CP Design Summary — {pipeline_length_km:.0f} km Offshore Pipeline
  {'═' * 60}

  PIPELINE
    Diameter:              {pipeline_od_inch}" OD × {pipeline_wt_inch}" WT
    Length:                 {pipeline_length_km:.0f} km
    Surface Area:          {surface_area:,.0f} m²

  COATING (FBE)
    Initial Breakdown:     {fbe['fc_init']:.1%}
    Mean Breakdown:        {fbe['fc_mean']:.1%}
    Final Breakdown:       {fbe['fc_final']:.1%}

  CURRENT DEMAND
    Mean:                  {I_mean:.2f} A
    Final:                 {I_final:.2f} A

  ANODE SYSTEM
    Type:                  Al-Zn-In Bracelet
    Individual Mass:       {anode_mass_kg:.0f} kg
    Number Required:       {n_anodes}
    Spacing:               {spacing_m:,.0f} m
    Total Weight:          {total_weight_tonnes:.1f} tonnes
    End-of-Life:           Year {profile.end_of_life_year:.0f}

  COMPLIANCE
    Overall Status:        {report.overall_status.value.upper()}
    Checks Passed:         {sum(1 for c in report.compliance_checks if c.status == ComplianceStatus.COMPLIANT)}/{len(report.compliance_checks)}
""")

    # ──────────────────────────────────────────────────────────────────────
    # Footer
    # ──────────────────────────────────────────────────────────────────────
    print("  ╔══════════════════════════════════════════════════════════════════════╗")
    print("  ║  ACE Engineer — Automated CP design from pipeline specs to anode    ║")
    print("  ║  sizing, depletion profiles, and compliance reports.                ║")
    print("  ║                                                                    ║")
    print("  ║  Contact: vamsee.achanta@aceengineer.com | aceengineer.com          ║")
    print("  ╚══════════════════════════════════════════════════════════════════════╝")
    print()


if __name__ == "__main__":
    main()
