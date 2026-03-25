import os
import numpy as np
from pathlib import Path
from digitalmodel.solvers.orcaflex.reporting import generate_orcaflex_report, OrcaFlexAnalysisReport
from digitalmodel.solvers.orcaflex.reporting.models import (
    GeometryData, LineProfileData, KeyPointData,
    MeshData, SegmentData, MeshQualityData,
    DynamicResultsData, TimeSeriesData, EnvelopeData,
    DesignCheckData, UtilizationData
)

# Output directory for generated HTML files
_OUTPUTS_DIR = Path(__file__).parent / "outputs"

def generate_riser_example():
    print("Generating SCR Riser Example...")
    arc_lengths = list(np.linspace(0, 2100, 100))
    x_s = [1210 * (1 - np.cos(s/2100 * np.pi/2)) for s in arc_lengths]
    z_s = [-20 - 1480 * (s/2100) for s in arc_lengths]
    y_s = [0.0] * len(arc_lengths)

    geometry = GeometryData(
        coordinate_system="MSL, z-positive-up",
        water_depth_m=1500.0,
        line_profile=LineProfileData(arc_length=arc_lengths, x=x_s, y=y_s, z=z_s),
        key_points=[
            KeyPointData(label="Hang-off",   arc_length_m=0.0,    x=0.0,    z=-20.0),
            KeyPointData(label="Sag-bend",   arc_length_m=820.0,  x=610.0,  z=-680.0),
            KeyPointData(label="TDP static", arc_length_m=1960.0, x=1210.0, z=-1500.0),
        ]
    )

    mesh = MeshData(
        total_segment_count=210,
        segments=[SegmentData(arc_length_m=s, length_m=10.0) for s in range(0, 2100, 10)],
        quality=MeshQualityData(
            max_adjacent_ratio=1.0,
            worst_ratio_arc_length_m=0.0,
            verdict="PASS",
            adjacent_ratios=[1.0] * 209
        )
    )

    times = list(np.linspace(0, 100, 50))
    dynamic = DynamicResultsData(
        ramp_end_time_s=20.0,
        time_series=[
            TimeSeriesData(id="te_governing",  label="Te @ 820m",
                           t=times, values=[2000 + 100*np.sin(t/10) for t in times],  units="kN"),
            TimeSeriesData(id="bm_governing",  label="BM @ 820m",
                           t=times, values=[500 + 50*np.cos(t/10) for t in times],  units="kN·m"),
        ],
        envelopes=[
            EnvelopeData(id="te_envelope", label="Effective Tension Envelope",
                         arc_length=arc_lengths,
                         max_values=[2200 - 0.1*s for s in arc_lengths], 
                         min_values=[1800 - 0.1*s for s in arc_lengths], 
                         units="kN"),
        ]
    )

    checks = DesignCheckData(
        code="DNV-OS-F201 (2010) + DNV-RP-C203 (2016)",
        checks=[
            UtilizationData(name="Combined Loading §5.4.2.2",
                            value=0.82, allowable=1.0, uc=0.82, pass_fail=True,
                            location_arc_m=820.0, load_case="100yr_collinear"),
            UtilizationData(name="Fatigue damage (F3 weld)",
                            value=0.031, allowable=0.033, uc=0.95, pass_fail=True,
                            location_arc_m=1960.0, load_case="fatigue_scatter"),
            UtilizationData(name="Burst pressure §5.4.2.1",
                            value=28.4, allowable=35.2, uc=0.81, pass_fail=True,
                            location_arc_m=1500.0, load_case="100yr_collinear"),
        ],
    )

    report_data = OrcaFlexAnalysisReport(
        project_name="GoM SCR Design Study",
        structure_id="SCR-001",
        structure_type="riser",
        analyst="J. Smith",
        orcaflex_version="11.4",
        design_codes=["DNV-OS-F201", "DNV-RP-C203"],
        geometry=geometry,
        mesh=mesh,
        dynamic_results=dynamic,
        design_checks=checks,
        recommendations=["Monitor TDP zone fatigue", "Perform VIV assessment"]
    )

    output_path = _OUTPUTS_DIR / "riser_example.html"
    output_path.parent.mkdir(parents=True, exist_ok=True)
    generate_orcaflex_report(report_data, output_path=output_path)
    print(f"Generated riser example at: {output_path}")

def generate_pipeline_example():
    print("Generating Pipeline Example...")
    arc_lengths = list(np.linspace(0, 500, 50))
    geometry = GeometryData(
        coordinate_system="KP, z-positive-up",
        water_depth_m=800.0,
        line_profile=LineProfileData(
            arc_length=arc_lengths,
            x=arc_lengths,
            y=[0.0] * len(arc_lengths),
            z=[-800.0] * len(arc_lengths)
        ),
        key_points=[
            KeyPointData(label="Tie-in A", arc_length_m=0.0, x=0.0, z=-800.0),
            KeyPointData(label="Free-span Mid", arc_length_m=250.0, x=250.0, z=-793.0),
            KeyPointData(label="Tie-in B", arc_length_m=500.0, x=500.0, z=-800.0),
        ]
    )

    checks = DesignCheckData(
        code="DNV-OS-F101, DNV-RP-F105",
        checks=[
            UtilizationData(name="Combined loading - free-span",
                            value=0.87, allowable=1.0, uc=0.87, pass_fail=True,
                            location_arc_m=250.0, load_case="Operation"),
            UtilizationData(name="VIV onset - free-span",
                            value=0.62, allowable=1.0, uc=0.62, pass_fail=True,
                            location_arc_m=250.0, load_case="1yr Current"),
        ],
    )

    report_data = OrcaFlexAnalysisReport(
        project_name="Deepwater Pipeline Study",
        structure_id="PIPE-FL-001",
        structure_type="pipeline",
        analyst="A. Engineer",
        design_codes=["DNV-OS-F101"],
        geometry=geometry,
        design_checks=checks
    )

    output_path = _OUTPUTS_DIR / "pipeline_example.html"
    output_path.parent.mkdir(parents=True, exist_ok=True)
    generate_orcaflex_report(report_data, output_path=output_path)
    print(f"Generated pipeline example at: {output_path}")

if __name__ == "__main__":
    generate_riser_example()
    generate_pipeline_example()
