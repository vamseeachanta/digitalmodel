# ABOUTME: Verifies every strength/capacity result carries its governing
# ABOUTME: code_reference (issue #1092) and that it surfaces in the lookup JSON.
import pandas as pd

from digitalmodel import codes
from digitalmodel.asset_integrity.corroded_pipe import (
    b31g_original,
    modified_b31g,
    rstreng_effective_area,
)
from digitalmodel.asset_integrity.dnv_rp_f101 import dnv_f101_single_defect
from digitalmodel.asset_integrity.ffs_lookup import (
    evaluate_pipe_corroded,
    evaluate_plate_metal_loss,
    record_from_assessment,
)
from digitalmodel.asset_integrity.assessment.ffs_coordinator import (
    FFSComponent,
    assess_component,
)
from digitalmodel.structural.structural_analysis.buckling import (
    ColumnBucklingAnalyzer,
    PlateBucklingAnalyzer,
)
from digitalmodel.structural.structural_analysis.capacity import (
    MemberCapacityChecker,
)
from digitalmodel.structural.structural_analysis.models import (
    PlateGeometry,
    STEEL_AH36,
    STEEL_S355,
)
from digitalmodel.structural.structural_analysis.panel_buckling import (
    StiffenedPanelBucklingAnalyzer,
    StiffenedPanelGeometry,
    StiffenerGeometry,
)
from digitalmodel.structural.structural_analysis.plate_metal_loss_ffs import (
    assess_plate_uniform_loss,
)
from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    WallThicknessAnalyzer,
)


# --- codes module ------------------------------------------------------------
def test_code_reference_label_format():
    assert codes.ASME_B31G.label == "ASME B31G (2012)"
    assert codes.CodeReference("X", "").label == "X"  # no edition
    assert codes.REGISTER  # non-empty register for #1093


# --- corroded pipe (ASME B31G) ----------------------------------------------
def test_corroded_pipe_code_reference():
    for r in (
        b31g_original(20.0, 0.5, 0.2, 4.0, 52_000.0),
        modified_b31g(20.0, 0.5, 0.2, 4.0, 52_000.0),
        rstreng_effective_area(20.0, 0.5, [0.0, 4.0], [0.2, 0.2], 52_000.0),
    ):
        assert r.code_reference == codes.ASME_B31G.label


# --- DNV-RP-F101 -------------------------------------------------------------
def test_dnv_f101_code_reference():
    r = dnv_f101_single_defect(20.0, 0.5, 0.2, 4.0, 66_700.0)
    assert r.code_reference == codes.DNV_RP_F101.label


# --- buckling: plate = DNV-RP-C201, column = Eurocode 3 ----------------------
def test_plate_buckling_code_reference():
    r = PlateBucklingAnalyzer(STEEL_S355).check_plate_buckling(
        PlateGeometry(800.0, 800.0, 12.0), sigma_x=100.0)
    assert r.code_reference == codes.DNV_RP_C201.label


def test_column_buckling_code_reference():
    r = ColumnBucklingAnalyzer(STEEL_S355).check_column_buckling(
        axial_force=1.0e5, area=5000.0, I_min=1.0e7, L_eff=3000.0)
    assert r.code_reference == codes.EUROCODE_3.label


# --- panel buckling (DNV-RP-C201) -------------------------------------------
def test_panel_buckling_code_reference():
    panel = StiffenedPanelGeometry(
        plate_length=2400.0, plate_thickness=12.0,
        stiffener=StiffenerGeometry(
            web_height=250.0, web_thickness=10.0, flange_width=90.0,
            flange_thickness=12.0, spacing=800.0, section_type="tee"),
    )
    r = StiffenedPanelBucklingAnalyzer(STEEL_AH36).check_panel(panel, sigma_x=120.0)
    assert r.code_reference == codes.DNV_RP_C201.label


# --- member capacity (Eurocode 3) -------------------------------------------
def test_capacity_code_reference():
    r = MemberCapacityChecker(STEEL_S355).check_tension_member(
        axial_force=1.0e5, area_gross=5000.0, area_net=4500.0)
    assert r.code_reference == codes.EUROCODE_3.label


# --- metal-loss FFS (API 579 framing over DNV-RP-C201 capacity) -------------
def test_metal_loss_ffs_code_reference():
    r = assess_plate_uniform_loss(
        PlateGeometry(800.0, 800.0, 12.0), STEEL_AH36, 2.0, sigma_x=120.0)
    assert "API 579" in r.code_reference
    assert "DNV-RP-C201" in r.code_reference


# --- FFS coordinator (API 579) ----------------------------------------------
def _component():
    return FFSComponent(
        component_id="LINE-001", design_code="B31.8", nominal_od_in=12.75,
        nominal_wt_in=0.500, design_pressure_psi=1000.0, smys_psi=52_000.0,
        corrosion_rate_in_per_yr=0.005, rsf_a=0.90)


def test_ffs_coordinator_code_reference_and_to_dict():
    grid = pd.DataFrame([[0.47] * 5 for _ in range(5)])
    result = assess_component(_component(), grid)
    assert result.code_reference == codes.API_579.label
    assert result.to_dict()["code_reference"] == codes.API_579.label


# --- wall thickness (governing design code) ---------------------------------
def test_wall_thickness_code_reference():
    result = WallThicknessAnalyzer(
        PipeGeometry(outer_diameter=0.3239, wall_thickness=0.0127,
                     corrosion_allowance=0.003),
        PipeMaterial(grade="X65", smys=448e6, smts=531e6),
        DesignLoads(internal_pressure=0.0, external_pressure=15.08e6),
        DesignFactors(), DesignCode.DNV_ST_F101,
    ).perform_analysis()
    assert result.code_reference == codes.DNV_ST_F101.label


# --- lookup / dashboard serialization ---------------------------------------
def test_lookup_records_carry_code_reference():
    rec = evaluate_pipe_corroded(
        method="modified_b31g", grade="X52", D_in=20.0, t_in=0.5,
        d_in=0.2, L_in=4.0)
    assert rec["code_reference"] == codes.ASME_B31G.label

    rec2 = evaluate_plate_metal_loss(
        grade="AH36", length_mm=800.0, width_mm=800.0, thickness_mm=12.0,
        sigma_x_mpa=120.0, metal_loss_mm=2.0)
    assert "DNV-RP-C201" in rec2["code_reference"]

    grid = pd.DataFrame([[0.47] * 5 for _ in range(5)])
    rec3 = record_from_assessment(assess_component(_component(), grid))
    assert rec3["code_reference"] == codes.API_579.label
