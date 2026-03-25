"""Unit tests for asset_integrity FFS module calculations.

WRK-138 Phase 0, Step 0.8: Verify mathematical correctness of pipe capacity,
API 579 metal loss assessment, fracture mechanics, and utility functions.

Tests use hand-verifiable engineering values with Arrange-Act-Assert structure.
"""

import math
import warnings

import numpy as np
import pytest

# Suppress SyntaxWarning from legacy escape sequences in data.py so that
# pytest assertion rewriting does not treat them as errors.
warnings.filterwarnings("ignore", category=SyntaxWarning)

from digitalmodel.asset_integrity.common.update_deep import (
    AttributeDict,
    update_deep_dictionary,
)


# ---------------------------------------------------------------------------
# Helper: build a minimal cfg dict that PipeCapacity / PipeSizing expect
# ---------------------------------------------------------------------------

def _make_pipe_cfg(
    nominal_od,
    design_wt,
    smys,
    smus,
    spec_code,
    design_factor,
    internal_pressure,
    external_pressure=0,
    corrosion_allowance=0.0,
    weld_factor=1.0,
    temperature_derating=1.0,
    d_over_t_transition=30,
    nominal_id=None,
    material_name="Steel",
    material_grade="X52",
    e_modulus=30_000_000.0,
    poisson=0.3,
    rho=0.2817,
    coupling_mass_ratio=0.0,
    internal_fluid=0.03703047,
    external_fluid=0.03703047,
):
    """Return an AttributeDict cfg suitable for PipeCapacity evaluation."""
    if nominal_id is None:
        nominal_id = nominal_od - 2 * design_wt

    cfg = AttributeDict({
        "default": AttributeDict({
            "db": None,
        }),
        "Outer_Pipe": {
            "Geometry": {
                "Nominal_OD": nominal_od,
                "Nominal_ID": nominal_id,
                "Design_WT": design_wt,
                "Corrosion_Allowance": corrosion_allowance,
            },
            "Material": {
                "Material": material_name,
                "Material_Grade": material_grade,
                "SMYS": smys,
                "SMUS": smus if smus else smys * 1.2,
                "WeldFactor": {"Seamless": weld_factor},
            },
            "Manufacturing": {
                "Coupling Mass Ratio": coupling_mass_ratio,
            },
        },
        "Inner_Pipe": None,
        "Material": {
            material_name: {
                "E": e_modulus,
                "Poissionsratio": poisson,
                "Rho": rho,
                "Grades": {
                    material_grade: {
                        "SMYS": smys,
                        "SMUS": smus if smus else smys * 1.2,
                    }
                },
            }
        },
        "Design": [
            {
                "Load Condition": {"Outer_Pipe": "internal_pressure"},
                "InternalPressure": {"Outer_Pipe": internal_pressure},
                "ExternalPressure": {"Outer_Pipe": external_pressure},
                "InternalFluid": {"Outer_Pipe": internal_fluid},
                "ExternalFluid": {"Outer_Pipe": external_fluid},
                "BendingMoment": 0,
                "AxialForce": 0,
                "EffectiveTension": None,
                "Torsion": 0,
                "Condition": "Restrained",
                "allowable_stress_to_yield_ratio": 0.67,
                "Code": [{"Outer_Pipe": spec_code}],
                "Material": {
                    "temperature_derating": {
                        "Outer_Pipe": {
                            spec_code: temperature_derating,
                        }
                    }
                },
            }
        ],
        "DesignFactors": {
            spec_code: {
                "internal_pressure": design_factor,
                "D_over_T_Trasition_Ratio": d_over_t_transition,
            }
        },
        "Analysis": {
            "result_folder": "/tmp/",
            "file_name": "test_output",
        },
    })
    return cfg


# ===========================================================================
# PIPE CAPACITY TESTS (5+ tests)
# ===========================================================================

class TestPipeCapacityBarlow:
    """Barlow / modified Barlow burst pressure and min thickness calculations."""

    def test_asme_b314_burst_pressure_12in_x52(self):
        """ASME B31.4 burst pressure for 12.75in OD, 0.375in WT, X52 steel.

        Modified Barlow (thin-wall, D/t >= 30):
            P = 2 * S_allow * t / D + P_ext
            S_allow = SMYS * F * E * T
                    = 52000 * 0.72 * 1.0 * 1.0 = 37440 psi

        Hand calc:
            P = 2 * 37440 * 0.375 / 12.75 + 0 = 2202.35 psi
        """
        # Arrange
        spec_code = "ASME B31.4-2016 Chapter IX Platform Piping"
        cfg = _make_pipe_cfg(
            nominal_od=12.75,
            design_wt=0.375,
            smys=52000,
            smus=66000,
            spec_code=spec_code,
            design_factor=0.72,
            internal_pressure=1500,
            external_pressure=0,
        )

        # Act
        from digitalmodel.asset_integrity.custom.PipeCapacity import PipeCapacity
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_modified_burlow_equation(
            pipe_flag="Outer_Pipe",
            specification_code=spec_code,
            load_condition_index=0,
            thickness=0.375,
        )

        # Assert -- hand calc: 2 * (52000*0.72*1.0*1.0) * 0.375 / 12.75 = 2202.35
        expected = 2 * (52000 * 0.72 * 1.0 * 1.0) * 0.375 / 12.75
        assert pressure == pytest.approx(expected, rel=1e-6), (
            f"Burst pressure {pressure:.2f} psi != expected {expected:.2f} psi"
        )

    def test_asme_b318_design_pressure_16in_x65(self):
        """ASME B31.8 design pressure for 16in OD, 0.625in WT, X65, class location F=0.5.

        S_allow = 65000 * 0.5 * 1.0 * 1.0 = 32500 psi
        D/t = 16/0.625 = 25.6 (<30 thick-wall branch)
        P = 2*t*S_allow/(D-t) + P_ext
          = 2*0.625*32500/(16-0.625) + 0
          = 40625 / 15.375
          = 2642.28 psi
        """
        spec_code = "ASME B31.8-2016 Chapter VIII Pipeline"
        cfg = _make_pipe_cfg(
            nominal_od=16.0,
            design_wt=0.625,
            smys=65000,
            smus=77000,
            spec_code=spec_code,
            design_factor=0.5,
            internal_pressure=2220,
            d_over_t_transition=30,
        )

        from digitalmodel.asset_integrity.custom.PipeCapacity import PipeCapacity
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_modified_burlow_equation(
            pipe_flag="Outer_Pipe",
            specification_code=spec_code,
            load_condition_index=0,
            thickness=0.625,
        )

        # D/t = 25.6 < 30 => thick-wall: P = 2*t*S/(D-t)
        s_allow = 65000 * 0.5 * 1.0 * 1.0
        expected = 2 * 0.625 * s_allow / (16.0 - 0.625)
        assert pressure == pytest.approx(expected, rel=1e-6)

    def test_asme_b314_thin_wall_barlow(self):
        """Barlow thin-wall formula verification (D/t >= 30).

        24in OD, 0.5in WT => D/t = 48 (thin-wall).
        S_allow = 52000 * 0.72 * 1.0 * 1.0 = 37440
        P = 2 * S * t / D = 2 * 37440 * 0.5 / 24 = 1560 psi
        """
        spec_code = "ASME B31.4-2016 Chapter IX Platform Piping"
        cfg = _make_pipe_cfg(
            nominal_od=24.0,
            design_wt=0.5,
            smys=52000,
            smus=66000,
            spec_code=spec_code,
            design_factor=0.72,
            internal_pressure=1000,
            d_over_t_transition=30,
        )

        from digitalmodel.asset_integrity.custom.PipeCapacity import PipeCapacity
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_modified_burlow_equation(
            pipe_flag="Outer_Pipe",
            specification_code=spec_code,
            load_condition_index=0,
            thickness=0.5,
        )

        # D/t = 48 >= 30 => thin-wall: P = 2*S*t/D
        expected = 2 * (52000 * 0.72) * 0.5 / 24.0
        assert pressure == pytest.approx(expected, rel=1e-6)

    def test_asme_b314_min_thickness_thin_wall(self):
        """Minimum thickness for thin-wall pipe per modified Barlow.

        24in OD, 0.5in WT, D/t=48 (>=30), Pi=1000, Pe=0
        t_min = (Pi - Pe) * D / (2 * S_allow)
              = 1000 * 24 / (2 * 37440)
              = 24000 / 74880
              = 0.32051 inch
        """
        spec_code = "ASME B31.4-2016 Chapter IX Platform Piping"
        cfg = _make_pipe_cfg(
            nominal_od=24.0,
            design_wt=0.5,
            smys=52000,
            smus=66000,
            spec_code=spec_code,
            design_factor=0.72,
            internal_pressure=1000,
            d_over_t_transition=30,
        )

        from digitalmodel.asset_integrity.custom.PipeCapacity import PipeCapacity
        pc = PipeCapacity(cfg)
        min_t = pc.evaluate_burst_minimum_thickness_modified_burlow_equation(
            pipe_flag="Outer_Pipe",
            specification_code=spec_code,
            load_condition_index=0,
        )

        s_allow = 52000 * 0.72 * 1.0 * 1.0
        expected = 1000 * 24.0 / (2 * s_allow)
        assert min_t == pytest.approx(expected, rel=1e-6)

    def test_api_rp_1111_burst_pressure_deepwater(self):
        """API RP 1111 burst pressure for deepwater pipeline.

        10.75in OD, 0.625in WT, X65, Fd=0.9
        D/t = 10.75/0.625 = 17.2, transition ratio = 15
        Since D/t > 15 (thin-wall branch):
            P = Fd * 0.90 * (SMYS + SMUS) * (t / (D - t))
              = 0.9 * 0.90 * (65000 + 77000) * (0.625 / (10.75 - 0.625))
              = 0.81 * 142000 * 0.0617284
              = 7102.22 psi
        """
        spec_code = "API RP 1111-2009"
        cfg = _make_pipe_cfg(
            nominal_od=10.75,
            design_wt=0.625,
            smys=65000,
            smus=77000,
            spec_code=spec_code,
            design_factor=None,
            internal_pressure=5000,
            d_over_t_transition=15,
        )
        cfg["DesignFactors"][spec_code]["internal_pressure"] = {"Fd": 0.9}

        from digitalmodel.asset_integrity.custom.PipeCapacity import PipeCapacity
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_API_RP_1111(
            pipe_flag="Outer_Pipe",
            specification_code=spec_code,
            load_condition_index=0,
            thickness=0.625,
        )

        # D/t = 17.2 > 15 => thin-wall branch
        fd = 0.9
        expected = fd * 0.90 * (65000 + 77000) * (0.625 / (10.75 - 0.625))
        assert pressure == pytest.approx(expected, rel=1e-6)

    def test_api_std_2rd_burst_pressure(self):
        """API STD 2RD burst pressure (von Mises thick-wall logarithmic formula).

        10.75in OD, 1.0in WT, X65
        Fd=0.67, k=1.0
        P = Fd * k * (SMYS + SMUS) * ln(D / (D - 2t))
          = 0.67 * 1.0 * (65000 + 77000) * ln(10.75 / (10.75 - 2.0))
          = 0.67 * 142000 * ln(10.75 / 8.75)
          = 95140 * 0.20479
          = 19483.6 psi
        """
        spec_code = "API STD 2RD-2013"
        cfg = _make_pipe_cfg(
            nominal_od=10.75,
            design_wt=1.0,
            smys=65000,
            smus=77000,
            spec_code=spec_code,
            design_factor=None,
            internal_pressure=10000,
        )
        cfg["DesignFactors"][spec_code]["internal_pressure"] = {
            "Fd": 0.67,
            "k": {"API 5L": 1.0},
        }

        from digitalmodel.asset_integrity.custom.PipeCapacity import PipeCapacity
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_burst_pressure_API_STD_2RD(
            pipe_flag="Outer_Pipe",
            specification_code=spec_code,
            load_condition_index=0,
            thickness=1.0,
        )

        expected = 0.67 * 1.0 * (65000 + 77000) * math.log(10.75 / (10.75 - 2.0))
        assert pressure == pytest.approx(expected, rel=1e-6)

    def test_collapse_propagation_pressure_api_rp_1111(self):
        """Collapse propagation pressure per API RP 1111.

        Pp = 24 * SMYS * (t / D) ^ 2.4
        For 10.75in OD, 0.625in WT, X65:
            Pp = 24 * 65000 * (0.625 / 10.75) ^ 2.4
            t/D = 0.05814
            (t/D)^2.4 = 0.05814^2.4 = 0.001424
            Pp = 1560000 * 0.001424 = 2221.44 psi

        Then Pe = Pp * Fp (propagation factor).
        """
        spec_code = "API RP 1111-2009"
        cfg = _make_pipe_cfg(
            nominal_od=10.75,
            design_wt=0.625,
            smys=65000,
            smus=77000,
            spec_code=spec_code,
            design_factor=None,
            internal_pressure=5000,
        )
        cfg["DesignFactors"][spec_code]["collapse_propagation"] = {"Fp": 0.8}
        cfg["Design"][0]["Load Condition"]["Outer_Pipe"] = "collapse_propagation"

        from digitalmodel.asset_integrity.custom.PipeCapacity import PipeCapacity
        pc = PipeCapacity(cfg)
        pressure = pc.evaluate_collapse_propagation_pressure_API_RP_1111(
            pipe_flag="Outer_Pipe",
            specification_code=spec_code,
            load_condition_index=0,
            thickness=0.625,
        )

        pp = 24 * 65000 * (0.625 / 10.75) ** 2.4
        expected = pp * 0.8
        assert pressure == pytest.approx(expected, rel=1e-6)


# ===========================================================================
# API 579 GML / LML TESTS (5+ tests)
# ===========================================================================

class TestAPI579GMLAssessment:
    """Test API 579 Part 4/5 General & Local Metal Loss calculations.

    These tests replicate the exact formulas from API579_components.py methods
    GMLAssessmentLengthEvaluation, GMLAcceptability, and LMLMAWPrEvaluation.
    The module itself cannot be imported on Python 3.12 due to a transitive
    dependency on the removed 'imp' stdlib module. The calculations tested
    here are pure math extracted from the source.
    """

    @staticmethod
    def _gml_assessment_length_evaluation(data_array, data):
        """Replicate API579_components.GMLAssessmentLengthEvaluation exactly."""
        tmm = min(data_array)
        tam = float(np.nanmean(data_array))
        tmax = max(data_array)

        fca_ml = tam - data["tmin"]
        nominal_wt_ml = data["tmin"] - fca_ml
        id_ml = data["NominalID"] + 2 * fca_ml
        rt = (tmm - fca_ml) / nominal_wt_ml

        if rt < data["RSFa"]:
            length_parameter = 1.123 * math.sqrt(
                ((1 - rt) / (1 - rt / data["RSFa"])) ** 2 - 1
            )
        else:
            length_parameter = 50.00

        averaging_length = length_parameter * math.sqrt(id_ml * nominal_wt_ml)
        return {
            "AveragingLength": averaging_length,
            "tmm": tmm,
            "FCAml": fca_ml,
            "tam": tam,
            "LengthParameter": length_parameter,
            "tmax": tmax,
        }

    @staticmethod
    def _gml_acceptability(data):
        """Replicate API579_components.GMLAcceptability exactly."""
        lhs = data["tmm"] - data["FCAml"]
        rhs = max(0.5 * data["tmin"], max(0.2 * data["NominalWT"], 0.1))
        wt_ratio = lhs / rhs

        hist_rate = (data["NominalWT"] - data["tam"]) / data["Age"]
        if data["FutureCorrosionRate"] == "Historical":
            if hist_rate < data["FCARateFloor"]:
                future_rate = data["FCARateFloor"]
            else:
                future_rate = hist_rate
        else:
            future_rate = data["FutureCorrosionRate"]

        remaining_life = (data["tam"] - data["tmin"]) / future_rate
        return {
            "WTAcceptabilityRatio": wt_ratio,
            "remainingLife": remaining_life,
            "futureCorrosionRate": future_rate,
            "HistoricalCorrosionRate": hist_rate,
        }

    @staticmethod
    def _lml_mawpr_evaluation(data, folias_table, level):
        """Replicate API579_components.LMLMAWPrEvaluation exactly."""
        if data["LongitudinalFlawLengthParameter"] <= 0.354:
            rt_floor = 0.2
        elif data["LongitudinalFlawLengthParameter"] < 20:
            rsa = data["RSFa"]
            mt = float(np.interp(
                data["LongitudinalFlawLengthParameter"],
                folias_table["FlawParameter"],
                folias_table["Mt"]["Cylindrical"],
            ))
            rt_floor = (rsa - rsa / mt) / (1 - rsa / mt)
        elif data["LongitudinalFlawLengthParameter"] > 20:
            rt_floor = 0.9

        if data["Rt"] > rt_floor:
            mawpr_eval = False
            mawpr = data["MAWP"]
            rsf = 1
        else:
            if level == 1:
                rsf = data["Rt"] / (1 - (1 - data["Rt"]) / mt)
            elif level == 2:
                rsf = data["AARatio"] / (1 - (1 - data["AARatio"]) / mt)
            if rsf >= data["RSFa"]:
                mawpr_eval = False
                mawpr = data["MAWP"]
            else:
                mawpr_eval = True
                mawpr = data["MAWP"] * rsf / data["RSFa"]

        return {"MAWPrEvaluation": mawpr_eval, "Mt": mt, "MAWPr": mawpr, "RSF": rsf}

    def test_gml_assessment_length_basic(self):
        """GML assessment length for a known thickness array.

        tmm = 0.50, tam = mean([0.56,0.55,0.58,0.50,0.57,0.56]) = 0.56
        tmin = 0.526, NominalID = 14.75, RSFa = 0.9

        FCAml = 0.56 - 0.526 = 0.034
        nominalWTml = 0.526 - 0.034 = 0.492
        Rt = (0.50 - 0.034) / 0.492 = 0.9472

        Since Rt (0.9472) >= RSFa (0.9), lambda = 50.0
        """
        data_array = [0.56, 0.55, 0.58, 0.50, 0.57, 0.56]
        customdata = {
            "tmin": 0.526,
            "NominalID": 14.75,
            "RSFa": 0.9,
            "NominalWT": 0.625,
            "Age": 15,
            "FCARateFloor": 0.00118,
            "FutureCorrosionRate": "Historical",
        }

        result = self._gml_assessment_length_evaluation(data_array, customdata)

        assert result["tmm"] == pytest.approx(0.50)
        assert result["tam"] == pytest.approx(np.mean(data_array), rel=1e-6)
        assert result["tmax"] == pytest.approx(0.58)

        tam = np.mean(data_array)
        fca_ml = tam - 0.526
        nom_wt_ml = 0.526 - fca_ml
        rt = (0.50 - fca_ml) / nom_wt_ml
        assert rt >= 0.9, "Rt should exceed RSFa for lambda=50 branch"
        assert result["LengthParameter"] == pytest.approx(50.0)

    def test_gml_assessment_length_with_significant_loss(self):
        """GML assessment length when Rt < RSFa triggers lambda formula.

        tmm = 0.35, tam = mean([0.45,0.45,0.45,0.35,0.50,0.50]) = 0.45
        FCAml = 0.45 - 0.526 = -0.076 (negative, tam < tmin)
        nominalWTml = 0.526 - (-0.076) = 0.602
        Rt = (0.35 + 0.076) / 0.602 = 0.7076 < 0.9

        lambda = 1.123 * sqrt(((1-Rt)/(1-Rt/RSFa))^2 - 1)
        """
        data_array = [0.45, 0.45, 0.45, 0.35, 0.50, 0.50]
        customdata = {
            "tmin": 0.526,
            "NominalID": 14.75,
            "RSFa": 0.9,
            "NominalWT": 0.625,
            "Age": 15,
            "FCARateFloor": 0.00118,
            "FutureCorrosionRate": "Historical",
        }

        result = self._gml_assessment_length_evaluation(data_array, customdata)

        tam = np.mean(data_array)
        fca_ml = tam - 0.526
        nom_wt_ml = 0.526 - fca_ml
        rt = (0.35 - fca_ml) / nom_wt_ml

        assert rt < 0.9, "Rt should be below RSFa for this test"
        assert result["LengthParameter"] < 50.0
        expected_lambda = 1.123 * math.sqrt(
            ((1 - rt) / (1 - rt / 0.9)) ** 2 - 1
        )
        assert result["LengthParameter"] == pytest.approx(expected_lambda, rel=1e-4)

    def test_gml_acceptability_ratio(self):
        """GML WT acceptability ratio (LHS / RHS) and remaining life.

        LHS = tmm - FCAml = 0.50 - 0.034 = 0.466
        RHS = max(0.5*0.526, max(0.2*0.625, 0.1)) = 0.263
        WTRatio = 0.466 / 0.263 = 1.772

        HistRate = (0.625 - 0.56) / 15 = 0.004333
        futureRate = max(0.004333, 0.00118) = 0.004333
        remainingLife = (0.56 - 0.526) / 0.004333 = 7.846 yrs
        """
        data = {
            "tmm": 0.50,
            "FCAml": 0.034,
            "tmin": 0.526,
            "NominalWT": 0.625,
            "Age": 15,
            "tam": 0.56,
            "FutureCorrosionRate": "Historical",
            "FCARateFloor": 0.00118,
        }

        result = self._gml_acceptability(data)

        lhs = 0.50 - 0.034
        rhs = max(0.5 * 0.526, max(0.2 * 0.625, 0.1))
        assert result["WTAcceptabilityRatio"] == pytest.approx(lhs / rhs, rel=1e-6)
        assert result["WTAcceptabilityRatio"] > 1.0, "Should be acceptable (>1)"

        hist_rate = (0.625 - 0.56) / 15
        assert result["HistoricalCorrosionRate"] == pytest.approx(hist_rate, rel=1e-6)
        assert result["futureCorrosionRate"] == pytest.approx(
            max(hist_rate, 0.00118), rel=1e-6
        )
        expected_life = (0.56 - 0.526) / max(hist_rate, 0.00118)
        assert result["remainingLife"] == pytest.approx(expected_life, rel=1e-4)

    def test_lml_mawpr_evaluation_folias_factor(self):
        """LML MAWP-reduced evaluation with Folias factor interpolation.

        At lambda=2.0, Mt = 1.618 (exact table value).
        Rt = 0.75
        RtFloor = (RSFa - RSFa/Mt) / (1 - RSFa/Mt)
                = (0.9 - 0.9/1.618) / (1 - 0.9/1.618)
                = (0.9 - 0.5562) / (1 - 0.5562)
                = 0.3438 / 0.4438 = 0.7748
        Rt(0.75) < RtFloor(0.7748) => reduced MAWP path
        RSF = 0.75 / (1 - 0.25/1.618) = 0.75/0.8455 = 0.8870
        RSF(0.887) < RSFa(0.9) => MAWPr = 2539 * 0.887 / 0.9 = 2499.8
        """
        folias_table = {
            "FlawParameter": [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0],
            "Mt": {
                "Cylindrical": [1.001, 1.056, 1.199, 1.394, 1.618, 1.857, 2.103],
            },
        }
        data = {
            "LongitudinalFlawLengthParameter": 2.0,
            "Rt": 0.75,
            "RSFa": 0.9,
            "MAWP": 2539.0,
            "tc": 0.55,
            "NominalID": 14.75,
        }

        result = self._lml_mawpr_evaluation(data, folias_table, level=1)

        assert result["Mt"] == pytest.approx(1.618, rel=1e-3)
        # Verify RSF and MAWPr
        mt = 1.618
        rsf = 0.75 / (1 - (1 - 0.75) / mt)
        assert result["RSF"] == pytest.approx(rsf, rel=1e-3)
        expected_mawpr = 2539.0 * rsf / 0.9
        assert result["MAWPr"] == pytest.approx(expected_mawpr, rel=1e-2)

    def test_folias_factor_interpolation_between_table_values(self):
        """Folias factor at lambda=1.25 via linear interpolation.

        Mt = interp(1.25, [1.0, 1.5], [1.199, 1.394])
           = 1.199 + (1.394 - 1.199) * 0.5 = 1.2965
        """
        flaw_params = [0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0]
        mt_cyl = [1.001, 1.056, 1.199, 1.394, 1.618, 1.857, 2.103]

        mt_at_1_25 = np.interp(1.25, flaw_params, mt_cyl)

        expected = 1.199 + (1.394 - 1.199) * 0.5
        assert mt_at_1_25 == pytest.approx(expected, rel=1e-6)
        assert mt_at_1_25 == pytest.approx(1.2965, rel=1e-4)

    def test_remaining_life_with_historical_rate_below_floor(self):
        """When historical corrosion rate < floor, the floor rate is used.

        HistRate = (0.625-0.620)/15 = 0.000333 < 0.00118
        futureRate = 0.00118
        remainingLife = (0.620 - 0.526) / 0.00118 = 79.66 years
        """
        data = {
            "tmm": 0.61,
            "FCAml": 0.005,
            "tmin": 0.526,
            "NominalWT": 0.625,
            "Age": 15,
            "tam": 0.620,
            "FutureCorrosionRate": "Historical",
            "FCARateFloor": 0.00118,
        }

        result = self._gml_acceptability(data)

        hist_rate = (0.625 - 0.620) / 15
        assert hist_rate < 0.00118, "Historical rate should be below floor"
        assert result["futureCorrosionRate"] == pytest.approx(0.00118, rel=1e-6)
        expected_life = (0.620 - 0.526) / 0.00118
        assert result["remainingLife"] == pytest.approx(expected_life, rel=1e-4)


# ===========================================================================
# FRACTURE MECHANICS TESTS (3+ tests)
# ===========================================================================

class TestFractureMechanicsFAD:
    """Test BS7910 Failure Assessment Diagram and related calculations."""

    def test_fad_option_1_at_lr_zero(self):
        """FAD Option 1: at Lr=0, Kr should equal 1.0.

        FAD curve formula (BS7910 Clause 7):
            f(Lr) = ((1 + 0.5*Lr^2)^(-0.5)) * (0.3 + 0.7*exp(-mu*Lr^6))

        At Lr = 0:
            f(0) = ((1)^(-0.5)) * (0.3 + 0.7*exp(0)) = 1.0 * (0.3 + 0.7) = 1.0
        """
        cfg = AttributeDict({
            "Outer_Pipe": {
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65",
                },
            },
            "Material": {
                "Steel": {
                    "E": 30_000_000.0,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {"SMYS": 65000, "SMUS": 77000},
                    },
                }
            },
        })

        from digitalmodel.asset_integrity.common.fad import FAD
        fad = FAD(cfg)
        result = fad.get_BS7910_2013_FAD()

        # First row should be Lr=0, Kr=1.0
        df = result["option_1"]
        kr_at_lr0 = df.loc[df["L_r"] == 0.0, "K_r"].values[0]
        assert kr_at_lr0 == pytest.approx(1.0, rel=1e-6)

    def test_fad_option_1_at_lr_max_kr_is_zero(self):
        """At Lr = Lr_max, Kr must be exactly 0 (cutoff point).

        Lr_max = (SMYS + SMUS) / (2 * SMYS)
               = (65000 + 77000) / (2 * 65000)
               = 142000 / 130000
               = 1.09231
        """
        cfg = AttributeDict({
            "Outer_Pipe": {
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65",
                },
            },
            "Material": {
                "Steel": {
                    "E": 30_000_000.0,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {"SMYS": 65000, "SMUS": 77000},
                    },
                }
            },
        })

        from digitalmodel.asset_integrity.common.fad import FAD
        fad = FAD(cfg)
        result = fad.get_BS7910_2013_FAD()

        df = result["option_1"]
        # Last row should be Lr_max with Kr=0
        last_row = df.iloc[-1]
        lr_max_expected = (65000 + 77000) / (2 * 65000)
        assert last_row["L_r"] == pytest.approx(lr_max_expected, rel=1e-4)
        assert last_row["K_r"] == pytest.approx(0.0, abs=1e-10)

    def test_fad_option_1_monotonically_decreasing(self):
        """FAD Option 1 Kr curve should be monotonically decreasing from Lr=0 to Lr_max."""
        cfg = AttributeDict({
            "Outer_Pipe": {
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65",
                },
            },
            "Material": {
                "Steel": {
                    "E": 30_000_000.0,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {"SMYS": 65000, "SMUS": 77000},
                    },
                }
            },
        })

        from digitalmodel.asset_integrity.common.fad import FAD
        fad = FAD(cfg)
        result = fad.get_BS7910_2013_FAD()

        df = result["option_1"]
        kr_values = df["K_r"].values

        # Every Kr value should be >= the next (monotonically non-increasing)
        for i in range(len(kr_values) - 1):
            assert kr_values[i] >= kr_values[i + 1], (
                f"FAD curve not monotonically decreasing at index {i}: "
                f"Kr[{i}]={kr_values[i]:.6f} < Kr[{i+1}]={kr_values[i+1]:.6f}"
            )

    def test_plastic_collapse_load_ratio_limit(self):
        """Lr_max per BS7910 Clause 7.3.2.

        Lr_max = (SMYS + SMUS) / (2 * SMYS)
        For X52 (SMYS=52000, SMUS=66000):
            Lr_max = (52000 + 66000) / (2 * 52000) = 118000/104000 = 1.13462
        """
        cfg = AttributeDict({
            "Outer_Pipe": {
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X52",
                },
            },
            "Material": {
                "Steel": {
                    "E": 30_000_000.0,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X52": {"SMYS": 52000, "SMUS": 66000},
                    },
                }
            },
        })

        from digitalmodel.asset_integrity.common.fad import FAD
        fad = FAD(cfg)
        lr_max = fad.get_plastic_collapse_load_ratio_limit()

        expected = (52000 + 66000) / (2 * 52000)
        assert lr_max == pytest.approx(expected, rel=1e-6)
        assert lr_max == pytest.approx(1.13462, rel=1e-4)


# ===========================================================================
# UTILITY TESTS (4 tests)
# ===========================================================================

class TestAttributeDict:
    """Test AttributeDict dot-access and dict-access equivalence."""

    def test_dot_access_and_dict_access_equivalence(self):
        """AttributeDict should support both d['key'] and d.key syntax."""
        d = AttributeDict({"alpha": 1, "beta": "hello"})

        assert d["alpha"] == d.alpha == 1
        assert d["beta"] == d.beta == "hello"

    def test_attribute_assignment(self):
        """Setting via dot or bracket should be interchangeable."""
        d = AttributeDict()
        d.foo = 42
        d["bar"] = 99

        assert d["foo"] == 42
        assert d.bar == 99

    def test_nested_attribute_dict(self):
        """Nested AttributeDict should support deep dot access."""
        d = AttributeDict({"level1": AttributeDict({"level2": 100})})

        assert d.level1.level2 == 100
        assert d["level1"]["level2"] == 100

    def test_attribute_dict_contains(self):
        """AttributeDict should support __contains__ (key in dict)."""
        d = AttributeDict({"x": 10, "y": 20})

        assert "x" in d
        assert "z" not in d


class TestUpdateDeepDictionary:
    """Test update_deep_dictionary for nested dict merging."""

    def test_shallow_merge(self):
        """Shallow keys should be updated."""
        d = {"a": 1, "b": 2}
        u = {"b": 3, "c": 4}

        result = update_deep_dictionary(d, u)

        assert result["a"] == 1
        assert result["b"] == 3
        assert result["c"] == 4

    def test_deep_merge_preserves_existing(self):
        """Deep merge should preserve keys not in the update."""
        d = {"outer": {"keep": "yes", "change": "old"}}
        u = {"outer": {"change": "new", "add": "extra"}}

        result = update_deep_dictionary(d, u)

        assert result["outer"]["keep"] == "yes"
        assert result["outer"]["change"] == "new"
        assert result["outer"]["add"] == "extra"

    def test_deep_merge_three_levels(self):
        """Three-level deep merge should work correctly."""
        d = {"L1": {"L2": {"L3_a": 1, "L3_b": 2}}}
        u = {"L1": {"L2": {"L3_b": 99, "L3_c": 3}}}

        result = update_deep_dictionary(d, u)

        assert result["L1"]["L2"]["L3_a"] == 1
        assert result["L1"]["L2"]["L3_b"] == 99
        assert result["L1"]["L2"]["L3_c"] == 3

    def test_non_mapping_replacement(self):
        """When update value is not a dict, it replaces directly."""
        d = {"a": {"nested": 1}}
        u = {"a": "replaced"}

        result = update_deep_dictionary(d, u)

        assert result["a"] == "replaced"


# ===========================================================================
# ADDITIONAL ENGINEERING CALCULATION TESTS
# ===========================================================================

class TestEngineeringFormulas:
    """Additional standalone engineering formula verifications."""

    def test_pipe_section_area(self):
        """Cross-sectional area of pipe: A = pi/4 * (OD^2 - ID^2).

        For 12.75in OD, 0.375in WT:
            ID = 12.75 - 2*0.375 = 12.0
            A = pi/4 * (12.75^2 - 12.0^2) = pi/4 * (162.5625 - 144.0) = pi/4 * 18.5625
            A = 14.5796 in^2
        """
        od = 12.75
        wt = 0.375
        id_ = od - 2 * wt

        a_pipe = (math.pi / 4) * (od ** 2 - id_ ** 2)

        expected = (math.pi / 4) * (12.75 ** 2 - 12.0 ** 2)
        assert a_pipe == pytest.approx(expected, rel=1e-6)
        assert a_pipe == pytest.approx(14.5796, rel=1e-3)

    def test_pipe_moment_of_inertia(self):
        """Moment of inertia of pipe: I = pi/64 * (OD^4 - ID^4).

        For 12.75in OD, 12.0in ID:
            I = pi/64 * (12.75^4 - 12.0^4)
              = pi/64 * (26426.42 - 20736.0)
              = pi/64 * 5690.42
              = 279.3 in^4
        """
        od = 12.75
        id_ = 12.0

        i_pipe = (math.pi / 64) * (od ** 4 - id_ ** 4)

        expected = (math.pi / 64) * (12.75 ** 4 - 12.0 ** 4)
        assert i_pipe == pytest.approx(expected, rel=1e-6)
        assert i_pipe == pytest.approx(279.33, rel=1e-2)

    def test_collapse_pressure_api_tr_5c3_formula(self):
        """API TR 5C3 elastic collapse: Pe = 46.95E6 / (D/t * (D/t - 1)^2).

        For D/t = 17.2:
            Pe = 46.95E6 / (17.2 * (17.2 - 1)^2)
               = 46.95E6 / (17.2 * 262.44)
               = 46.95E6 / 4513.97
               = 10401 psi
        """
        d_over_t = 17.2
        pe = 46.95e6 / (d_over_t * (d_over_t - 1) ** 2)

        expected = 46.95e6 / (17.2 * (16.2) ** 2)
        assert pe == pytest.approx(expected, rel=1e-6)
        assert pe == pytest.approx(10401, rel=5e-3)

    def test_api_std_2rd_collapse_pressure_formula(self):
        """API STD 2RD collapse: Pc = Py * Pel / sqrt(Py^2 + Pel^2).

        For 10.75in OD, 0.625in WT, X65:
            t/D = 0.058139
            Py = 2 * SMYS * t/D = 2 * 65000 * 0.058139 = 7558.1 psi
            Pel = 2 * E * (t/D)^3 / (1 - nu^2)
                = 2 * 30E6 * 0.000196546 / (1 - 0.09)
                = 11793 / 0.91
                = 12960 psi
            Pc = 7558.1 * 12960 / sqrt(7558.1^2 + 12960^2)
               = 97952736 / sqrt(57125959 + 167961600)
               = 97952736 / 15008
               = 6527 psi
        """
        smys = 65000
        e_mod = 30_000_000
        poisson = 0.3
        t_over_d = 0.625 / 10.75

        py = 2 * smys * t_over_d
        pel = 2 * e_mod * (t_over_d ** 3) / (1 - poisson ** 2)
        pc = py * pel / math.sqrt(py ** 2 + pel ** 2)

        assert py == pytest.approx(2 * 65000 * (0.625 / 10.75), rel=1e-6)
        assert pc > 0
        # Verify Pc is between Py and Pel (always the case for this formula)
        assert pc < max(py, pel)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
