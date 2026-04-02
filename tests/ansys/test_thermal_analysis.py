# ABOUTME: Tests for ThermalAnalysisGenerator — thermal APDL command generation
# ABOUTME: Verifies convection, radiation, transient commands and coupling setup

"""Tests for thermal_analysis — ThermalAnalysisGenerator methods."""

import pytest

from digitalmodel.ansys.thermal_analysis import (
    ConvectionBC,
    RadiationBC,
    HeatFluxBC,
    ThermalAnalysisConfig,
    ThermalAnalysisGenerator,
    ThermalMaterialConfig,
    TransientConfig,
)


def _gen() -> ThermalAnalysisGenerator:
    return ThermalAnalysisGenerator()


# ---------------------------------------------------------------------------
# Thermal element types
# ---------------------------------------------------------------------------

class TestThermalElementType:
    def test_generates_et_solid70(self):
        config = ThermalAnalysisConfig(element_type="SOLID70")
        text = _gen().generate_thermal_element_type(config)
        assert "ET,1,SOLID70" in text

    def test_generates_et_plane55(self):
        config = ThermalAnalysisConfig(element_type="PLANE55")
        text = _gen().generate_thermal_element_type(config)
        assert "ET,1,PLANE55" in text

    def test_axisymmetric_keyopt_for_plane55(self):
        config = ThermalAnalysisConfig(element_type="PLANE55")
        text = _gen().generate_thermal_element_type(config)
        assert "KEYOPT,1,3,1" in text


# ---------------------------------------------------------------------------
# Thermal materials
# ---------------------------------------------------------------------------

class TestThermalMaterials:
    def test_generates_kxx(self):
        mat = ThermalMaterialConfig(mat_id=1, thermal_conductivity_w_mk=50.0)
        text = _gen().generate_thermal_materials([mat])
        assert "MP,KXX,1,50.0" in text

    def test_generates_specific_heat(self):
        mat = ThermalMaterialConfig(mat_id=1, specific_heat_j_kgk=480.0)
        text = _gen().generate_thermal_materials([mat])
        assert "MP,C,1,480.0" in text

    def test_generates_emissivity(self):
        mat = ThermalMaterialConfig(mat_id=1, emissivity=0.8)
        text = _gen().generate_thermal_materials([mat])
        assert "MP,EMIS,1,0.8" in text

    def test_multiple_materials(self):
        mats = [
            ThermalMaterialConfig(mat_id=1, name="Steel"),
            ThermalMaterialConfig(mat_id=2, name="Insulation"),
        ]
        text = _gen().generate_thermal_materials(mats)
        assert "MP,KXX,1," in text
        assert "MP,KXX,2," in text


# ---------------------------------------------------------------------------
# Convection BCs
# ---------------------------------------------------------------------------

class TestConvectionBCs:
    def test_generates_sfa_conv(self):
        bc = ConvectionBC(
            surface_id=1, surface_type="AREA",
            film_coefficient_w_m2k=10.0, bulk_temperature_c=25.0,
        )
        text = _gen().generate_convection_bcs([bc])
        assert "SFA,1,1,CONV,10.0,25.0" in text

    def test_generates_sf_conv_for_nodes(self):
        bc = ConvectionBC(
            surface_id=5, surface_type="NODE",
            film_coefficient_w_m2k=500.0, bulk_temperature_c=200.0,
        )
        text = _gen().generate_convection_bcs([bc])
        assert "SF,5,CONV,500.0,200.0" in text

    def test_generates_sfl_conv_for_lines(self):
        bc = ConvectionBC(
            surface_id=3, surface_type="LINE",
            film_coefficient_w_m2k=50.0, bulk_temperature_c=100.0,
        )
        text = _gen().generate_convection_bcs([bc])
        assert "SFL,3,CONV,50.0,100.0" in text


# ---------------------------------------------------------------------------
# Radiation BCs
# ---------------------------------------------------------------------------

class TestRadiationBCs:
    def test_generates_rdsf_to_ambient(self):
        bc = RadiationBC(
            surface_id=1, emissivity=0.8, ambient_temperature_c=25.0,
            enclosure_id=0,
        )
        text = _gen().generate_radiation_bcs([bc])
        assert "RDSF" in text

    def test_generates_stefan_boltzmann(self):
        bc = RadiationBC()
        text = _gen().generate_radiation_bcs([bc])
        assert "STEF," in text

    def test_generates_toffst(self):
        bc = RadiationBC()
        text = _gen().generate_radiation_bcs([bc])
        assert "TOFFST,273.15" in text


# ---------------------------------------------------------------------------
# Heat flux BCs
# ---------------------------------------------------------------------------

class TestHeatFluxBCs:
    def test_generates_hflux(self):
        bc = HeatFluxBC(surface_id=1, heat_flux_w_m2=5000.0, bc_type="flux")
        text = _gen().generate_heat_flux_bcs([bc])
        assert "HFLUX,5000.0" in text

    def test_generates_heat_generation(self):
        bc = HeatFluxBC(heat_generation_w_m3=1e6, bc_type="generation")
        text = _gen().generate_heat_flux_bcs([bc])
        assert "HGEN" in text

    def test_generates_fixed_temperature(self):
        bc = HeatFluxBC(
            surface_id=2, surface_type="AREA",
            heat_flux_w_m2=200.0, bc_type="temperature",
        )
        text = _gen().generate_heat_flux_bcs([bc])
        assert "DA,2,TEMP,200.0" in text


# ---------------------------------------------------------------------------
# Steady-state solution
# ---------------------------------------------------------------------------

class TestSteadyStateSolution:
    def test_generates_antype_0(self):
        text = _gen().generate_steady_state_solution()
        assert "ANTYPE,0" in text

    def test_generates_solve(self):
        text = _gen().generate_steady_state_solution()
        assert "SOLVE" in text

    def test_generates_cnvtol(self):
        text = _gen().generate_steady_state_solution()
        assert "CNVTOL,HEAT" in text


# ---------------------------------------------------------------------------
# Transient solution
# ---------------------------------------------------------------------------

class TestTransientSolution:
    def test_generates_antype_4(self):
        config = TransientConfig()
        text = _gen().generate_transient_solution(config)
        assert "ANTYPE,4" in text

    def test_generates_deltim(self):
        config = TransientConfig(time_step_s=60.0)
        text = _gen().generate_transient_solution(config)
        assert "DELTIM,60.0" in text

    def test_generates_time(self):
        config = TransientConfig(total_time_s=3600.0)
        text = _gen().generate_transient_solution(config)
        assert "TIME,3600.0" in text

    def test_initial_temperature(self):
        config = TransientConfig(initial_temperature_c=25.0)
        text = _gen().generate_transient_solution(config)
        assert "TUNIF,25.0" in text


# ---------------------------------------------------------------------------
# Thermal results
# ---------------------------------------------------------------------------

class TestThermalResults:
    def test_generates_prnsol_temp(self):
        text = _gen().generate_thermal_results_extraction()
        assert "PRNSOL,TEMP" in text

    def test_generates_tg_output(self):
        text = _gen().generate_thermal_results_extraction()
        assert "PRNSOL,TG" in text

    def test_generates_get_max_min(self):
        text = _gen().generate_thermal_results_extraction()
        assert "*GET,T_MAX" in text
        assert "*GET,T_MIN" in text


# ---------------------------------------------------------------------------
# Thermal-structural coupling
# ---------------------------------------------------------------------------

class TestThermalStructuralCoupling:
    def test_generates_etchg_tts(self):
        text = _gen().generate_thermal_structural_coupling()
        assert "ETCHG,TTS" in text

    def test_generates_ldread(self):
        text = _gen().generate_thermal_structural_coupling("my_thermal.rth")
        assert "LDREAD,TEMP" in text
        assert "my_thermal.rth" in text

    def test_generates_tref(self):
        text = _gen().generate_thermal_structural_coupling()
        assert "TREF," in text


# ---------------------------------------------------------------------------
# Full thermal script
# ---------------------------------------------------------------------------

class TestFullThermalScript:
    def test_includes_batch_command(self):
        config = ThermalAnalysisConfig()
        text = _gen().generate_full_thermal_script(config)
        assert "/BATCH" in text

    def test_includes_solve(self):
        config = ThermalAnalysisConfig()
        text = _gen().generate_full_thermal_script(config)
        assert "SOLVE" in text

    def test_transient_uses_antype_4(self):
        config = ThermalAnalysisConfig(analysis_type="transient")
        text = _gen().generate_full_thermal_script(config)
        assert "ANTYPE,4" in text
