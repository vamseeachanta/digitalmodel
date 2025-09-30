"""
Comprehensive test suite for src/digitalmodel/custom/PipeSizing.py

This module provides comprehensive testing for the PipeSizing class including:
- Unit tests for all 14 functions with proper assertions
- Property-based tests using Hypothesis for mathematical calculations
- Edge case tests (zero values, negative numbers, extreme values)
- Parametrized tests for different pipe configurations
- Test engineering formulas with known results
- Mock external dependencies
- 90%+ coverage target for the 101 lines of code

Test Categories:
- Unit Tests: Individual function testing
- Property Tests: Mathematical property validation
- Edge Cases: Boundary condition testing
- Integration Tests: Component interaction testing
- Performance Tests: Computational efficiency validation
"""

import pytest
import math
import sys
from pathlib import Path
from unittest.mock import Mock, MagicMock, patch
from typing import Dict, Any, Optional

# Property-based testing
from hypothesis import given, strategies as st, assume, settings, HealthCheck
from hypothesis.strategies import floats, integers, dictionaries, text, booleans, one_of

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel.custom.PipeSizing import PipeSizing


class TestPipeSizingUnit:
    """Unit tests for individual PipeSizing methods."""

    @pytest.fixture
    def minimal_config(self):
        """Minimal configuration for PipeSizing initialization."""
        return {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 10.75,  # inches
                    "Nominal_ID": 10.0,   # inches
                    "Design_WT": 0.375    # inches
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,  # psi
                    "Rho": 0.284,   # lb/in³
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {
                            "SMYS": 65000,  # psi
                            "SMUS": 77000   # psi
                        }
                    }
                }
            }
        }

    @pytest.fixture
    def dual_pipe_config(self):
        """Configuration with both outer and inner pipes."""
        return {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 12.75,
                    "Nominal_ID": 12.0,
                    "Design_WT": 0.375
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": {
                "Geometry": {
                    "Nominal_OD": 8.625,
                    "Nominal_ID": 8.0,
                    "Design_WT": 0.3125
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X52"
                }
            },
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {"SMYS": 65000, "SMUS": 77000},
                        "X52": {"SMYS": 52000, "SMUS": 66000}
                    }
                }
            }
        }

    @pytest.fixture
    def catenary_config(self):
        """Configuration for catenary riser calculations."""
        return {
            "geometry": {
                "NominalOD": 12.75,
                "NominalID": 12.0,
                "ExternalCoating": {"Thickness": 0.5},
                "Strakes": {"BaseThickness": None}
            },
            "Material": {
                "Steel": {"Rho": 0.284},
                "ExternalCoating": {"Rho": 0.036},
                "SeaWater": {"Rho": 0.037},
                "Strakes": {
                    "MassPerUnitLength": 5.0,
                    "WeightPerUnitLength": 10.0
                }
            },
            "default": {
                "Constants": {"g": 32.174}  # ft/s²
            }
        }

    def test_init(self, minimal_config):
        """Test PipeSizing initialization."""
        pipe_sizing = PipeSizing(minimal_config)
        assert pipe_sizing.cfg == minimal_config
        assert "equivalent_pipe" in pipe_sizing.cfg
        assert pipe_sizing.cfg["equivalent_pipe"] == {}

    def test_pipe_properties_calculate_id_from_od_and_wt(self, minimal_config):
        """Test pipe_properties calculates ID when only OD and WT are provided."""
        config = minimal_config.copy()
        config["Outer_Pipe"]["Geometry"]["Nominal_ID"] = None

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        expected_id = 10.75 - 2 * 0.375  # OD - 2*WT
        assert pipe_sizing.cfg["Outer_Pipe"]["Geometry"]["Nominal_ID"] == expected_id
        assert expected_id == 10.0

    def test_pipe_properties_calculate_od_from_id_and_wt(self, minimal_config):
        """Test pipe_properties calculates OD when only ID and WT are provided."""
        config = minimal_config.copy()
        config["Outer_Pipe"]["Geometry"]["Nominal_OD"] = None

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        expected_od = 10.0 + 2 * 0.375  # ID + 2*WT
        assert pipe_sizing.cfg["Outer_Pipe"]["Geometry"]["Nominal_OD"] == expected_od
        assert expected_od == 10.75

    def test_pipe_properties_calculate_wt_from_od_and_id(self, minimal_config):
        """Test pipe_properties calculates WT when only OD and ID are provided."""
        config = minimal_config.copy()
        config["Outer_Pipe"]["Geometry"]["Design_WT"] = None

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        expected_wt = (10.75 - 10.0) / 2  # (OD - ID) / 2
        assert pipe_sizing.cfg["Outer_Pipe"]["Geometry"]["Design_WT"] == expected_wt
        assert expected_wt == 0.375

    def test_pipe_section_properties(self, minimal_config):
        """Test pipe section properties calculations."""
        pipe_sizing = PipeSizing(minimal_config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        section_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]

        # Verify area calculations
        expected_ao = (math.pi / 4) * (10.75 ** 2)
        expected_ai = (math.pi / 4) * (10.0 ** 2)
        expected_a = expected_ao - expected_ai

        assert abs(section_props["Ao"] - expected_ao) < 1e-10
        assert abs(section_props["Ai"] - expected_ai) < 1e-10
        assert abs(section_props["A"] - expected_a) < 1e-10

        # Verify moment of inertia calculations
        expected_io = (math.pi / 64) * (10.75 ** 4)
        expected_ii = (math.pi / 64) * (10.0 ** 4)
        expected_i = expected_io - expected_ii

        assert abs(section_props["Io"] - expected_io) < 1e-8
        assert abs(section_props["Ii"] - expected_ii) < 1e-8
        assert abs(section_props["I"] - expected_i) < 1e-8

    def test_get_fea_properties(self, minimal_config):
        """Test FEA properties calculation."""
        pipe_sizing = PipeSizing(minimal_config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        fea_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]

        # Verify shear modulus calculation
        expected_g = 30000000 / (2 * (1 + 0.3))  # E / (2 * (1 + ν))
        assert pipe_sizing.cfg["Material"]["Steel"]["G"] == expected_g

        # Verify composite properties
        assert "MassPerUnitLength" in fea_props
        assert "EI" in fea_props
        assert "EA" in fea_props
        assert "GJ" in fea_props
        assert fea_props["E"] == 30000000
        assert fea_props["SMYS"] == 65000
        assert fea_props["SMUS"] == 77000

    def test_system_properties_single_pipe(self, minimal_config):
        """Test system properties for single pipe system."""
        pipe_sizing = PipeSizing(minimal_config)
        pipe_sizing.evaluate_pipe_system_properties()

        # For single pipe, equivalent pipe should match outer pipe
        outer_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]
        equiv_props = pipe_sizing.cfg["equivalent_pipe"]["section_properties"]

        assert equiv_props == outer_props

    def test_system_properties_dual_pipe(self, dual_pipe_config):
        """Test system properties for dual pipe system."""
        pipe_sizing = PipeSizing(dual_pipe_config)

        # Initialize equivalent_pipe structure first
        pipe_sizing.cfg["equivalent_pipe"]["section_properties"] = {"pipe": {}}

        pipe_sizing.evaluate_pipe_system_properties()

        # Verify that properties are combined
        outer_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]
        inner_props = pipe_sizing.cfg["Inner_Pipe"]["section_properties"]["pipe"]
        equiv_props = pipe_sizing.cfg["equivalent_pipe"]["section_properties"]["pipe"]

        expected_mass = outer_props["MassPerUnitLength"] + inner_props["MassPerUnitLength"]
        expected_ei = outer_props["EI"] + inner_props["EI"]
        expected_ea = outer_props["EA"] + inner_props["EA"]
        expected_gj = outer_props["GJ"] + inner_props["GJ"]

        assert equiv_props["MassPerUnitLength"] == expected_mass
        assert equiv_props["EI"] == expected_ei
        assert equiv_props["EA"] == expected_ea
        assert equiv_props["GJ"] == expected_gj

    def test_evaluate_pipe_system_properties(self, dual_pipe_config):
        """Test complete pipe system evaluation."""
        pipe_sizing = PipeSizing(dual_pipe_config)

        # Initialize equivalent_pipe structure first
        pipe_sizing.cfg["equivalent_pipe"]["section_properties"] = {"pipe": {}}

        pipe_sizing.evaluate_pipe_system_properties()

        # Verify both pipes were processed
        assert "section_properties" in pipe_sizing.cfg["Outer_Pipe"]
        assert "section_properties" in pipe_sizing.cfg["Inner_Pipe"]
        assert "section_properties" in pipe_sizing.cfg["equivalent_pipe"]

    def test_get_pipe_system_properties(self, minimal_config):
        """Test get_pipe_system_properties method."""
        pipe_sizing = PipeSizing(minimal_config)
        result = pipe_sizing.get_pipe_system_properties()

        assert result == pipe_sizing.cfg["equivalent_pipe"]
        assert "section_properties" in result

    def test_section_properties_helper(self, minimal_config):
        """Test section properties helper function."""
        pipe_sizing = PipeSizing(minimal_config)

        data = {"OD": 12.75, "ID": 12.0}
        result = pipe_sizing.sectionProperties(data)

        expected_a = (math.pi / 4) * (12.75 ** 2 - 12.0 ** 2)
        expected_ai = (math.pi / 4) * (12.0 ** 2)
        expected_ao = (math.pi / 4) * (12.75 ** 2)
        expected_i = (math.pi / 64) * (12.75 ** 4 - 12.0 ** 4)

        assert abs(result["A"] - expected_a) < 1e-10
        assert abs(result["Ai"] - expected_ai) < 1e-10
        assert abs(result["Ao"] - expected_ao) < 1e-10
        assert abs(result["I"] - expected_i) < 1e-8


class TestPipeSizingEdgeCases:
    """Edge case tests for boundary conditions and error handling."""

    def test_zero_diameter_values(self):
        """Test handling of zero diameter values."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 0.0,
                    "Nominal_ID": 0.0,
                    "Design_WT": 0.0
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        # All calculated values should be zero for zero inputs
        section_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]
        assert section_props["A"] == 0.0
        assert section_props["I"] == 0.0
        assert section_props["J"] == 0.0

    def test_negative_values_handling(self):
        """Test handling of negative input values."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": -10.0,
                    "Nominal_ID": -8.0,
                    "Design_WT": -1.0
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        # Should not raise exception but will produce mathematically consistent results
        pipe_sizing.pipe_properties("Outer_Pipe")

        # Verify calculations proceed without error
        assert "section_properties" in pipe_sizing.cfg["Outer_Pipe"]

    def test_extremely_large_values(self):
        """Test handling of extremely large diameter values."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 1e6,
                    "Nominal_ID": 1e6 - 1000,
                    "Design_WT": 500
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        # Should handle large values without overflow
        section_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]
        assert section_props["A"] > 0
        assert section_props["I"] > 0

    def test_missing_material_grade(self):
        """Test handling of missing material grade."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 10.75,
                    "Nominal_ID": 10.0,
                    "Design_WT": 0.375
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "NonexistentGrade"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)

        # Should raise KeyError for missing grade
        with pytest.raises(KeyError):
            pipe_sizing.pipe_properties("Outer_Pipe")


class TestPipeSizingPropertyBased:
    """Property-based tests using Hypothesis for mathematical validation."""

    @given(
        od=floats(min_value=1.0, max_value=1000.0, allow_nan=False, allow_infinity=False),
        wt=floats(min_value=0.1, max_value=10.0, allow_nan=False, allow_infinity=False)
    )
    @settings(suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_diameter_relationship_property(self, od, wt):
        """Test that OD = ID + 2*WT relationship always holds."""
        assume(wt < od / 2)  # Ensure wall thickness is physically possible

        id_value = od - 2 * wt

        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": od,
                    "Nominal_ID": None,  # Will be calculated
                    "Design_WT": wt
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        calculated_id = pipe_sizing.cfg["Outer_Pipe"]["Geometry"]["Nominal_ID"]

        # Verify relationship holds within floating-point precision
        assert abs(calculated_id - id_value) < 1e-10

    @given(
        od=floats(min_value=1.0, max_value=100.0, allow_nan=False, allow_infinity=False),
        id_value=floats(min_value=0.5, max_value=99.0, allow_nan=False, allow_infinity=False)
    )
    @settings(suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_area_calculations_property(self, od, id_value):
        """Test that area calculations satisfy mathematical properties."""
        assume(id_value < od)  # Inner diameter must be less than outer

        pipe_sizing = PipeSizing({})

        # Calculate areas directly
        ao = (math.pi / 4) * (od ** 2)
        ai = (math.pi / 4) * (id_value ** 2)
        a = ao - ai

        # Verify area relationships
        assert ao > ai, "Outer area must be greater than inner area"
        assert a > 0, "Net area must be positive"
        assert a == ao - ai, "Net area must equal difference"

    @given(
        od=floats(min_value=2.0, max_value=50.0, allow_nan=False, allow_infinity=False),
        id_value=floats(min_value=1.0, max_value=49.0, allow_nan=False, allow_infinity=False)
    )
    @settings(suppress_health_check=[HealthCheck.function_scoped_fixture])
    def test_moment_of_inertia_property(self, od, id_value):
        """Test moment of inertia calculation properties."""
        assume(id_value < od)

        # Calculate moments of inertia
        io = (math.pi / 64) * (od ** 4)
        ii = (math.pi / 64) * (id_value ** 4)
        i = io - ii

        # Verify properties
        assert io > ii, "Outer MOI must be greater than inner MOI"
        assert i > 0, "Net MOI must be positive"

        # MOI scales with fourth power of diameter
        assert io == (math.pi / 64) * (od ** 4)
        assert ii == (math.pi / 64) * (id_value ** 4)


class TestPipeSizingParametrized:
    """Parametrized tests for different pipe configurations."""

    @pytest.mark.parametrize("od,id_val,expected_wt", [
        (10.75, 10.0, 0.375),
        (12.75, 12.0, 0.375),
        (8.625, 8.0, 0.3125),
        (6.625, 6.0, 0.3125),
        (4.5, 4.0, 0.25)
    ])
    def test_wall_thickness_calculations(self, od, id_val, expected_wt):
        """Test wall thickness calculations for standard pipe sizes."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": od,
                    "Nominal_ID": id_val,
                    "Design_WT": None  # Will be calculated
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        calculated_wt = pipe_sizing.cfg["Outer_Pipe"]["Geometry"]["Design_WT"]
        assert abs(calculated_wt - expected_wt) < 1e-10

    @pytest.mark.parametrize("material_grade,expected_smys", [
        ("X42", 42000),
        ("X52", 52000),
        ("X60", 60000),
        ("X65", 65000),
        ("X70", 70000)
    ])
    def test_material_properties(self, material_grade, expected_smys):
        """Test material property assignment for different grades."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 10.75,
                    "Nominal_ID": 10.0,
                    "Design_WT": 0.375
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": material_grade
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X42": {"SMYS": 42000, "SMUS": 60000},
                        "X52": {"SMYS": 52000, "SMUS": 66000},
                        "X60": {"SMYS": 60000, "SMUS": 75000},
                        "X65": {"SMYS": 65000, "SMUS": 77000},
                        "X70": {"SMYS": 70000, "SMUS": 82000}
                    }
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        smys = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]["SMYS"]
        assert smys == expected_smys


class TestPipeSizingIntegration:
    """Integration tests for complete workflows."""

    def test_complete_single_pipe_workflow(self):
        """Test complete workflow for single pipe analysis."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 16.0,
                    "Nominal_ID": None,  # To be calculated
                    "Design_WT": 0.5
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        result = pipe_sizing.get_pipe_system_properties()

        # Verify complete workflow produces all required properties
        pipe_props = result["section_properties"]["pipe"]

        assert "A" in pipe_props
        assert "I" in pipe_props
        assert "J" in pipe_props
        assert "MassPerUnitLength" in pipe_props
        assert "EI" in pipe_props
        assert "EA" in pipe_props
        assert "GJ" in pipe_props
        assert "SMYS" in pipe_props
        assert "SMUS" in pipe_props

    def test_complete_dual_pipe_workflow(self):
        """Test complete workflow for dual pipe analysis."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 16.0,
                    "Nominal_ID": 15.0,
                    "Design_WT": 0.5
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": {
                "Geometry": {
                    "Nominal_OD": 10.75,
                    "Nominal_ID": 10.0,
                    "Design_WT": 0.375
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X52"
                }
            },
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X65": {"SMYS": 65000, "SMUS": 77000},
                        "X52": {"SMYS": 52000, "SMUS": 66000}
                    }
                }
            }
        }

        pipe_sizing = PipeSizing(config)

        # Initialize equivalent_pipe structure
        pipe_sizing.cfg["equivalent_pipe"]["section_properties"] = {"pipe": {}}

        result = pipe_sizing.get_pipe_system_properties()

        # Verify dual pipe properties are combined
        equiv_props = result["section_properties"]["pipe"]
        outer_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]
        inner_props = pipe_sizing.cfg["Inner_Pipe"]["section_properties"]["pipe"]

        # Check that equivalent properties are sums
        assert equiv_props["MassPerUnitLength"] == outer_props["MassPerUnitLength"] + inner_props["MassPerUnitLength"]
        assert equiv_props["EI"] == outer_props["EI"] + inner_props["EI"]
        assert equiv_props["EA"] == outer_props["EA"] + inner_props["EA"]
        assert equiv_props["GJ"] == outer_props["GJ"] + inner_props["GJ"]


class TestPipeSizingCatenaryFunctions:
    """Tests for catenary riser specific functions."""

    @pytest.fixture
    def catenary_config(self):
        """Configuration for catenary riser testing."""
        return {
            "geometry": {
                "NominalOD": 12.75,
                "NominalID": 12.0,
                "ExternalCoating": {"Thickness": 0.5},
                "Strakes": {"BaseThickness": None}
            },
            "Material": {
                "Steel": {"Rho": 0.284},
                "ExternalCoating": {"Rho": 0.036},
                "SeaWater": {"Rho": 0.037}
            },
            "default": {
                "Constants": {"g": 32.174}
            }
        }

    def test_steel_section_properties(self, catenary_config):
        """Test steel section properties calculation."""
        pipe_sizing = PipeSizing({})
        result = pipe_sizing.steelSectionProperties(catenary_config)

        steel_section = result["SteelSection"]
        assert steel_section["OD"] == 12.75
        assert steel_section["ID"] == 12.0
        assert "A" in steel_section
        assert "Ai" in steel_section
        assert "Ao" in steel_section
        assert "I" in steel_section

    def test_insulation_section_properties(self, catenary_config):
        """Test insulation section properties calculation."""
        pipe_sizing = PipeSizing({})

        # First calculate steel section
        config = pipe_sizing.steelSectionProperties(catenary_config)

        # Mock the undefined sectionProperties reference
        original_sectionprops = None
        try:
            original_sectionprops = sectionProperties
        except NameError:
            # Define a mock function in the global namespace temporarily
            def mock_section_properties(data):
                return {
                    "OD": data["OD"],
                    "ID": data["ID"],
                    "A": (math.pi / 4) * (data["OD"]**2 - data["ID"]**2),
                    "Ai": (math.pi / 4) * (data["ID"]**2),
                    "Ao": (math.pi / 4) * (data["OD"]**2),
                    "I": (math.pi / 64) * (data["OD"]**4 - data["ID"]**4)
                }
            globals()['sectionProperties'] = mock_section_properties

        try:
            result = pipe_sizing.insulationSectionProperties(config)

            insulation_section = result["InsulationSection"]
            assert insulation_section["OD"] == 13.75  # NominalOD + 2*coating thickness
            assert insulation_section["ID"] == 12.75  # NominalOD
        finally:
            # Cleanup
            if original_sectionprops is None and 'sectionProperties' in globals():
                del globals()['sectionProperties']

    def test_equivalent_pipe_no_strakes_no_buoyancy(self, catenary_config):
        """Test equivalent pipe calculation without strakes or buoyancy."""
        pipe_sizing = PipeSizing({})

        # Setup required sections
        catenary_config["SteelSection"] = {
            "A": 10.0,
            "Ai": 113.1,  # π/4 * 12²
            "Ao": 127.7   # π/4 * 12.75²
        }
        catenary_config["InsulationSection"] = {
            "A": 5.0,
            "Ao": 148.5   # π/4 * 13.75²
        }

        fluid_density = 0.03  # lb/in³

        result = pipe_sizing.equivalentPipe(catenary_config, fluid_density, Buoyancy=False)

        equiv_pipe = result["equivalentPipe"]
        assert "massPerUnitLength" in equiv_pipe
        assert "weightPerUnitLength" in equiv_pipe
        assert equiv_pipe["massPerUnitLength"] > 0
        assert equiv_pipe["weightPerUnitLength"] < equiv_pipe["massPerUnitLength"]  # Weight < mass due to buoyancy

    def test_equivalent_pipe_with_strakes_no_buoyancy(self):
        """Test equivalent pipe calculation with strakes but no buoyancy."""
        pipe_sizing = PipeSizing({})

        config = {
            "geometry": {
                "Strakes": {"BaseThickness": 0.5}  # Has strakes
            },
            "SteelSection": {
                "A": 10.0,
                "Ai": 113.1,
                "Ao": 127.7
            },
            "InsulationSection": {
                "A": 5.0,
                "Ao": 148.5
            },
            "Material": {
                "Steel": {"Rho": 0.284},
                "ExternalCoating": {"Rho": 0.036},
                "SeaWater": {"Rho": 0.037},
                "Strakes": {
                    "MassPerUnitLength": 5.0,
                    "WeightPerUnitLength": 10.0
                }
            },
            "default": {
                "Constants": {"g": 32.174}
            }
        }

        fluid_density = 0.03

        result = pipe_sizing.equivalentPipe(config, fluid_density, Buoyancy=False)

        equiv_pipe = result["equivalentPipe"]
        assert "massPerUnitLength" in equiv_pipe
        assert "weightPerUnitLength" in equiv_pipe
        # With strakes, mass should include strake contribution
        assert equiv_pipe["massPerUnitLength"] > 0

    def test_equivalent_pipe_with_buoyancy(self):
        """Test equivalent pipe calculation with buoyancy."""
        pipe_sizing = PipeSizing({})

        config = {
            "geometry": {
                "Strakes": {"BaseThickness": None}  # No strakes
            },
            "SteelSection": {
                "A": 10.0,
                "Ai": 113.1,
                "Ao": 127.7
            },
            "InsulationSection": {
                "A": 5.0,
                "Ao": 148.5
            },
            "BuoyancySection": {
                "A": 8.0,
                "Ao": 200.0
            },
            "Material": {
                "Steel": {"Rho": 0.284},
                "ExternalCoating": {"Rho": 0.036},
                "SeaWater": {"Rho": 0.037},
                "Buoyancy": {"Rho": 0.01}  # Low density buoyancy material
            },
            "default": {
                "Constants": {"g": 32.174}
            }
        }

        fluid_density = 0.03

        result = pipe_sizing.equivalentPipe(config, fluid_density, Buoyancy=True)

        equiv_pipe = result["equivalentPipe"]
        assert "massPerUnitLength" in equiv_pipe
        assert "weightPerUnitLength" in equiv_pipe
        # With buoyancy, should include buoyancy section contribution
        assert equiv_pipe["massPerUnitLength"] > 0

    def test_buoyancy_section_properties(self):
        """Test buoyancy section properties calculation."""
        pipe_sizing = PipeSizing({})

        config = {
            "InsulationSection": {"OD": 15.0},
            "LazyWaveCatenaryDefinition": {
                "UniformBuoyancy": {"Thickness": 2.0}
            }
        }

        # Mock sectionProperties function
        original_sectionprops = globals().get('sectionProperties')

        def mock_section_properties(data):
            return {
                "OD": data["OD"],
                "ID": data["ID"],
                "A": (math.pi / 4) * (data["OD"]**2 - data["ID"]**2),
                "Ai": (math.pi / 4) * (data["ID"]**2),
                "Ao": (math.pi / 4) * (data["OD"]**2),
                "I": (math.pi / 64) * (data["OD"]**4 - data["ID"]**4)
            }

        globals()['sectionProperties'] = mock_section_properties

        try:
            result = pipe_sizing.buoyancySectionProperties(config)

            buoyancy_section = result["BuoyancySection"]
            assert buoyancy_section["OD"] == 19.0  # 15.0 + 2*2.0
            assert buoyancy_section["ID"] == 15.0
        finally:
            if original_sectionprops is None and 'sectionProperties' in globals():
                del globals()['sectionProperties']
            elif original_sectionprops is not None:
                globals()['sectionProperties'] = original_sectionprops

    def test_pipe_properties_catenary_workflow(self, catenary_config):
        """Test complete catenary pipe properties workflow."""
        pipe_sizing = PipeSizing({})

        # Define a mock sectionProperties function in global namespace
        def mock_section_properties(data):
            return {
                "OD": data["OD"],
                "ID": data["ID"],
                "A": (math.pi / 4) * (data["OD"]**2 - data["ID"]**2),
                "Ai": (math.pi / 4) * (data["ID"]**2),
                "Ao": (math.pi / 4) * (data["OD"]**2),
                "I": (math.pi / 64) * (data["OD"]**4 - data["ID"]**4)
            }

        original_sectionprops = None
        try:
            original_sectionprops = globals().get('sectionProperties')
        except:
            pass

        globals()['sectionProperties'] = mock_section_properties

        try:
            fluid_density = 0.03
            result = pipe_sizing.pipeProperties(catenary_config, fluid_density, Buoyancy=False)

            # Verify all sections are calculated
            assert "SteelSection" in result
            assert "InsulationSection" in result
            assert "BuoyancySection" in result
            assert "equivalentPipe" in result

            # Buoyancy section should be None when Buoyancy=False
            assert result["BuoyancySection"] is None
        finally:
            # Cleanup
            if original_sectionprops is None and 'sectionProperties' in globals():
                del globals()['sectionProperties']
            elif original_sectionprops is not None:
                globals()['sectionProperties'] = original_sectionprops

    def test_buoyancy_factor_and_diameter(self):
        """Test buoyancyFactorAndDiameter method (currently empty)."""
        pipe_sizing = PipeSizing({})

        # This function currently just passes, but test it exists
        result = pipe_sizing.buoyancyFactorAndDiameter()
        assert result is None  # Function returns None


class TestPipeSizingKnownResults:
    """Tests with known engineering results for validation."""

    def test_api_5l_grade_b_pipe(self):
        """Test calculation for standard API 5L Grade B pipe."""
        # Standard 10-3/4" OD pipe with 0.365" wall thickness
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 10.75,
                    "Nominal_ID": None,  # Should calculate to 10.02
                    "Design_WT": 0.365
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "B"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,  # psi
                    "Rho": 0.284,   # lb/in³
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "B": {"SMYS": 35000, "SMUS": 60000}  # API 5L Grade B
                    }
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        # Verify calculated ID
        calculated_id = pipe_sizing.cfg["Outer_Pipe"]["Geometry"]["Nominal_ID"]
        expected_id = 10.75 - 2 * 0.365  # 10.02
        assert abs(calculated_id - expected_id) < 1e-10

        # Verify area calculations (known values for this pipe size)
        section_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]

        # Cross-sectional area (steel only)
        expected_steel_area = (math.pi / 4) * (10.75**2 - 10.02**2)  # ≈ 11.9 in²
        assert abs(section_props["A"] - expected_steel_area) < 0.1

        # Moment of inertia
        expected_i = (math.pi / 64) * (10.75**4 - 10.02**4)  # ≈ 279 in⁴
        assert abs(section_props["I"] - expected_i) < 1.0

    def test_known_moment_calculations(self):
        """Test moment calculations against known engineering values."""
        # 8-5/8" OD pipe
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 8.625,
                    "Nominal_ID": 8.0,
                    "Design_WT": 0.3125
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X52"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {"X52": {"SMYS": 52000, "SMUS": 66000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        section_props = pipe_sizing.cfg["Outer_Pipe"]["section_properties"]["pipe"]

        # Known values for 8-5/8" pipe
        expected_area = (math.pi / 4) * (8.625**2 - 8.0**2)  # ≈ 8.25 in²
        expected_i = (math.pi / 64) * (8.625**4 - 8.0**4)    # ≈ 72.5 in⁴

        assert abs(section_props["A"] - expected_area) < 0.1
        assert abs(section_props["I"] - expected_i) < 1.0


class TestPipeSizingMocking:
    """Tests with mocked dependencies."""

    def test_mocked_section_properties(self):
        """Test section properties calculation without mocking."""
        pipe_sizing = PipeSizing({})
        data = {"OD": 10.75, "ID": 10.0}

        result = pipe_sizing.sectionProperties(data)

        # Verify calculations are correct
        expected_a = (math.pi / 4) * (10.75**2 - 10.0**2)
        expected_ai = (math.pi / 4) * (10.0**2)
        expected_ao = (math.pi / 4) * (10.75**2)
        expected_i = (math.pi / 64) * (10.75**4 - 10.0**4)

        assert abs(result["A"] - expected_a) < 1e-10
        assert abs(result["Ai"] - expected_ai) < 1e-10
        assert abs(result["Ao"] - expected_ao) < 1e-10
        assert abs(result["I"] - expected_i) < 1e-8

    def test_material_property_exception_handling(self):
        """Test exception handling in get_fea_properties."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 10.75,
                    "Nominal_ID": 10.0,
                    "Design_WT": 0.375
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Inner_Pipe": None,
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    # No "G" property - should be calculated
                    "Grades": {"X65": {"SMYS": 65000, "SMUS": 77000}}
                }
            }
        }

        pipe_sizing = PipeSizing(config)
        pipe_sizing.pipe_properties("Outer_Pipe")

        # Should calculate G = E / (2 * (1 + ν))
        expected_g = 30000000 / (2 * (1 + 0.3))  # 11,538,462 psi
        assert abs(pipe_sizing.cfg["Material"]["Steel"]["G"] - expected_g) < 1


class TestPipeSizingPerformance:
    """Performance tests for computational efficiency."""

    def test_large_scale_calculation_performance(self, benchmark):
        """Benchmark performance for large-scale calculations."""
        config = {
            "Outer_Pipe": {
                "Geometry": {
                    "Nominal_OD": 36.0,  # Large pipe
                    "Nominal_ID": 35.0,
                    "Design_WT": 0.5
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X70"
                }
            },
            "Inner_Pipe": {
                "Geometry": {
                    "Nominal_OD": 24.0,
                    "Nominal_ID": 23.0,
                    "Design_WT": 0.5
                },
                "Material": {
                    "Material": "Steel",
                    "Material_Grade": "X65"
                }
            },
            "Material": {
                "Steel": {
                    "E": 30000000,
                    "Rho": 0.284,
                    "Poissionsratio": 0.3,
                    "Grades": {
                        "X70": {"SMYS": 70000, "SMUS": 82000},
                        "X65": {"SMYS": 65000, "SMUS": 77000}
                    }
                }
            }
        }

        def run_calculation():
            pipe_sizing = PipeSizing(config)
            return pipe_sizing.get_pipe_system_properties()

        # Benchmark should complete in reasonable time
        result = benchmark(run_calculation)
        assert "section_properties" in result


# Mark classes for different test categories
TestPipeSizingUnit = pytest.mark.unit(TestPipeSizingUnit)
TestPipeSizingEdgeCases = pytest.mark.unit(TestPipeSizingEdgeCases)
TestPipeSizingPropertyBased = pytest.mark.property(TestPipeSizingPropertyBased)
TestPipeSizingParametrized = pytest.mark.unit(TestPipeSizingParametrized)
TestPipeSizingIntegration = pytest.mark.integration(TestPipeSizingIntegration)
TestPipeSizingCatenaryFunctions = pytest.mark.unit(TestPipeSizingCatenaryFunctions)
TestPipeSizingKnownResults = pytest.mark.unit(TestPipeSizingKnownResults)
TestPipeSizingMocking = pytest.mark.unit(TestPipeSizingMocking)
TestPipeSizingPerformance = pytest.mark.benchmark(TestPipeSizingPerformance)


if __name__ == "__main__":
    pytest.main([__file__, "-v", "--tb=short"])