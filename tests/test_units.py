"""Tests for the shared UnitRegistry module (GH #1484)."""

import pytest
import pint

from digitalmodel.units import ureg, Q_


class TestImports:
    """Verify the public API is importable and usable."""

    def test_ureg_is_unit_registry(self):
        assert isinstance(ureg, pint.UnitRegistry)

    def test_Q_is_quantity_class(self):
        q = Q_(1.0, "meter")
        assert isinstance(q, pint.Quantity)


class TestBasicConversions:
    """Sanity-check built-in unit conversions."""

    def test_meters_to_feet(self):
        length = Q_(1.0, "meter").to("foot")
        assert abs(length.magnitude - 3.28084) < 1e-3

    def test_kg_to_lb(self):
        mass = Q_(1.0, "kilogram").to("pound")
        assert abs(mass.magnitude - 2.20462) < 1e-3

    def test_incompatible_units_raises(self):
        with pytest.raises(pint.DimensionalityError):
            Q_(1.0, "meter").to("kilogram")


class TestCustomUnits:
    """Verify project-specific custom unit definitions."""

    def test_ksi_to_psi(self):
        stress = Q_(1.0, "ksi").to("psi")
        assert stress.magnitude == pytest.approx(1000.0)

    def test_pcf_dimensionality(self):
        density = Q_(1.0, "pcf")
        # Should be convertible to kg/m^3
        converted = density.to("kg/m**3")
        assert converted.magnitude > 0

    def test_ppg_dimensionality(self):
        mud_weight = Q_(1.0, "ppg")
        converted = mud_weight.to("kg/m**3")
        assert converted.magnitude > 0

    def test_bbl_to_gallon(self):
        volume = Q_(1.0, "bbl").to("gallon")
        assert volume.magnitude == pytest.approx(42.0)
