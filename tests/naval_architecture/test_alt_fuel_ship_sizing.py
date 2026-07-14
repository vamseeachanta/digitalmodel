# ABOUTME: Library tests for the alternative-fuel (LH2) ship-design sizing
# ABOUTME: toolkit with hand-verified expected values in the docstrings.
"""Hand-verified tests for ``digitalmodel.alt_fuel_ship_sizing``.

Synthetic ~40k DWT bulker-like concept, round numbers throughout (no client
or vessel data): 4000 nm route at 12 kn, 4750 kW shaft power at service
speed (an *input* from the resistance lane), 500 kW hotel load, fuel-cell
plant at 50 % LHV efficiency, electric drivetrain at 95 %, LHV overridden
to a round 120.0 MJ/kg (published value 119.96).
"""

import math

import pytest

from digitalmodel.alt_fuel_ship_sizing import (
    FUEL_PRESETS,
    FuelProperties,
    PortLeg,
    SeaLeg,
    TankParameters,
    ThrustMatrix,
    WindRoseBin,
    boil_off,
    cylindrical_tank_geometry,
    electrical_power_kw,
    endurance_hours,
    fuel_mass_flow_kg_per_h,
    fuel_preset,
    gross_tonnage,
    interpolate_thrust,
    run_trade_study,
    shaft_power_at_speed,
    size_fuel_chain,
    size_fuel_chain_voyage,
    tonnage,
    wind_assist_saving,
)
from digitalmodel.alt_fuel_ship_sizing.constants import (
    AMBIENT_TEMPERATURE_K,
    KNOT_MPS,
    LH2_DENSITY_KG_PER_M3,
    METHANOL_DENSITY_KG_PER_M3,
    METHANOL_HEAT_OF_VAPORIZATION_KJ_PER_KG,
    METHANOL_LOWER_HEATING_VALUE_MJ_PER_KG,
    NH3_HEAT_OF_VAPORIZATION_KJ_PER_KG,
    NH3_LIQUID_DENSITY_KG_PER_M3,
    NH3_LOWER_HEATING_VALUE_MJ_PER_KG,
    NH3_NBP_K,
)
from digitalmodel.alt_fuel_ship_sizing.trade_study import TradeStudyInputs
from digitalmodel.alt_fuel_ship_sizing.trade_study import (
    WindAssistInputs as TSWind,
)

ROUND_FUEL = FuelProperties(lhv_mj_per_kg=120.0)  # published: 119.96 MJ/kg
TANK = TankParameters(
    ullage_fraction=0.08,
    length_to_diameter=5.0,
    insulation_heat_flux_w_per_m2=1.5,
    bog_handling="consumed",
)

MATRIX = ThrustMatrix.from_lists(
    tws_mps=[5.0, 10.0],
    twa_deg=[0.0, 90.0, 180.0],
    thrust_kn=[[0.0, 50.0, 20.0], [0.0, 100.0, 40.0]],
)


def _bulker_chain(**overrides):
    kwargs = dict(
        distance_nm=4000.0,
        speed_kn=12.0,
        shaft_power_kw=4750.0,
        hotel_load_kw=500.0,
        eta_electric_drivetrain=0.95,
        eta_fuel_cell_lhv=0.50,
        tank=TANK,
        fuel=ROUND_FUEL,
        fuel_margin_fraction=0.10,
    )
    kwargs.update(overrides)
    return size_fuel_chain(**kwargs)


class TestFuelChain:
    def test_energy_chain_hand_verified(self):
        """Hand arithmetic, all round numbers:

        endurance   = 4000 nm / 12 kn                = 333.333 h
        electrical  = 4750 / 0.95 + 500              = 5500 kW
        LHV power   = 5500 / 0.50                    = 11000 kW
        mass flow   = 11000 kW * 3600 s/h / 120e3 kJ = 330 kg/h
        voyage fuel = 330 * 333.333 * 1.10           = 121000 kg
        net volume  = 121000 / 70.85                 = 1707.83 m3
        gross       = 1707.83 / (1 - 0.08)           = 1856.34 m3
        """
        result = _bulker_chain()
        assert result.endurance_hours == pytest.approx(4000.0 / 12.0, rel=1e-12)
        assert result.electrical_power_kw == pytest.approx(5500.0, rel=1e-12)
        assert result.fuel_lhv_power_kw == pytest.approx(11000.0, rel=1e-12)
        assert result.fuel_mass_flow_kg_per_h == pytest.approx(330.0, rel=1e-12)
        assert result.voyage_fuel_kg == pytest.approx(121000.0, rel=1e-12)
        assert result.required_fuel_mass_kg == pytest.approx(121000.0, rel=1e-12)
        assert result.net_tank_volume_m3 == pytest.approx(
            121000.0 / LH2_DENSITY_KG_PER_M3, rel=1e-12
        )
        assert result.net_tank_volume_m3 == pytest.approx(1707.833, rel=1e-6)
        assert result.gross_tank_volume_m3 == pytest.approx(1856.341, rel=1e-6)

    def test_unit_functions_hand_verified(self):
        assert endurance_hours(4000.0, 12.0) == pytest.approx(333.3333333, rel=1e-9)
        assert electrical_power_kw(4750.0, 500.0, 0.95) == pytest.approx(5500.0)
        # 5500/0.5 = 11000 kW; * 3.6 / 120 MJ/kg = 330 kg/h
        assert fuel_mass_flow_kg_per_h(5500.0, 0.50, 120.0) == pytest.approx(330.0)

    def test_cylindrical_tank_geometry_hand_verified(self):
        """V = pi D^2 L / 4 with L = 5 D -> D = (4V / 5pi)^(1/3).

        For V = 1856.34 m3: D = 7.7899 m, L = 38.950 m,
        A = pi D L + pi D^2 / 2 = 1048.52 m2.
        """
        geometry = cylindrical_tank_geometry(1856.3407075573014, 5.0)
        d_expected = (4.0 * 1856.3407075573014 / (math.pi * 5.0)) ** (1.0 / 3.0)
        assert geometry.diameter_m == pytest.approx(d_expected, rel=1e-12)
        assert geometry.diameter_m == pytest.approx(7.78991, rel=1e-6)
        assert geometry.length_m == pytest.approx(5.0 * geometry.diameter_m, rel=1e-12)
        assert geometry.surface_area_m2 == pytest.approx(1048.523, rel=1e-6)
        # volume closes: pi D^2 L / 4
        assert (
            math.pi * geometry.diameter_m**2 * geometry.length_m / 4.0
        ) == pytest.approx(geometry.gross_volume_m3, rel=1e-12)

    def test_boil_off_hand_verified(self):
        """Q = 1.5 W/m2 * 1048.52 m2 = 1572.78 W;
        BOG = 1572.78 / 446000 J/kg * 86400 s/day = 304.68 kg/day;
        full-load mass = 70.85 * 1856.34 * 0.92 = 121000 kg;
        BOR = 100 * 304.68 / 121000 = 0.2518 %/day (plausible for a
        vacuum-insulated LH2 tank of this size).
        """
        geometry = cylindrical_tank_geometry(1856.3407075573014, 5.0)
        result = boil_off(geometry, TANK, FuelProperties())
        assert result.heat_leak_w == pytest.approx(1572.784, rel=1e-6)
        assert result.bog_kg_per_day == pytest.approx(304.6828, rel=1e-6)
        assert result.full_load_liquid_mass_kg == pytest.approx(121000.0, rel=1e-9)
        assert result.bog_rate_percent_per_day == pytest.approx(0.251804, rel=1e-5)

    def test_bog_consumed_no_extra_fuel_and_no_warning(self):
        result = _bulker_chain()
        # 304.7 kg/day BOG << 330*24 = 7920 kg/day consumption
        assert result.bog_lost_kg == 0.0
        assert not result.bog_exceeds_consumption
        assert result.warnings == []

    def test_bog_lost_grows_tank_by_fixed_point(self):
        """Vented BOG adds fuel: 304.7 kg/day * 13.89 days = ~4232 kg on the
        first pass; the converged requirement is 125332 kg (iteration adds
        ~100 kg more as the tank grows), gross volume 1922.8 m3 > 1856.3 m3.
        """
        lost_tank = TankParameters(
            ullage_fraction=0.08,
            length_to_diameter=5.0,
            insulation_heat_flux_w_per_m2=1.5,
            bog_handling="lost",
        )
        result = _bulker_chain(tank=lost_tank)
        assert result.bog_lost_kg == pytest.approx(4332.116, rel=1e-5)
        assert result.required_fuel_mass_kg == pytest.approx(125332.116, rel=1e-6)
        assert result.gross_tank_volume_m3 == pytest.approx(1922.8026, rel=1e-6)
        assert result.gross_tank_volume_m3 > 1856.34
        # converged: requirement == voyage fuel + lost BOG
        assert result.required_fuel_mass_kg == pytest.approx(
            result.voyage_fuel_kg + result.bog_lost_kg, abs=1e-5
        )

    def test_bog_exceeds_consumption_warns(self):
        """A deliberately poor insulation (150 W/m2) makes BOG generation
        exceed the fuel-cell draw -> warning under 'consumed' handling."""
        poor_tank = TankParameters(
            ullage_fraction=0.08,
            length_to_diameter=5.0,
            insulation_heat_flux_w_per_m2=150.0,
            bog_handling="consumed",
        )
        result = _bulker_chain(tank=poor_tank, hotel_load_kw=0.0, shaft_power_kw=95.0)
        assert result.bog_exceeds_consumption
        assert result.warnings

    def test_u_value_path_equals_heat_flux_path(self):
        """U = 1.5 / (293.15 - 20.28) W/m2K reproduces q = 1.5 W/m2."""
        u_tank = TankParameters(
            ullage_fraction=0.08,
            length_to_diameter=5.0,
            u_value_w_per_m2_k=1.5 / (293.15 - 20.28),
            ambient_temperature_k=293.15,
            bog_handling="consumed",
        )
        assert _bulker_chain(tank=u_tank).boil_off.heat_leak_w == pytest.approx(
            _bulker_chain().boil_off.heat_leak_w, rel=1e-12
        )

    def test_input_validation(self):
        with pytest.raises(ValueError):
            endurance_hours(-1.0, 12.0)
        with pytest.raises(ValueError):
            electrical_power_kw(4750.0, 500.0, 1.5)
        with pytest.raises(ValueError):
            fuel_mass_flow_kg_per_h(5500.0, 0.0)
        with pytest.raises(ValueError):
            TankParameters(ullage_fraction=1.0)
        with pytest.raises(ValueError):
            TankParameters(insulation_heat_flux_w_per_m2=1.5, bog_handling="flare")
        with pytest.raises(ValueError):
            TankParameters()  # neither heat flux nor U-value


def _voyage(legs, **overrides):
    kwargs = dict(
        shaft_power_kw=4750.0,
        hotel_load_kw=500.0,
        eta_electric_drivetrain=0.95,
        eta_fuel_cell_lhv=0.50,
        tank=TANK,
        fuel=ROUND_FUEL,
        fuel_margin_fraction=0.10,
    )
    kwargs.update(overrides)
    return size_fuel_chain_voyage(legs, **kwargs)


class TestVoyageProfile:
    ROUND_TRIP = [
        SeaLeg(distance_nm=2000.0, speed_kn=12.0),
        PortLeg(dwell_h=720.0),
        SeaLeg(distance_nm=2000.0, speed_kn=12.0),
    ]

    def test_single_sea_leg_reproduces_single_passage(self):
        """One sea leg == the single-passage API (backward compatibility):
        121000 kg, 1856.34 m3 gross (hand-verified in TestFuelChain)."""
        voyage = _voyage([SeaLeg(distance_nm=4000.0, speed_kn=12.0)])
        single = _bulker_chain()
        assert voyage.required_fuel_mass_kg == pytest.approx(
            single.required_fuel_mass_kg, rel=1e-12
        )
        assert voyage.gross_tank_volume_m3 == pytest.approx(
            single.gross_tank_volume_m3, rel=1e-12
        )
        assert voyage.sea_hours == pytest.approx(single.endurance_hours, rel=1e-12)
        assert voyage.port_dwell_h == 0.0
        assert voyage.total_distance_nm == pytest.approx(4000.0)
        assert len(voyage.legs) == 1
        assert voyage.legs[0].leg_type == "sea"

    def test_port_dwell_drives_tank_size(self):
        """The same 4000 nm route split around a 30-day port dwell needs a
        bigger tank than the dwell-free route: the dwell generates BOG at
        the converged tank's ~318 kg/day with zero burn to absorb it, so
        ~9619 kg is added to the 121000 kg sea requirement (fixed point:
        required = 130618.7 kg, gross 2003.91 m3 > 1856.34 m3).
        """
        no_dwell = _voyage(
            [
                SeaLeg(distance_nm=2000.0, speed_kn=12.0),
                SeaLeg(distance_nm=2000.0, speed_kn=12.0),
            ]
        )
        with_dwell = _voyage(self.ROUND_TRIP)

        assert no_dwell.required_fuel_mass_kg == pytest.approx(121000.0, rel=1e-9)
        assert no_dwell.gross_tank_volume_m3 == pytest.approx(1856.341, rel=1e-6)
        assert with_dwell.required_fuel_mass_kg == pytest.approx(
            130618.687, rel=1e-6
        )
        assert with_dwell.gross_tank_volume_m3 == pytest.approx(2003.907, rel=1e-6)
        assert (
            with_dwell.gross_tank_volume_m3 > no_dwell.gross_tank_volume_m3
        )
        # Fixed point closed: requirement == burned fuel (with margin) + lost BOG.
        assert with_dwell.required_fuel_mass_kg == pytest.approx(
            with_dwell.voyage_fuel_kg + with_dwell.bog_lost_kg, abs=1e-5
        )
        # All the lost BOG is the port dwell's, at the converged tank's rate.
        assert with_dwell.bog_lost_kg == pytest.approx(
            with_dwell.boil_off.bog_kg_per_day * 720.0 / 24.0, rel=1e-12
        )

    def test_per_leg_accounting(self):
        """Sea legs: 330 kg/h x 166.667 h = 55000 kg burned each, BOG
        consumed (fed to the fuel cell). Port leg: zero burn, all BOG lost."""
        voyage = _voyage(self.ROUND_TRIP)
        sea_a, port, sea_b = voyage.legs
        assert [leg.leg_type for leg in voyage.legs] == ["sea", "port", "sea"]
        assert sea_a.fuel_burned_kg == pytest.approx(55000.0, rel=1e-12)
        assert sea_b.fuel_burned_kg == pytest.approx(55000.0, rel=1e-12)
        assert sea_a.duration_h == pytest.approx(2000.0 / 12.0, rel=1e-12)
        assert sea_a.bog_lost_kg == 0.0
        assert sea_a.bog_consumed_kg == pytest.approx(
            voyage.boil_off.bog_kg_per_day * sea_a.duration_h / 24.0, rel=1e-12
        )
        assert port.fuel_burned_kg == 0.0
        assert port.duration_h == pytest.approx(720.0)
        assert port.bog_consumed_kg == 0.0
        assert port.bog_lost_kg == pytest.approx(port.bog_generated_kg, rel=1e-12)
        # Voyage totals roll up from the legs.
        assert voyage.voyage_fuel_kg == pytest.approx(
            (sea_a.fuel_burned_kg + sea_b.fuel_burned_kg) * 1.10, rel=1e-12
        )
        assert voyage.total_voyage_hours == pytest.approx(
            sea_a.duration_h + 720.0 + sea_b.duration_h, rel=1e-12
        )

    def test_lost_handling_vents_all_legs(self):
        lost_tank = TankParameters(
            ullage_fraction=0.08,
            length_to_diameter=5.0,
            insulation_heat_flux_w_per_m2=1.5,
            bog_handling="lost",
        )
        voyage = _voyage(self.ROUND_TRIP, tank=lost_tank)
        assert all(
            leg.bog_lost_kg == pytest.approx(leg.bog_generated_kg, rel=1e-12)
            for leg in voyage.legs
        )
        assert voyage.bog_lost_kg == pytest.approx(
            voyage.boil_off.bog_kg_per_day
            * voyage.total_voyage_hours
            / 24.0,
            rel=1e-12,
        )
        assert voyage.required_fuel_mass_kg == pytest.approx(
            voyage.voyage_fuel_kg + voyage.bog_lost_kg, abs=1e-5
        )

    def test_leg_shaft_power_override(self):
        """A leg-level shaft power (half the voyage-level 4750 kW) burns
        less on that leg: P_elec = 2375/0.95 + 500 = 3000 kW -> 180 kg/h."""
        voyage = _voyage(
            [
                SeaLeg(distance_nm=1200.0, speed_kn=12.0),
                SeaLeg(distance_nm=1200.0, speed_kn=12.0, shaft_power_kw=2375.0),
            ]
        )
        full, half = voyage.legs
        assert full.fuel_burned_kg == pytest.approx(330.0 * 100.0, rel=1e-12)
        assert half.fuel_burned_kg == pytest.approx(180.0 * 100.0, rel=1e-12)

    def test_bog_exceeds_consumption_flagged_per_sea_leg(self):
        """Poor insulation makes generation exceed the slow leg's draw; the
        warning names the offending sea leg (port legs are never flagged)."""
        poor_tank = TankParameters(
            ullage_fraction=0.08,
            length_to_diameter=5.0,
            insulation_heat_flux_w_per_m2=150.0,
            bog_handling="consumed",
        )
        voyage = _voyage(
            [
                SeaLeg(distance_nm=100.0, speed_kn=12.0, shaft_power_kw=95.0),
                PortLeg(dwell_h=24.0),
            ],
            tank=poor_tank,
            hotel_load_kw=0.0,
            # The absurd heat flux makes lost port BOG a large fraction of
            # the requirement; the fixed point still contracts (ratio ~2/3)
            # but needs more than the default 50 passes to reach 1e-6 kg.
            max_iterations=200,
        )
        assert voyage.legs[0].bog_exceeds_consumption
        assert not voyage.legs[1].bog_exceeds_consumption
        assert voyage.bog_exceeds_consumption
        assert voyage.warnings and "sea leg(s) 0" in voyage.warnings[0]

    def test_validation(self):
        with pytest.raises(ValueError):
            _voyage([])
        with pytest.raises(ValueError):
            _voyage([PortLeg(dwell_h=24.0)])  # no sea leg
        with pytest.raises(ValueError):
            SeaLeg(distance_nm=-1.0, speed_kn=12.0)
        with pytest.raises(ValueError):
            SeaLeg(distance_nm=100.0, speed_kn=0.0)
        with pytest.raises(ValueError):
            PortLeg(dwell_h=0.0)


class TestFuelPresets:
    def test_lh2_preset_is_the_default(self):
        assert fuel_preset("lh2") == FuelProperties()
        assert FUEL_PRESETS["lh2"] == FuelProperties()

    def test_nh3_refrigerated_preset_cited_values(self):
        nh3 = fuel_preset("nh3_refrigerated")
        assert nh3.density_kg_per_m3 == NH3_LIQUID_DENSITY_KG_PER_M3 == 682.0
        assert nh3.lhv_mj_per_kg == NH3_LOWER_HEATING_VALUE_MJ_PER_KG == 18.6
        assert (
            nh3.heat_of_vaporization_kj_per_kg
            == NH3_HEAT_OF_VAPORIZATION_KJ_PER_KG
            == 1370.0
        )
        assert nh3.storage_temperature_k == NH3_NBP_K == 239.82

    def test_methanol_preset_cited_values(self):
        meoh = fuel_preset("methanol")
        assert meoh.density_kg_per_m3 == METHANOL_DENSITY_KG_PER_M3 == 791.4
        assert meoh.lhv_mj_per_kg == METHANOL_LOWER_HEATING_VALUE_MJ_PER_KG == 19.9
        assert (
            meoh.heat_of_vaporization_kj_per_kg
            == METHANOL_HEAT_OF_VAPORIZATION_KJ_PER_KG
            == 1100.0
        )
        assert meoh.storage_temperature_k == AMBIENT_TEMPERATURE_K == 293.15

    def test_unknown_preset_rejected(self):
        with pytest.raises(ValueError, match="nh3_refrigerated"):
            fuel_preset("nh3_pressurized")

    def test_nh3_fuel_chain_mass_flow_hand_verified(self):
        """Same 5500 kW electrical / 50 % plant on NH3: 11000 kW LHV x 3.6 /
        18.6 MJ/kg = 2129.032 kg/h (vs 330 kg/h on round-number LH2)."""
        result = _bulker_chain(fuel=fuel_preset("nh3_refrigerated"))
        assert result.fuel_mass_flow_kg_per_h == pytest.approx(
            11000.0 * 3.6 / 18.6, rel=1e-12
        )
        assert result.fuel_mass_flow_kg_per_h == pytest.approx(2129.0323, rel=1e-7)
        # Denser fuel: net volume = required / 682 kg/m3.
        assert result.net_tank_volume_m3 == pytest.approx(
            result.required_fuel_mass_kg / 682.0, rel=1e-12
        )

    def test_methanol_u_value_tank_rejected_at_ambient(self):
        """Methanol stores at ambient: a U-value tank characterisation sees
        zero temperature difference and must raise (no boil-off regime)."""
        u_tank = TankParameters(
            u_value_w_per_m2_k=0.05,
            ambient_temperature_k=AMBIENT_TEMPERATURE_K,
        )
        with pytest.raises(ValueError, match="ambient_temperature_k"):
            _bulker_chain(tank=u_tank, fuel=fuel_preset("methanol"))


class TestWindAssist:
    def test_bilinear_interpolation_hand_verified(self):
        """Grid corners (5,0)=0, (5,90)=50, (10,0)=0, (10,90)=100; the
        midpoint (7.5 m/s, 45 deg) averages the four -> 37.5 kN."""
        assert interpolate_thrust(MATRIX, 7.5, 45.0) == pytest.approx(37.5)
        # on-node lookups
        assert interpolate_thrust(MATRIX, 10.0, 90.0) == pytest.approx(100.0)
        assert interpolate_thrust(MATRIX, 5.0, 180.0) == pytest.approx(20.0)

    def test_edge_handling(self):
        """Below the lowest tabulated wind speed the edge thrust scales with
        (tws/tws_min)^2: 50 * (2.5/5)^2 = 12.5 kN. Above the top row it
        clamps; angles fold port/starboard (270 deg -> 90 deg)."""
        assert interpolate_thrust(MATRIX, 2.5, 90.0) == pytest.approx(12.5)
        assert interpolate_thrust(MATRIX, 0.0, 90.0) == pytest.approx(0.0)
        assert interpolate_thrust(MATRIX, 50.0, 90.0) == pytest.approx(100.0)
        assert interpolate_thrust(MATRIX, 10.0, 270.0) == pytest.approx(100.0)
        assert interpolate_thrust(MATRIX, 10.0, -90.0) == pytest.approx(100.0)

    def test_wind_rose_saving_hand_verified(self):
        """12 kn = 12 * 1852/3600 = 6.17333 m/s.

        bin 1: 100 kN * 6.17333 / 0.70 = 881.905 kW at p = 0.50
        bin 2:  20 kN * 6.17333 / 0.70 = 176.381 kW at p = 0.25
        expected saving = 0.5 * 881.905 + 0.25 * 176.381 = 485.048 kW
        (remaining p = 0.25 is calm -> no thrust);
        saving fraction = 485.048 / 4750 = 0.10212.
        """
        assert 12.0 * KNOT_MPS == pytest.approx(6.173333333, rel=1e-9)
        result = wind_assist_saving(
            MATRIX,
            [WindRoseBin(10.0, 90.0, 0.5), WindRoseBin(5.0, 180.0, 0.25)],
            ship_speed_kn=12.0,
            required_shaft_power_kw=4750.0,
            propulsive_efficiency=0.70,
        )
        assert result.bins[0].power_saving_kw == pytest.approx(881.9048, rel=1e-6)
        assert result.bins[1].power_saving_kw == pytest.approx(176.3810, rel=1e-6)
        assert result.expected_power_saving_kw == pytest.approx(485.0476, rel=1e-6)
        assert result.saving_fraction == pytest.approx(0.1021153, rel=1e-5)
        assert result.effective_shaft_power_kw == pytest.approx(
            4750.0 - 485.0476, rel=1e-6
        )

    def test_saving_capped_at_required_power(self):
        result = wind_assist_saving(
            MATRIX,
            [WindRoseBin(10.0, 90.0, 1.0)],
            ship_speed_kn=12.0,
            required_shaft_power_kw=500.0,  # < 881.9 kW available
            propulsive_efficiency=0.70,
        )
        assert result.expected_power_saving_kw == pytest.approx(500.0)
        assert result.effective_shaft_power_kw == pytest.approx(0.0)

    def test_validation(self):
        with pytest.raises(ValueError):
            ThrustMatrix.from_lists([5.0], [0.0, 90.0], [[0.0, 1.0]])
        with pytest.raises(ValueError):
            ThrustMatrix.from_lists([5.0, 10.0], [0.0, 90.0], [[0.0, 1.0]])
        with pytest.raises(ValueError):
            wind_assist_saving(
                MATRIX,
                [WindRoseBin(10.0, 90.0, 0.9), WindRoseBin(5.0, 90.0, 0.2)],
                ship_speed_kn=12.0,
                required_shaft_power_kw=4750.0,
                propulsive_efficiency=0.70,
            )


class TestTradeStudy:
    def _inputs(self, **overrides):
        kwargs = dict(
            distance_nm=4000.0,
            reference_speed_kn=12.0,
            reference_shaft_power_kw=4750.0,
            hotel_load_kw=500.0,
            eta_electric_drivetrain=0.95,
            eta_fuel_cell_lhv=0.50,
            tank=TANK,
            fuel=ROUND_FUEL,
            fuel_margin_fraction=0.10,
            wind=TSWind(
                matrix=MATRIX,
                wind_rose=[WindRoseBin(10.0, 90.0, 0.5), WindRoseBin(5.0, 180.0, 0.25)],
                propulsive_efficiency=0.70,
            ),
        )
        kwargs.update(overrides)
        return TradeStudyInputs(**kwargs)

    def test_cube_law_hand_verified(self):
        """P(6 kn) = 4750 * (6/12)^3 = 4750 / 8 = 593.75 kW."""
        inputs = self._inputs()
        assert shaft_power_at_speed(inputs, 12.0) == pytest.approx(4750.0)
        assert shaft_power_at_speed(inputs, 6.0) == pytest.approx(593.75)

    def test_speed_power_curve_interpolation(self):
        """Linear between (10, 3000) and (14, 7000): 12 kn -> 5000 kW."""
        inputs = self._inputs(speed_power_curve=[(10.0, 3000.0), (14.0, 7000.0)])
        assert shaft_power_at_speed(inputs, 12.0) == pytest.approx(5000.0)
        with pytest.raises(ValueError):
            shaft_power_at_speed(inputs, 9.0)

    def test_table_matches_fuel_chain(self):
        """The no-wind 12 kn row reproduces the hand-verified fuel chain
        (121000 kg, 1856.34 m3); wind rows carry less fuel."""
        rows = run_trade_study(self._inputs(), [10.0, 12.0], [False, True])
        assert len(rows) == 4
        by_id = {row["case_id"]: row for row in rows}
        base = by_id["v12kn_nowind"]
        assert base["required_fuel_mass_kg"] == pytest.approx(121000.0, rel=1e-9)
        assert base["gross_tank_volume_m3"] == pytest.approx(1856.341, rel=1e-6)
        assert base["fuel_saving_vs_no_wind_percent"] == pytest.approx(0.0)
        wind = by_id["v12kn_wind"]
        assert wind["required_fuel_mass_kg"] < base["required_fuel_mass_kg"]
        assert wind["fuel_saving_vs_no_wind_percent"] > 0.0
        # only the shaft-power share is reduced; hotel load is not
        assert wind["fuel_saving_vs_no_wind_percent"] < 100.0 * (
            wind["wind_power_saving_kw"] / base["shaft_power_kw"]
        )

    def test_fuel_cost_column(self):
        """121000 kg * 6.0 /kg = 726000 for the no-wind 12 kn case."""
        rows = run_trade_study(
            self._inputs(fuel_price_per_kg=6.0), [12.0], [False]
        )
        assert rows[0]["fuel_cost"] == pytest.approx(726000.0, rel=1e-9)

    def test_wind_requested_without_inputs_raises(self):
        with pytest.raises(ValueError):
            run_trade_study(self._inputs(wind=None), [12.0], [True])


class TestTonnage:
    def test_gross_tonnage_hand_verified(self):
        """ITC 69 Annex I Reg 3: V = 100000 m3 -> K1 = 0.2 + 0.02*5 = 0.30,
        GT = 30000 exactly. V = 10000 -> K1 = 0.28, GT = 2800."""
        assert gross_tonnage(100000.0) == pytest.approx(30000.0, rel=1e-12)
        assert gross_tonnage(10000.0) == pytest.approx(2800.0, rel=1e-12)

    def test_net_tonnage_floors_hand_verified(self):
        """Reg 4 provisos, exact numbers: V = 100000 -> GT = 30000;
        Vc = 10000 -> K2 = 0.28; d = 7.5, D = 10 -> 4d/3D = 1 (factor 1.0);
        cargo term = 0.28 * 10000 = 2800 < 0.25 GT = 7500 -> floored;
        NT = 7500 < 0.30 GT = 9000 -> floored to 9000."""
        result = tonnage(100000.0, 10000.0, 7.5, 10.0)
        assert result.gross_tonnage == pytest.approx(30000.0, rel=1e-12)
        assert result.draught_depth_factor == pytest.approx(1.0)
        assert result.cargo_term_floor_applied
        assert result.cargo_term == pytest.approx(7500.0, rel=1e-12)
        assert result.nt_floor_applied
        assert result.net_tonnage == pytest.approx(9000.0, rel=1e-12)

    def test_net_tonnage_passenger_term_hand_verified(self):
        """V = 100000 -> GT = 30000, K3 = 1.25*(30000+10000)/10000 = 5.0;
        N1 = 100, N2 = 200 -> K3*(N1 + N2/10) = 5*(100+20) = 600 exactly;
        Vc = 60000 -> K2 = 0.2 + 0.02*log10(60000) = 0.2955630;
        d = 10, D = 15 -> factor = (40/45)^2 = 0.7901235;
        cargo term = 0.2955630*60000*0.7901235 = 14011.877 (no floors);
        NT = 14011.877 + 600 = 14611.877."""
        result = tonnage(100000.0, 60000.0, 10.0, 15.0, 100, 200)
        assert result.k2 == pytest.approx(0.2955630250, rel=1e-9)
        assert result.k3 == pytest.approx(5.0, rel=1e-12)
        assert result.draught_depth_factor == pytest.approx(
            (40.0 / 45.0) ** 2, rel=1e-12
        )
        assert result.passenger_term == pytest.approx(600.0, rel=1e-12)
        assert result.cargo_term == pytest.approx(14011.8767, rel=1e-8)
        assert result.net_tonnage == pytest.approx(14611.8767, rel=1e-8)
        assert not result.cargo_term_floor_applied
        assert not result.nt_floor_applied

    def test_fewer_than_13_passengers_taken_as_zero(self):
        """Reg 4: N1 + N2 < 13 -> both zero. 5 + 7 = 12 passengers give the
        same NT as a pure cargo ship (1757.625 for V=10000, Vc=8000,
        d=8, D=12)."""
        with_pax = tonnage(10000.0, 8000.0, 8.0, 12.0, 5, 7)
        without = tonnage(10000.0, 8000.0, 8.0, 12.0)
        assert with_pax.passenger_term == 0.0
        assert with_pax.net_tonnage == pytest.approx(without.net_tonnage, rel=1e-12)
        assert with_pax.net_tonnage == pytest.approx(1757.6252, rel=1e-7)

    def test_factor_capped_at_unity(self):
        deep = tonnage(100000.0, 60000.0, 12.0, 12.0)  # 4d/3D = 4/3 -> capped
        assert deep.draught_depth_factor == pytest.approx(1.0)

    def test_validation(self):
        with pytest.raises(ValueError):
            tonnage(10000.0, 20000.0, 8.0, 12.0)  # Vc > V
        with pytest.raises(ValueError):
            tonnage(-1.0, 100.0, 8.0, 12.0)
        with pytest.raises(ValueError):
            tonnage(10000.0, 8000.0, 8.0, 12.0, -1, 0)
