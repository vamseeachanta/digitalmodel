#!/usr/bin/env python3
"""
ABOUTME: TDD tests for the CFD case-coupling and pre-run verification layer
(#1528 slice 4). Maps a reduced-order SweepCase (+ tank/conduit/roll spec) into a
buildable OpenFOAMCase, verifies the half-sine flow-pulse integral, mass/volume
conservation and dry-out/overflow feasibility BEFORE any CFD run, and emits a
de-identified synthetic gravity-exchange case + manifest. Independent oracles
(hand-computed Q_peak, builder-emitted dict files) stand in for truth.
"""

import math

import pytest

from digitalmodel.solvers.openfoam.case_builder import OpenFOAMCaseBuilder
from digitalmodel.solvers.openfoam.gravity_conduit import ConduitGeometry
from digitalmodel.solvers.openfoam.models import CaseType, OpenFOAMCase
from digitalmodel.solvers.openfoam.motion import MotionType
from digitalmodel.solvers.openfoam.sloshing_sweep import SweepCase
from digitalmodel.solvers.openfoam.case_coupling import (
    CaseManifest,
    CouplingSpec,
    SyntheticCase,
    VerificationResult,
    build_case_manifest,
    emit_synthetic_gravity_exchange_case,
    map_sweep_case_to_openfoam_case,
    synthetic_coupling_spec,
    synthetic_sweep_case,
    verify_coupling,
)


# ============================================================================
# Test fixtures / helpers (de-identified, synthetic)
# ============================================================================


def _spec(**overrides) -> CouplingSpec:
    """A feasible synthetic tank/conduit/roll spec (overridable)."""
    base = dict(
        tank_length=20.0,
        tank_width=6.0,
        tank_height=10.0,
        roll_amplitude_deg=5.0,
        roll_origin=(10.0, 3.0, 0.0),
        conduit=ConduitGeometry(area=0.5, discharge_coefficient=0.6, loss_coefficient=1.5),
        dest_fill_fraction=0.30,
        density=1025.0,
        n_cells=(40, 8, 40),
    )
    base.update(overrides)
    return CouplingSpec(**base)


def _sweep_case(
    *,
    fill_fraction: float = 0.50,
    conduit_capacity: float = 0.08,
    tank_height: float = 10.0,
    roll_period_s: float = 6.0,
    near_resonance: bool = True,
) -> SweepCase:
    """Construct a SweepCase row directly (public frozen dataclass)."""
    return SweepCase(
        case_id="synthetic-abcdef012345",
        fill_fraction=fill_fraction,
        fill_depth=fill_fraction * tank_height,
        conduit_capacity=conduit_capacity,
        period_ratio=1.0,
        natural_frequency_hz=1.0 / roll_period_s,
        natural_period_s=roll_period_s,
        roll_frequency_hz=1.0 / roll_period_s,
        roll_period_s=roll_period_s,
        near_resonance=near_resonance,
        approximation="linear_potential_tanh_dispersion",
    )


# ============================================================================
# Mapper: SweepCase -> OpenFOAMCase
# ============================================================================


class TestMapper:
    def test_returns_multiphase_sloshing_interfoam_case(self):
        case = map_sweep_case_to_openfoam_case(_sweep_case(), _spec())
        assert isinstance(case, OpenFOAMCase)
        assert case.case_type is CaseType.SLOSHING
        assert case.solver_config.solver_name == "interFoam"
        assert case.solver_config.is_multiphase is True

    def test_case_name_is_case_id(self):
        sc = _sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, _spec())
        assert case.name == sc.case_id

    def test_fill_level_from_fill_fraction(self):
        sc = _sweep_case(fill_fraction=0.42)
        case = map_sweep_case_to_openfoam_case(sc, _spec())
        assert case.fill_level == pytest.approx(0.42)

    def test_domain_from_tank_dimensions(self):
        case = map_sweep_case_to_openfoam_case(_sweep_case(), _spec())
        assert case.domain.min_coords == [0.0, 0.0, 0.0]
        assert case.domain.max_coords == [20.0, 6.0, 10.0]
        # Vertical (z) extent must equal tank_height so fill_level*height == fill_depth.
        assert case.domain.max_coords[2] - case.domain.min_coords[2] == pytest.approx(10.0)

    def test_roll_motion_from_roll_period(self):
        sc = _sweep_case(roll_period_s=6.0)
        case = map_sweep_case_to_openfoam_case(sc, _spec())
        assert case.motion is not None
        assert case.motion.motion_type is MotionType.ROLL
        assert case.motion.period == pytest.approx(6.0)
        assert case.motion.amplitude == pytest.approx(5.0)
        assert case.motion.frequency_hz == pytest.approx(sc.roll_frequency_hz)
        assert case.motion.origin == (10.0, 3.0, 0.0)

    def test_defines_inlet_and_outlet_boundary_conditions(self):
        case = map_sweep_case_to_openfoam_case(_sweep_case(), _spec())
        patches = {bc.patch_name for bc in case.boundary_conditions}
        assert "inlet" in patches
        assert "outlet" in patches

    def test_metadata_carries_case_id_capacity_and_phase_convention(self):
        sc = _sweep_case(conduit_capacity=0.08)
        case = map_sweep_case_to_openfoam_case(sc, _spec())
        assert case.metadata["case_id"] == sc.case_id
        assert case.metadata["conduit_capacity"] == pytest.approx(0.08)
        assert isinstance(case.metadata["phase_convention"], str)
        assert case.metadata["phase_convention"]  # non-empty

    def test_metadata_q_peak_matches_hand_computed_half_sine_law(self):
        spec = _spec()
        sc = _sweep_case(fill_fraction=0.50, conduit_capacity=0.08, roll_period_s=6.0)
        case = map_sweep_case_to_openfoam_case(sc, spec)
        # Independent oracle.
        capacity = spec.tank_length * spec.tank_width * spec.tank_height
        v_transfer = 0.08 * capacity
        f = 1.0 / 6.0
        q_peak = math.pi * f * v_transfer
        assert case.metadata["transfer_volume_m3"] == pytest.approx(v_transfer)
        assert case.metadata["q_peak_m3_s"] == pytest.approx(q_peak)

    def test_rejects_spec_inconsistent_with_sweep_fill_depth(self):
        # sweep fill_depth built on tank_height=10; spec says 12 -> inconsistent.
        sc = _sweep_case(fill_fraction=0.5, tank_height=10.0)
        with pytest.raises(ValueError, match="fill_depth"):
            map_sweep_case_to_openfoam_case(sc, _spec(tank_height=12.0))


# ============================================================================
# Verification gate
# ============================================================================


class TestGatePasses:
    def test_gate_passes_for_feasible_case(self):
        spec = _spec()
        sc = _sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, spec)
        result = verify_coupling(case, sc, spec)
        assert isinstance(result, VerificationResult)
        assert result.passed is True
        assert result.feasible is True
        assert result.boundary_conditions_ok is True

    def test_gate_pulse_integral_identity(self):
        spec = _spec()
        sc = _sweep_case(conduit_capacity=0.08, roll_period_s=6.0)
        case = map_sweep_case_to_openfoam_case(sc, spec)
        result = verify_coupling(case, sc, spec)
        capacity = spec.tank_length * spec.tank_width * spec.tank_height
        v_transfer = 0.08 * capacity
        q_peak = math.pi * (1.0 / 6.0) * v_transfer
        # V_transfer == Q_peak * T / pi (the volume-integral identity).
        assert result.q_peak_m3_s == pytest.approx(q_peak)
        assert result.pulse_integral_m3 == pytest.approx(v_transfer)
        assert result.pulse_integral_m3 == pytest.approx(q_peak * 6.0 / math.pi)
        assert result.transfer_volume_m3 == pytest.approx(v_transfer)

    def test_gate_reports_small_mass_and_volume_residual(self):
        spec = _spec()
        sc = _sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, spec)
        result = verify_coupling(case, sc, spec)
        assert result.mass_residual_kg < 1e-6
        assert result.volume_residual_m3 < 1e-9

    def test_gate_passes_zero_capacity_control(self):
        spec = _spec()
        sc = _sweep_case(conduit_capacity=0.0)
        case = map_sweep_case_to_openfoam_case(sc, spec)
        result = verify_coupling(case, sc, spec)
        assert result.passed is True
        assert result.transfer_volume_m3 == pytest.approx(0.0)
        assert result.q_peak_m3_s == pytest.approx(0.0)


class TestGateFailures:
    def test_rejects_transfer_exceeding_available_liquid_dryout(self):
        spec = _spec()
        # source volume = 0.05*1200 = 60; V_transfer = 0.10*1200 = 120 -> dry-out.
        sc = _sweep_case(fill_fraction=0.05, conduit_capacity=0.10)
        case = map_sweep_case_to_openfoam_case(sc, spec)
        with pytest.raises(ValueError, match="(?i)dry-out|available liquid"):
            verify_coupling(case, sc, spec)

    def test_rejects_transfer_exceeding_free_volume_overflow(self):
        spec = _spec(dest_fill_fraction=0.95)  # dest free = 0.05*1200 = 60
        sc = _sweep_case(fill_fraction=0.5, conduit_capacity=0.10)  # V_transfer=120
        case = map_sweep_case_to_openfoam_case(sc, spec)
        with pytest.raises(ValueError, match="(?i)overflow|free volume"):
            verify_coupling(case, sc, spec)

    def test_rejects_missing_boundary_conditions(self):
        spec = _spec()
        sc = _sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, spec)
        case.boundary_conditions = []  # strip the inlet/outlet BCs
        with pytest.raises(ValueError, match="(?i)inlet|outlet|boundary"):
            verify_coupling(case, sc, spec)

    def test_rejects_non_conserving_plan_tampered_q_peak(self):
        spec = _spec()
        sc = _sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, spec)
        # Corrupt the recorded flow law so Q_peak*T/pi != V_transfer.
        case.metadata["q_peak_m3_s"] = case.metadata["q_peak_m3_s"] * 2.0
        with pytest.raises(ValueError, match="(?i)conserv|pulse|q_peak|integral"):
            verify_coupling(case, sc, spec)

    def test_rejects_missing_phase_convention(self):
        spec = _spec()
        sc = _sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, spec)
        del case.metadata["phase_convention"]
        with pytest.raises(ValueError, match="(?i)phase convention"):
            verify_coupling(case, sc, spec)


# ============================================================================
# Synthetic case + manifest
# ============================================================================


class TestSyntheticCase:
    def test_synthetic_spec_and_case_are_feasible_and_pass_gate(self):
        spec = synthetic_coupling_spec()
        sc = synthetic_sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, spec)
        result = verify_coupling(case, sc, spec)
        assert result.passed is True

    def test_emit_returns_buildable_case_that_passes_gate(self):
        synthetic = emit_synthetic_gravity_exchange_case()
        assert isinstance(synthetic, SyntheticCase)
        assert synthetic.verification.passed is True
        assert synthetic.case.solver_config.is_multiphase is True

    def test_builder_emits_case_tree_with_key_dicts(self, tmp_path):
        synthetic = emit_synthetic_gravity_exchange_case()
        case_dir = OpenFOAMCaseBuilder(synthetic.case).build(tmp_path)
        assert (case_dir / "system" / "controlDict").is_file()
        assert (case_dir / "system" / "blockMeshDict").is_file()
        # motion -> dynamicMeshDict ; partial fill -> setFieldsDict
        assert (case_dir / "constant" / "dynamicMeshDict").is_file()
        assert (case_dir / "system" / "setFieldsDict").is_file()

    def test_manifest_has_case_id_inputs_solver_and_hash(self):
        synthetic = emit_synthetic_gravity_exchange_case()
        m = synthetic.manifest
        assert isinstance(m, CaseManifest)
        assert m.case_id == synthetic.case.name
        assert isinstance(m.inputs, dict) and m.inputs
        assert isinstance(m.solver, dict) and m.solver.get("solver_name") == "interFoam"
        assert m.content_hash and len(m.content_hash) >= 8

    def test_manifest_is_deterministic(self):
        a = emit_synthetic_gravity_exchange_case().manifest
        b = emit_synthetic_gravity_exchange_case().manifest
        assert a.content_hash == b.content_hash

    def test_manifest_carries_no_absolute_paths(self):
        m = emit_synthetic_gravity_exchange_case().manifest

        def _walk(obj):
            if isinstance(obj, dict):
                for v in obj.values():
                    yield from _walk(v)
            elif isinstance(obj, (list, tuple)):
                for v in obj:
                    yield from _walk(v)
            elif isinstance(obj, str):
                yield obj

        for s in _walk({"inputs": m.inputs, "solver": m.solver}):
            assert not s.startswith("/"), f"absolute path leaked: {s!r}"
            assert ":\\" not in s, f"windows abs path leaked: {s!r}"

    def test_build_case_manifest_matches_metadata_flow_law(self):
        spec = synthetic_coupling_spec()
        sc = synthetic_sweep_case()
        case = map_sweep_case_to_openfoam_case(sc, spec)
        result = verify_coupling(case, sc, spec)
        m = build_case_manifest(case, sc, spec, result)
        assert m.transfer_volume_m3 == pytest.approx(case.metadata["transfer_volume_m3"])
        assert m.q_peak_m3_s == pytest.approx(case.metadata["q_peak_m3_s"])
