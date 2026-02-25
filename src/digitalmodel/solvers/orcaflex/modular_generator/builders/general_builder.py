"""Builder for the General section of OrcaFlex models."""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


@BuilderRegistry.register("01_general.yml", order=10)
class GeneralBuilder(BaseBuilder):
    """Builds the General section of the OrcaFlex model.

    The General section contains simulation control settings including:
    - Units system
    - Time stepping parameters
    - Stage durations
    - Solver configuration
    - North direction

    Reference: 01_general.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the General section from simulation settings.

        Returns:
            Dictionary with 'General' key containing OrcaFlex settings.
        """
        sim = self.spec.simulation

        # Preserve original unit system from extracted models.
        # Default to SI only when no generic properties are available.
        # Unit sub-properties (LengthUnits, MassUnits, ForceUnits, g) MUST
        # appear in this file (order=10) BEFORE the Environment section
        # (order=30), otherwise OrcFxAPI interprets environment values with
        # wrong units when UnitsSystem is "User".
        units_system = "SI"
        gen_props: dict[str, Any] = {}
        generic = getattr(self.spec, "generic", None)
        if generic and generic.general_properties:
            gen_props = generic.general_properties
            units_system = gen_props.get("UnitsSystem", units_system)

        general: dict[str, Any] = {
            "UnitsSystem": units_system,
        }
        # Emit unit sub-properties for non-SI models so they're set early
        if units_system != "SI":
            for key in ("LengthUnits", "MassUnits", "ForceUnits", "g"):
                if key in gen_props:
                    general[key] = gen_props[key]

        general.update({
            "JacobianBufferingPolicy": 1,
            "JacobianPerturbationFactor": 0,
            "BuoysIncludedInStatics": "Individually specified",
            "LineStaticsStep1Policy": "All lines included",
            "LineStaticsStep2Policy": "Solve coupled systems",
            "WholeSystemStaticsEnabled": True,
            "DynamicsSolutionMethod": "Implicit time domain",
            "ImplicitUseVariableTimeStep": False,
            "ImplicitConstantTimeStep": sim.time_step,
            "LogPrecision": "Single",
            "TargetLogSampleInterval": sim.time_step,
            "LogStartTime": None,
            "StartTime": None,
            "FirstStage": None,
            "RampStartTime": None,
            "RampFinishTime": None,
            "TimeHistoryImportFrom": None,
            "TimeHistoryImportTo": None,
            "StageDuration": list(sim.stages),
            "RestartStateRecordingPeriodicCount": 0,
            "RestartStateRecordingTest": None,
        })

        # Only emit NorthDirection if non-zero (OrcaFlex defaults to 0)
        if sim.north_direction:
            general["NorthDirection"] = sim.north_direction

        return {"General": general}
