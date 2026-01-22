"""Builder for the General section of OrcaFlex models."""

from typing import Any

from .base import BaseBuilder


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

        general = {
            "UnitsSystem": "SI",
            "JacobianBufferingPolicy": 1,
            "JacobianPerturbationFactor": 0,
            "BuoysIncludedInStatics": "Individually specified",
            "LineStaticsStep1Policy": "All lines included",
            "LineStaticsStep2Policy": "Solve coupled systems",
            "WholeSystemStaticsEnabled": "Yes",
            "DynamicsSolutionMethod": "Implicit time domain",
            "ImplicitUseVariableTimeStep": "No",
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
            "NorthDirection": sim.north_direction,
        }

        return {"General": general}
