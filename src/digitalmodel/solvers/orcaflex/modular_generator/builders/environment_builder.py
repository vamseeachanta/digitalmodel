"""Builder for the Environment section of OrcaFlex models."""

from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry


# Mapping from spec wave types to OrcaFlex wave type strings
WAVE_TYPE_MAP = {
    "dean_stream": "Dean stream",
    "airy": "Airy",
    "stokes_5th": "Stokes' 5th",
    "cnoidal": "Cnoidal",
    "jonswap": "JONSWAP",
    "pierson_moskowitz": "Pierson-Moskowitz",
    "user_defined": "User defined spectrum",
}


@BuilderRegistry.register("03_environment.yml", order=30)
class EnvironmentBuilder(BaseBuilder):
    """Builds the Environment section of the OrcaFlex model.

    The Environment section contains:
    - Water properties (density, surface level, viscosity)
    - Seabed properties (type, stiffness, slope)
    - Wave conditions (type, height, period, direction)
    - Current conditions (speed, direction, profile)
    - Wind conditions (speed, direction, air properties)

    Reference: 03_environment.yml in modular include format.
    """

    def build(self) -> dict[str, Any]:
        """Build the Environment section from environmental settings.

        Returns:
            Dictionary with 'Environment' key containing OrcaFlex settings.
        """
        env = self.spec.environment

        environment = {
            # Water properties
            "WaterSurfaceZ": 0,
            "KinematicViscosity": 1.35e-06,
            "SeaTemperature": 10,
            "ReynoldsNumberCalculation": "Flow direction",
            "HorizontalWaterDensityFactor": None,
            "VerticalDensityVariation": "Constant",
            "Density": env.water.density,
            # Seabed properties
            "SeabedType": "Flat",
            "SeabedOrigin": [0, 0],
            "SeabedOriginDepth": env.water.depth,
            "NominalDepth": None,
            "SeabedSlopeDirection": 180,
            "SeabedSlope": env.seabed.slope,
            "SeabedModel": "Elastic",
            "SeabedNormalStiffness": env.seabed.stiffness.normal,
            "SeabedShearStiffness": env.seabed.stiffness.shear,
            # Wave conditions
            "WaveTrains": self._build_wave_trains(env.waves),
            "WaveKinematicsCutoffDepth": "Infinity",
            "WaveCalculationMethod": "Instantaneous position (exact)",
            "WaveCalculationTimeInterval": 0,
            "WaveCalculationSpatialInterval": 0,
            # Current conditions
            "MultipleCurrentDataCanBeDefined": "No",
            "CurrentModel": "Variation scheme",
            "CurrentRamped": "No",
            "CurrentApplyVerticalStretching": "No",
            "HorizontalCurrentFactor": None,
            "VerticalCurrentVariationMethod": "Interpolated",
            "RefCurrentSpeed": env.current.speed,
            "RefCurrentDirection": env.current.direction,
            "CurrentDepth, CurrentFactor, CurrentRotation": self._build_current_profile(
                env.current.profile
            ),
            # Wind conditions
            "IncludeVesselWindLoads": "Yes",
            "IncludeLineWindLoads": "Yes",
            "IncludeBuoyWindLoads": "Yes",
            "IncludeBuoyWingWindLoads": "Yes",
            "WindRamping": "From mean",
            "WindType": "Constant",
            "AirDensity": 0.00128,
            "AirSpeedOfSound": 343,
            "WindSpeed": env.wind.speed,
            "WindDirection": env.wind.direction,
            "VerticalWindVariationFactor": None,
        }

        return {"Environment": environment}

    def _build_wave_trains(self, waves: Any) -> list[dict[str, Any]]:
        """Build wave train configuration from wave settings.

        Args:
            waves: Wave specification from the input spec.

        Returns:
            List containing wave train definitions.
        """
        # Map wave type to OrcaFlex enum value
        wave_type = WAVE_TYPE_MAP.get(waves.type, waves.type)

        wave_train = {
            "Name": "Wave1",
            "WaveType": wave_type,
            "WaveDirection": waves.direction,
            "WaveHeight": waves.height,
            "WavePeriod": waves.period,
            "WaveOrigin": [0, 0],
            "WaveTimeOrigin": 0,
        }

        # Add wave-type specific parameters
        if waves.type == "dean_stream":
            wave_train["WaveStreamFunctionOrder"] = 5
            wave_train["WaveCurrentSpeedInWaveDirectionAtMeanWaterLevel"] = None

        return [wave_train]

    def _build_current_profile(
        self, profile: list[list[float]]
    ) -> list[list[float | int]]:
        """Build current profile in OrcaFlex format.

        Converts [[depth, factor], ...] to [[depth, factor, rotation], ...]
        where rotation is always 0 (no rotation from reference direction).

        OrcFxAPI requires NumberOfCurrentLevels >= 2. When only one level is
        provided, a second level is added at the seabed depth with the same
        factor.

        Args:
            profile: Current profile as [[depth, factor], ...]

        Returns:
            Profile in OrcaFlex format [[depth, factor, rotation], ...]
        """
        result = [[p[0], p[1], 0] for p in profile]
        if len(result) < 2:
            seabed_depth = self.spec.environment.water.depth
            last_factor = result[0][1] if result else 1.0
            if result and result[0][0] >= seabed_depth:
                # Single point at or below seabed — add surface point
                result.insert(0, [0, last_factor, 0])
            else:
                result.append([seabed_depth, last_factor, 0])
        return result
