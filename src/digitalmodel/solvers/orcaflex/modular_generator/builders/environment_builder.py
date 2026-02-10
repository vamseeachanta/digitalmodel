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

# Deterministic wave types use WaveHeight + WavePeriod
_DETERMINISTIC_WAVE_TYPES = {
    "Dean stream", "Airy", "Stokes' 5th", "Cnoidal",
    "dean_stream", "airy", "stokes_5th", "cnoidal",
}

# Spectral wave types use WaveHs + WaveTz
_SPECTRAL_WAVE_TYPES = {
    "JONSWAP", "Pierson-Moskowitz", "Torsethaugen", "Ochi-Hubble",
    "jonswap", "pierson_moskowitz",
}


@BuilderRegistry.register("03_environment.yml", order=30)
class EnvironmentBuilder(BaseBuilder):
    """Builds the Environment section of the OrcaFlex model.

    Uses hardcoded safe defaults as the base layer, then overlays
    spec-derived values (water depth, density, wave params, current, wind).

    Note:
        Raw pass-through of monolithic Environment properties was tested
        but causes "Change not allowed" errors due to OrcaFlex mode-dependent
        dormant properties.  The ``raw_properties`` field on the Environment
        schema is preserved for diagnostic use (semantic validation tool)
        but is NOT used by this builder.
    """

    # Default Environment dict used when no raw_properties are available.
    _DEFAULTS: dict[str, Any] = {
        "WaterSurfaceZ": 0,
        "KinematicViscosity": 1.35e-06,
        "SeaTemperature": 10,
        "ReynoldsNumberCalculation": "Flow direction",
        "HorizontalWaterDensityFactor": None,
        "VerticalDensityVariation": "Constant",
        "SeabedType": "Flat",
        "SeabedOrigin": [0, 0],
        "NominalDepth": None,
        "SeabedSlopeDirection": 180,
        "SeabedModel": "Elastic",
        "WaveKinematicsCutoffDepth": "Infinity",
        "WaveCalculationMethod": "Instantaneous position (exact)",
        "WaveCalculationTimeInterval": 0,
        "WaveCalculationSpatialInterval": 0,
        "MultipleCurrentDataCanBeDefined": "No",
        "CurrentModel": "Variation scheme",
        "CurrentRamped": "No",
        "CurrentApplyVerticalStretching": "No",
        "HorizontalCurrentFactor": None,
        "VerticalCurrentVariationMethod": "Interpolated",
        "IncludeVesselWindLoads": "Yes",
        "IncludeLineWindLoads": "Yes",
        "IncludeBuoyWindLoads": "Yes",
        "IncludeBuoyWingWindLoads": "Yes",
        "WindRamping": "From mean",
        "WindType": "Constant",
        "AirDensity": 0.00128,
        "AirSpeedOfSound": 343,
        "VerticalWindVariationFactor": None,
    }

    def build(self) -> dict[str, Any]:
        """Build the Environment section from environmental settings.

        Returns:
            Dictionary with 'Environment' key containing OrcaFlex settings.

        Note:
            The ``raw_properties`` on the Environment schema captures ALL
            original OrcaFlex properties for diagnostic use (semantic
            validation tool).  However, we do NOT pass raw properties into
            the generated YAML because OrcaFlex YAML exports contain
            mode-dependent dormant properties that cause "Change not allowed"
            errors when re-loaded in a different property order.  The builder
            uses hardcoded safe defaults instead.
        """
        env = self.spec.environment

        environment = dict(self._DEFAULTS)

        # Overlay spec-derived values (always authoritative)
        environment["Density"] = env.water.density
        environment["SeabedOriginDepth"] = env.water.depth
        environment["SeabedSlope"] = env.seabed.slope
        environment["SeabedNormalStiffness"] = env.seabed.stiffness.normal
        environment["SeabedShearStiffness"] = env.seabed.stiffness.shear

        # Wave trains
        environment["WaveTrains"] = self._build_wave_trains(env.waves)

        # Current
        environment["RefCurrentSpeed"] = env.current.speed
        environment["RefCurrentDirection"] = env.current.direction
        environment["CurrentDepth, CurrentFactor, CurrentRotation"] = (
            self._build_current_profile(env.current.profile)
        )

        # Wind
        environment["WindSpeed"] = env.wind.speed
        environment["WindDirection"] = env.wind.direction

        return {"Environment": environment}

    def _build_wave_trains(self, waves: Any) -> list[dict[str, Any]]:
        """Build wave train configuration from wave settings.

        Uses the correct OrcaFlex property names based on wave type:
        - Deterministic (Dean stream, Airy, etc.): WaveHeight + WavePeriod
        - Spectral (JONSWAP, Pierson-Moskowitz, etc.): WaveHs + WaveTz
        - Other (User defined, No waves, etc.): no height/period emitted

        Args:
            waves: Wave specification from the input spec.

        Returns:
            List containing wave train definitions.
        """
        # Map wave type to OrcaFlex enum value
        wave_type = WAVE_TYPE_MAP.get(waves.type, waves.type)

        wave_train: dict[str, Any] = {
            "Name": "Wave1",
            "WaveType": wave_type,
            "WaveDirection": waves.direction,
            "WaveOrigin": [0, 0],
            "WaveTimeOrigin": 0,
        }

        # Emit height/period with correct property names for the wave type
        if wave_type in _DETERMINISTIC_WAVE_TYPES:
            wave_train["WaveHeight"] = waves.height
            wave_train["WavePeriod"] = waves.period
        elif wave_type in _SPECTRAL_WAVE_TYPES:
            wave_train["WaveHs"] = waves.height
            wave_train["WaveTz"] = waves.period
            if wave_type in ("JONSWAP", "jonswap"):
                wave_train["WaveGamma"] = waves.gamma
        # For other wave types (User defined, User specified components,
        # No waves, etc.), omit height/period — OrcaFlex uses type-specific
        # parameters that pass through via generic properties.

        # Add wave-type specific parameters
        if waves.type == "dean_stream" or wave_type == "Dean stream":
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
