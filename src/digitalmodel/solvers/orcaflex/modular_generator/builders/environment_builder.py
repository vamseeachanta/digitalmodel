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
        "SeabedSlopeDirection": 0,
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

    # Safe properties that can be overlaid from raw_properties without
    # triggering "Change not allowed" errors.  These are mode-independent
    # scalar or list properties that exist regardless of wind/wave type.
    _SAFE_RAW_OVERLAY_KEYS: set[str] = {
        "WaterSurfaceZ",
        "KinematicViscosity",
        "SeaTemperature",
        "ReynoldsNumberCalculation",
        "HorizontalWaterDensityFactor",
        "VerticalDensityVariation",
        "SeabedType",
        "SeabedOrigin",
        "WaterDepth",
        "SeabedSlopeDirection",
        "SeabedSlope",
        "SeabedModel",
        "SeabedNormalStiffness",
        "SeabedShearStiffness",
        "KinematicStretchingMethod",
        "UserSpecifiedRandomWaveSeeds",
        "WaveFrequencySpectrumDiscretisationMethod",
        "WaveKinematicsCutoffDepth",
        "WaveCalculationMethod",
        "WaveCalculationTimeInterval",
        "WaveCalculationSpatialInterval",
        "MultipleCurrentDataCanBeDefined",
        "CurrentModel",
        "CurrentRamped",
        "CurrentApplyVerticalStretching",
        "HorizontalCurrentFactor",
        "VerticalCurrentVariationMethod",
        "IncludeVesselWindLoads",
        "IncludeLineWindLoads",
        "IncludeBuoyWindLoads",
        "IncludeBuoyWingWindLoads",
        "WindRamping",
        "AirDensity",
        "AirSpeedOfSound",
    }

    # Wind-type-dependent properties keyed by WindType value.
    # Only emitted when the detected wind type matches.
    _WIND_TYPE_PROPS: dict[str, set[str]] = {
        "API spectrum": {
            "WindSpectrumElevation",
            "WindSpectrumFMin",
            "WindSpectrumFMax",
            "WindTimeOrigin",
            "WindSeed",
            "NumberOfWindComponents",
        },
        "NPD spectrum": {
            "WindSpectrumElevation",
            "WindSpectrumFMin",
            "WindSpectrumFMax",
            "WindTimeOrigin",
            "WindSeed",
            "NumberOfWindComponents",
        },
        "ESDU spectrum": {
            "WindSpectrumElevation",
            "WindSpectrumFMin",
            "WindSpectrumFMax",
            "WindTimeOrigin",
            "WindSeed",
            "NumberOfWindComponents",
        },
    }

    def build(self) -> dict[str, Any]:
        """Build the Environment section from environmental settings.

        Returns:
            Dictionary with 'Environment' key containing OrcaFlex settings.

        Strategy:
            1. Start with hardcoded safe defaults.
            2. Overlay mode-independent properties from raw_properties (if
               available) using _SAFE_RAW_OVERLAY_KEYS whitelist.
            3. Detect WindType from raw_properties and emit wind-type-
               dependent properties from _WIND_TYPE_PROPS.
            4. Apply spec-derived values (always authoritative) last.
        """
        env = self.spec.environment
        raw = getattr(env, "raw_properties", {}) or {}

        environment = dict(self._DEFAULTS)

        # Step 2: Overlay safe mode-independent raw properties
        for key in self._SAFE_RAW_OVERLAY_KEYS:
            if key in raw:
                environment[key] = raw[key]

        # Step 3: Detect and apply wind type from raw_properties
        wind_type = raw.get("WindType", "Constant")
        environment["WindType"] = wind_type
        wind_dep_keys = self._WIND_TYPE_PROPS.get(wind_type, set())
        for key in wind_dep_keys:
            if key in raw:
                environment[key] = raw[key]

        # Step 4: Overlay spec-derived values (always authoritative)
        environment["Density"] = env.water.density
        # Use WaterDepth if raw has it (extracted models), else SeabedOriginDepth
        # (hand-crafted specs) for backward compatibility.
        if "WaterDepth" in raw:
            environment["WaterDepth"] = env.water.depth
            # Remove NominalDepth default when WaterDepth is used
            environment.pop("NominalDepth", None)
        else:
            environment["SeabedOriginDepth"] = env.water.depth
        environment["SeabedSlope"] = env.seabed.slope
        environment["SeabedNormalStiffness"] = env.seabed.stiffness.normal
        environment["SeabedShearStiffness"] = env.seabed.stiffness.shear

        # Wave trains
        environment["WaveTrains"] = self._build_wave_trains(env.waves, raw)

        # Current — mode depends on VerticalCurrentVariationMethod
        current_method = environment.get(
            "VerticalCurrentVariationMethod", "Interpolated"
        )
        if current_method == "Power law":
            # Power law mode: emit surface/seabed speeds and exponent.
            # These props are only valid in Power law mode (dormant in
            # Interpolated mode), so overlay them conditionally here.
            for key in ("CurrentExponent", "CurrentSpeedAtSurface",
                        "CurrentSpeedAtSeabed"):
                if key in raw:
                    environment[key] = raw[key]
            environment["RefCurrentDirection"] = env.current.direction
            # Do NOT emit RefCurrentSpeed, CurrentDepth/Factor/Rotation
            # — they are Interpolated-mode properties and will cause
            # "Change not allowed" errors in Power law mode.
        else:
            # Interpolated mode (default): emit tabular profile
            environment["RefCurrentSpeed"] = env.current.speed
            environment["RefCurrentDirection"] = env.current.direction
            environment["CurrentDepth, CurrentFactor, CurrentRotation"] = (
                self._build_current_profile(env.current.profile)
            )

        # Wind
        environment["WindSpeed"] = env.wind.speed
        environment["WindDirection"] = env.wind.direction

        return {"Environment": environment}

    def _build_wave_trains(
        self, waves: Any, raw: dict[str, Any] | None = None
    ) -> list[dict[str, Any]]:
        """Build wave train configuration from wave settings.

        Uses the correct OrcaFlex property names based on wave type:
        - Deterministic (Dean stream, Airy, etc.): WaveHeight + WavePeriod
        - Spectral (JONSWAP, Pierson-Moskowitz, etc.): WaveHs + WaveTz
        - Other (User defined, No waves, etc.): no height/period emitted

        When raw_properties contain WaveTrains, the raw wave train data is
        used as the base layer to preserve all original properties (Name,
        spectral discretisation params, etc.), with spec-derived values
        overlaid on top.

        Args:
            waves: Wave specification from the input spec.
            raw: Raw environment properties dict (optional).

        Returns:
            List containing wave train definitions.
        """
        raw = raw or {}

        # Map wave type to OrcaFlex enum value
        wave_type = WAVE_TYPE_MAP.get(waves.type, waves.type)

        # If raw WaveTrains exist, use first train as base layer
        raw_trains = raw.get("WaveTrains", [])
        if raw_trains and isinstance(raw_trains, list) and raw_trains:
            wave_train = dict(raw_trains[0])
        else:
            wave_train = {
                "Name": "Wave 1",
                "WaveOrigin": [0, 0],
                "WaveTimeOrigin": 0,
            }

        # Always overlay spec-derived values
        wave_train["WaveType"] = wave_type
        wave_train["WaveDirection"] = waves.direction

        # Emit height/period with correct property names for the wave type
        if wave_type in _DETERMINISTIC_WAVE_TYPES:
            wave_train["WaveHeight"] = waves.height
            wave_train["WavePeriod"] = waves.period
        elif wave_type in _SPECTRAL_WAVE_TYPES:
            wave_train["WaveHs"] = waves.height
            wave_train["WaveTz"] = waves.period
            if wave_type in ("JONSWAP", "jonswap"):
                # WaveGamma is dormant when WaveJONSWAPParameters is "Automatic"
                jonswap_params = wave_train.get("WaveJONSWAPParameters", "")
                if jonswap_params != "Automatic":
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
