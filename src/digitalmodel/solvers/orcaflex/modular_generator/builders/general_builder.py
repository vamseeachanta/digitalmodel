"""Single ordered owner of General settings; explicit source precedes fallbacks."""
import logging
from typing import Any

from .base import BaseBuilder
from .registry import BuilderRegistry

logger = logging.getLogger(__name__)

# General-section keys to skip when emitting general_properties.
# These are view/display/cosmetic properties that OrcaFlex exports via
# SaveData() but which can be dormant (not settable) depending on the
# current view mode.  Setting a dormant property triggers a
# "Change not allowed" error at load time.
_SKIP_GENERAL_KEYS: set[str] = {
    # Default view settings
    "DefaultViewAngle1",
    "DefaultViewAngle2",
    "DefaultViewCentre",
    "DefaultViewSize",
    "DefaultViewOrientation",
    "DefaultViewResetWhenConnectedObjectMoved",
    "DefaultViewDistortionX",
    "DefaultViewDistortionY",
    "DefaultViewDistortionZ",
    "DefaultViewAzimuth",
    "DefaultViewElevation",
    "DefaultViewMode",
    # Default shaded view settings (dormant unless view mode is Shaded)
    "DefaultShadedFillMode",
    "DefaultShadedProjectionMode",
    # Drawing cosmetics
    "BackgroundColour",
    "WireframeMode",
    # Sea surface / seabed rendering
    "SeaSurfaceTranslucency",
    "SeabedTranslucency",
    "SeaSurfaceGridDensity",
    "SeabedGridDensity",
    "SeaSurfacePen",
    # Model state bookkeeping
    "ModelState",
    # Temperature units — display-only, encoding of degree symbol (°)
    # differs between UTF-8 and Latin-1 causing OrcFxAPI "not found" errors
    "TemperatureUnits",
    # Variable time-step max — only settable when ImplicitUseVariableTimeStep
    # is True; dormant otherwise, causing "Change not allowed" errors.
    # NOT skipped when general_properties re-enables variable time stepping
    # (see build()) — stripping it then would silently revert the model to
    # OrcaFlex's default max step and change dynamics results.
    "ImplicitVariableMaxTimeStep",
}


def _defaults(sim):
    defaults = {
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
        "RestartStateRecordingTest": "",
    }
    defaults['UnitsSystem'] = 'SI'
    if sim.north_direction:
        defaults['NorthDirection'] = sim.north_direction
    return {'UnitsSystem': defaults.pop('UnitsSystem'), **defaults}


def _insert_fallbacks(source, defaults):
    keys = list(source)
    order = list(defaults)
    for index, key in enumerate(order):
        if key not in source:
            following = next((item for item in order[index + 1:] if item in keys), None)
            keys.insert(keys.index(following) if following else len(keys), key)
    return {key: source[key] if key in source else defaults[key] for key in keys}


def _source_settings(spec):
    generic = getattr(spec, 'generic', None)
    source = dict(generic.general_properties) if generic and generic.general_properties else {}
    flag = source.get('ImplicitUseVariableTimeStep', False)
    if type(flag) is not bool:
        raise ValueError('ImplicitUseVariableTimeStep requires a Boolean')
    skip = _SKIP_GENERAL_KEYS - {'ImplicitVariableMaxTimeStep'} if flag else _SKIP_GENERAL_KEYS
    source = {key: value for key, value in source.items() if key not in skip}
    typed = dict(ImplicitConstantTimeStep=spec.simulation.time_step,
                 TargetLogSampleInterval=spec.simulation.time_step,
                 StageDuration=list(spec.simulation.stages), NorthDirection=spec.simulation.north_direction)
    for key, value in typed.items():
        if key in source and source[key] != value:
            logger.warning('General %s explicit source overrides typed simulation fallback', key)
    return source


def _order_controls(general):
    units = ['UnitsSystem']
    if general['UnitsSystem'] != 'SI':
        units += [key for key in ('LengthUnits', 'MassUnits', 'ForceUnits', 'g') if key in general]
    flag = general['ImplicitUseVariableTimeStep']
    active = 'ImplicitVariableMaxTimeStep' if flag else 'ImplicitConstantTimeStep'
    inactive = 'ImplicitConstantTimeStep' if flag else 'ImplicitVariableMaxTimeStep'
    general.pop(inactive, None)
    keys = units + [key for key in general if key not in units and key != active]
    if active in general:
        keys.insert(keys.index('ImplicitUseVariableTimeStep') + 1, active)
    return {key: general[key] for key in keys}


@BuilderRegistry.register('01_general.yml', order=10)
class GeneralBuilder(BaseBuilder):
    """Emit source controls once, before Environment and object includes."""

    def build(self) -> dict[str, Any]:
        source = _source_settings(self.spec)
        general = _insert_fallbacks(source, _defaults(self.spec.simulation))
        return {'General': _order_controls(general)}
