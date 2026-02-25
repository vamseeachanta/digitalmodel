# ABOUTME: Pydantic data models for dynacard analysis.
# ABOUTME: Includes surface card, rod string, pump, and analysis result structures.

from typing import List, Optional, Dict, Any
from pydantic import BaseModel, Field
import numpy as np
from .constants import (
    STEEL_YOUNGS_MODULUS_PSI,
    STEEL_DENSITY_LB_PER_FT3,
    WAVE_SPEED_FT_PER_S,
    DEFAULT_FLUID_DENSITY_LB_PER_FT3,
    DEFAULT_MINIMUM_TENSILE_STRENGTH_PSI,
    GRAVITATIONAL_ACCELERATION_SI,
    GRAVITATIONAL_ACCELERATION_FT_PER_S2,
    DENSITY_OF_WATER_SI_KG_PER_M3,
    DENSITY_OF_STEEL_SI_KG_PER_M3,
    DEFAULT_PRIME_MOVER_EFFICIENCY,
    DEFAULT_PUMPING_UNIT_EFFICIENCY,
    DEFAULT_PUMP_EFFICIENCY,
    DEFAULT_FD_NUM_NODES,
    SQUARE_FEET_TO_SQUARE_INCHES,
)


class CardData(BaseModel):
    """Raw position and load data for a dynamometer card."""
    position: List[float]
    load: List[float]
    timestamp: Optional[str] = None

    class Config:
        arbitrary_types_allowed = True


class RodSection(BaseModel):
    """Properties for a single section of the sucker rod string."""
    diameter: float  # inches
    length: float  # feet
    count: int = 1
    modulus_of_elasticity: float = STEEL_YOUNGS_MODULUS_PSI
    density: float = STEEL_DENSITY_LB_PER_FT3
    speed_of_sound: float = WAVE_SPEED_FT_PER_S
    damping_factor: float = 0.0
    weight_per_foot: float = 0.0  # lbs/ft - calculated if not provided
    minimum_tensile: float = DEFAULT_MINIMUM_TENSILE_STRENGTH_PSI
    grade: str = "KD"
    manufacturer: str = "Generic"

    def model_post_init(self, __context: Any) -> None:
        """Calculate weight per foot if not provided."""
        if self.weight_per_foot == 0.0:
            # Steel weight: pi * r^2 * density / 144 (convert sq in to sq ft)
            r_in = self.diameter / 2.0
            self.weight_per_foot = np.pi * (r_in ** 2) * self.density / SQUARE_FEET_TO_SQUARE_INCHES


class PumpProperties(BaseModel):
    """Downhole pump specifications."""
    diameter: float  # inches
    depth: float  # feet (pump setting depth)
    efficiency: float = DEFAULT_PUMP_EFFICIENCY


class SurfaceUnit(BaseModel):
    """Pumping unit surface equipment specifications."""
    manufacturer: str = "Generic"
    unit_type: str = "Conventional"
    stroke_length: float = 0.0  # inches
    gear_box_rating: float = 0.0  # in-lbs
    structural_imbalance: float = 0.0
    counterbalance_moment: float = 0.0
    geometry: str = "C. Clockwise"  # Clockwise or C. Clockwise
    dimensional_a: float = 0.0
    dimensional_c: float = 0.0
    dimensional_i: float = 0.0
    dimensional_k: float = 0.0
    dimensional_p: float = 0.0
    radius: float = 0.0
    phase_angle: float = 0.0  # tau - degrees
    beam_rating: float = 0.0  # structure rating in lbs
    max_stroke_length: float = 0.0  # inches


class WellTestData(BaseModel):
    """Well test production data."""
    oil_rate: float = 0.0  # BPD
    water_rate: float = 0.0  # BPD
    gas_rate: float = 0.0  # MCF/D
    water_cut: float = 0.0  # percent
    runtime: float = 24.0  # hours
    test_date: Optional[str] = None


class SurveyData(BaseModel):
    """Well survey/trajectory data."""
    measured_depth: List[float] = []
    inclination: List[float] = []
    azimuth: List[float] = []


class MotorProperties(BaseModel):
    """Rod pump motor specifications."""
    model: str = ""  # Motor model (e.g., "NEMA B", "NEMA D")
    horsepower: float = 0.0  # Rated horsepower
    voltage: float = 0.0  # Operating voltage
    manufacturer: str = ""


class CalculationParameters(BaseModel):
    """Solver configuration parameters."""
    acceleration_due_to_gravity: float = GRAVITATIONAL_ACCELERATION_SI  # m/s^2 (SI) or 32.174 ft/s^2
    density_of_water: float = DENSITY_OF_WATER_SI_KG_PER_M3  # kg/m^3 or 62.4 lbs/ft^3
    density_of_steel: float = DENSITY_OF_STEEL_SI_KG_PER_M3  # kg/m^3 or 490 lbs/ft^3
    coulomb_friction_coefficient: float = 0.0
    num_finite_difference_nodes: int = DEFAULT_FD_NUM_NODES
    remove_jump_value: bool = True
    use_si_units: bool = False  # False = oilfield units
    efficiency_prime_mover: float = DEFAULT_PRIME_MOVER_EFFICIENCY
    efficiency_pumping_unit: float = DEFAULT_PUMPING_UNIT_EFFICIENCY


class InputParameters(BaseModel):
    """Well operating parameters."""
    strokes_per_minute: float
    tubing_pressure: float = 0.0  # psi
    runtime: float = 24.0  # hours
    load_max_sp: Optional[float] = None
    load_min_sp: Optional[float] = None
    fluid_density: Optional[float] = None  # lbs/ft^3
    casing_pressure: Optional[float] = None  # psi
    # Load analysis parameters
    stroke_load_peak: Optional[float] = None  # actual peak load (lbs)
    normal_peak_load: Optional[float] = None  # expected peak load (lbs)
    stroke_load_min: Optional[float] = None  # actual min load (lbs)
    normal_min_load: Optional[float] = None  # expected min load (lbs)
    # Lift capacity parameters
    spm_24hr_avg: Optional[float] = None  # 24-hour average SPM
    assumed_pump_efficiency: float = DEFAULT_PUMP_EFFICIENCY  # for lift capacity calc


class DynacardAnalysisContext(BaseModel):
    """Complete context for a single well analysis."""
    api14: str
    surface_card: CardData
    rod_string: List[RodSection]
    pump: PumpProperties
    surface_unit: SurfaceUnit = Field(default_factory=SurfaceUnit)
    motor: MotorProperties = Field(default_factory=MotorProperties)
    spm: float  # strokes per minute
    fluid_density: float = DEFAULT_FLUID_DENSITY_LB_PER_FT3  # lbs/ft^3
    runtime: float = 24.0  # hours per day
    well_test: Optional[WellTestData] = None
    survey: Optional[SurveyData] = None
    input_params: Optional[InputParameters] = None
    calc_params: CalculationParameters = Field(default_factory=CalculationParameters)

    class Config:
        arbitrary_types_allowed = True

    @property
    def rod_length(self) -> float:
        """Total rod string length in feet."""
        return sum(s.length for s in self.rod_string)

    @property
    def rod_weight(self) -> float:
        """Total rod string weight in lbs."""
        return sum(s.weight_per_foot * s.length for s in self.rod_string)

    @property
    def pump_area(self) -> float:
        """Pump cross-sectional area in square inches."""
        return np.pi * (self.pump.diameter ** 2) / 4.0


class FluidLoadAnalysis(BaseModel):
    """Results from fluid load calculation."""
    fluid_load: float = 0.0  # lbs
    upstroke_load: float = 0.0  # lbs
    downstroke_load: float = 0.0  # lbs


class CPIPAnalysis(BaseModel):
    """Results from CPIP (Calculated Pump Intake Pressure) analysis."""
    pump_intake_pressure: float = 0.0  # psi
    pump_discharge_pressure: float = 0.0  # psi
    fluid_density: float = 0.0  # lbs/ft^3
    tubing_gradient: float = 0.0  # psi/ft


class PumpFillageAnalysis(BaseModel):
    """Results from pump fillage analysis."""
    gross_stroke: float = 0.0  # inches (max plunger travel)
    net_stroke: float = 0.0  # inches (effective plunger travel)
    fillage: float = 0.0  # percent
    corners: List[int] = []  # corner indices


class ProductionAnalysis(BaseModel):
    """Results from theoretical production calculation."""
    gross_displacement: float = 0.0  # BPD
    net_displacement: float = 0.0  # BPD
    theoretical_production: float = 0.0  # BPD
    pump_efficiency: float = 0.0  # percent


class TorqueStatistics(BaseModel):
    """Min/max torque statistics in M-in-lbs (thousands of inch-pounds)."""
    max_torque: float = 0.0
    min_torque: float = 0.0
    abs_max_torque: float = 0.0


class GearBoxLoadingAnalysis(BaseModel):
    """Results from gear box torque analysis per API 11E."""
    actual_torque: TorqueStatistics = Field(default_factory=TorqueStatistics)
    balanced_torque: TorqueStatistics = Field(default_factory=TorqueStatistics)
    actual_to_rated_percent: TorqueStatistics = Field(default_factory=TorqueStatistics)
    balanced_to_rated_percent: TorqueStatistics = Field(default_factory=TorqueStatistics)
    gear_box_rating: float = 0.0  # M-in-lbs
    actual_counterbalance_moment: float = 0.0  # M-in-lbs
    optimal_counterbalance_moment: float = 0.0  # M-in-lbs
    counterbalance_status: str = "not_calculated"  # converged/unconverged/not_calculated
    crank_angle: List[float] = []  # degrees
    actual_torque_curve: List[float] = []  # M-in-lbs at each sample
    balanced_torque_curve: List[float] = []  # M-in-lbs at each sample
    torque_factor: List[float] = []  # dimensionless


class PowerConsumptionAnalysis(BaseModel):
    """Results from power consumption analysis."""
    card_area: float = 0.0  # in-lbs (work per stroke)
    polished_rod_horsepower: float = 0.0  # HP
    prime_mover_horsepower: float = 0.0  # HP
    power_consumption_kw: float = 0.0  # kW
    daily_energy_consumption: float = 0.0  # kWh
    motor_design: str = "Others"  # "Mark II" or "Others"
    nema_code: str = "NEMA B"  # "NEMA B" or "NEMA D"
    cyclic_load_factor: float = 0.0  # F_CL


class LiftCapacityAnalysis(BaseModel):
    """Results from lift capacity calculation."""
    lift_capacity: float = 0.0  # BPD - maximum fluid lifting capability
    plunger_diameter: float = 0.0  # inches
    stroke_length: float = 0.0  # inches - downhole effective stroke
    spm_24hr_avg: float = 0.0  # strokes per minute (24-hour average)
    assumed_efficiency: float = 1.0  # pump efficiency factor


class LoadRatioAnalysis(BaseModel):
    """Results from load ratio analysis."""
    peak_load_ratio: float = 0.0  # actual peak / normal peak
    low_load_ratio: float = 0.0  # actual min / normal min
    actual_peak_load: float = 0.0  # lbs
    normal_peak_load: float = 0.0  # lbs
    actual_min_load: float = 0.0  # lbs
    normal_min_load: float = 0.0  # lbs


class CardGeometryAnalysis(BaseModel):
    """Results from card geometry analysis."""
    area: float = 0.0  # in-lbs (position*load units)
    perimeter: float = 0.0  # combined units
    position_range: float = 0.0  # inches (max - min position)
    load_range: float = 0.0  # lbs (max - min load)
    centroid_position: float = 0.0  # inches
    centroid_load: float = 0.0  # lbs
    # Zoned area analysis (quadrant distribution)
    zone_areas: List[float] = []  # [bottom_left, bottom_right, top_left, top_right]
    zone_area_fractions: List[float] = []  # normalized to total area


class RodBucklingAnalysis(BaseModel):
    """Results from rod buckling analysis."""
    # Buckling detection flags
    sinusoidal_buckling_detected: bool = False
    helical_buckling_detected: bool = False
    # Neutral point (transition from tension to compression)
    neutral_point_depth: float = 0.0  # feet from surface
    neutral_point_fraction: float = 0.0  # fraction of rod string length
    # Buckling severity metrics
    max_compressive_load: float = 0.0  # lbs (most negative load)
    compression_depth_start: float = 0.0  # feet where compression begins
    compression_length: float = 0.0  # feet of rod in compression
    # Critical buckling loads (calculated thresholds)
    sinusoidal_critical_load: float = 0.0  # lbs - threshold for sinusoidal buckling
    helical_critical_load: float = 0.0  # lbs - threshold for helical buckling
    # Buckling tendency (effective axial load considering buoyancy)
    min_buckling_tendency: float = 0.0  # lbs (most compressive)
    max_buckling_tendency: float = 0.0  # lbs (most tensile)
    # Analysis metadata
    analysis_method: str = "simplified"  # "simplified" or "full_simulation"
    warning_message: str = ""


class TorqueBalanceAnalysis(BaseModel):
    """Results from torque balance optimization analysis."""
    # Analytical counterbalance estimate
    estimated_optimal_moment: float = 0.0  # M-in-lbs (analytical estimate)
    actual_counterbalance_moment: float = 0.0  # M-in-lbs (current setting)
    # Objective function values
    objective_at_actual: float = 0.0  # |max_up - max_down| at actual M
    objective_at_optimal: float = 0.0  # |max_up - max_down| at optimal M
    # Peak torque values
    upstroke_peak_torque: float = 0.0  # M-in-lbs (max torque on upstroke)
    downstroke_peak_torque: float = 0.0  # M-in-lbs (max torque on downstroke)
    peak_torque_difference: float = 0.0  # |upstroke - downstroke|
    # Geometry parameters computed
    stroke_transition_index: int = 0  # index where upstroke ends
    rotation_direction: int = 1  # +1 CW, -1 CCW
    # Analysis metadata
    analysis_method: str = "analytical"  # "analytical" or "iterative"
    warning_message: str = ""


class IdealCardAnalysis(BaseModel):
    """Results from ideal/reference card generation."""
    # Ideal surface card
    ideal_surface_position: List[float] = []  # inches
    ideal_surface_load: List[float] = []  # lbs
    # Ideal pump/downhole card
    ideal_pump_position: List[float] = []  # inches
    ideal_pump_load: List[float] = []  # lbs
    # Card metrics for comparison
    ideal_peak_load: float = 0.0  # lbs
    ideal_min_load: float = 0.0  # lbs
    ideal_stroke_length: float = 0.0  # inches
    ideal_fluid_load: float = 0.0  # lbs
    ideal_card_area: float = 0.0  # in-lbs (work per cycle)
    # Deviation from measured card
    load_deviation_rms: float = 0.0  # lbs (RMS difference)
    position_deviation_rms: float = 0.0  # inches (RMS difference)
    shape_similarity: float = 0.0  # 0-1 (1 = identical)
    # Simulation parameters
    fillage_assumed: float = 1.0  # 0-1 (pump fillage)
    damping_coefficient: float = 0.1  # damping used
    num_time_points: int = 0  # number of simulation points
    # Analysis metadata
    generation_method: str = "simplified"  # "simplified" or "full_simulation"
    warning_message: str = ""


class DiagnosticResult(BaseModel):
    """Result from ML-based dynacard diagnostic classification."""
    classification: str = "NORMAL"
    confidence: float = 0.0
    differential: List[Dict[str, Any]] = []
    feature_importances: Optional[Dict[str, float]] = None
    model_version: str = "1.0"


class AnalysisResults(BaseModel):
    """Output results from the dynacard analysis."""
    ctx: Optional[DynacardAnalysisContext] = None
    downhole_card: Optional[CardData] = None
    peak_polished_rod_load: float = 0.0
    minimum_polished_rod_load: float = 0.0

    # P1 Analysis Results
    fluid_load: Optional[FluidLoadAnalysis] = None
    cpip: Optional[CPIPAnalysis] = None
    fillage: Optional[PumpFillageAnalysis] = None
    production: Optional[ProductionAnalysis] = None

    # P2 Analysis Results
    gear_box: Optional[GearBoxLoadingAnalysis] = None
    power_consumption: Optional[PowerConsumptionAnalysis] = None

    # P3 Analysis Results
    lift_capacity: Optional[LiftCapacityAnalysis] = None
    load_ratios: Optional[LoadRatioAnalysis] = None

    # P4 Analysis Results
    card_geometry: Optional[CardGeometryAnalysis] = None

    # P5 Analysis Results
    rod_buckling: Optional[RodBucklingAnalysis] = None

    # P6 Analysis Results
    ideal_card: Optional[IdealCardAnalysis] = None

    # P7 Analysis Results
    torque_balance: Optional[TorqueBalanceAnalysis] = None

    # Legacy compatibility
    pump_fillage: float = 0.0
    inferred_production: float = 0.0
    buckling_detected: bool = False
    diagnostic_message: str = "Analysis not yet performed"

    # Solver metadata
    solver_method: str = "gibbs"  # "gibbs" or "finite_difference"

    class Config:
        arbitrary_types_allowed = True
