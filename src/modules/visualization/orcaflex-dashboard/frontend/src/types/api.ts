// API Types for OrcaFlex Dashboard
// Comprehensive TypeScript types matching backend API

// Base response types
export interface ApiResponse<T = any> {
  data: T;
  message?: string;
  success: boolean;
  timestamp: string;
}

export interface PaginatedResponse<T> extends ApiResponse<T[]> {
  pagination: {
    page: number;
    limit: number;
    total: number;
    pages: number;
  };
}

export interface ErrorResponse {
  error: string;
  message: string;
  details?: Record<string, any>;
  timestamp: string;
}

// Analysis types
export interface Analysis {
  id: string;
  name: string;
  description?: string;
  model_file_path: string;
  status: AnalysisStatus;
  progress: number;
  created_at: string;
  updated_at: string;
  completed_at?: string;
  error_message?: string;
  configuration: AnalysisConfiguration;
  metadata: AnalysisMetadata;
}

export type AnalysisStatus = 
  | 'created'
  | 'queued'
  | 'running'
  | 'completed'
  | 'failed'
  | 'cancelled';

export interface AnalysisConfiguration {
  simulation_time: number;
  time_step: number;
  wave_conditions?: WaveConditions;
  current_conditions?: CurrentConditions;
  wind_conditions?: WindConditions;
  solver_settings: SolverSettings;
  output_settings: OutputSettings;
}

export interface WaveConditions {
  height: number;
  period: number;
  direction: number;
  spectrum_type: 'JONSWAP' | 'PM' | 'Pierson-Moskowitz';
  gamma?: number;
}

export interface CurrentConditions {
  surface_speed: number;
  direction: number;
  profile_type: 'uniform' | 'linear' | 'power_law';
  seabed_speed?: number;
  reference_depth?: number;
}

export interface WindConditions {
  speed: number;
  direction: number;
  reference_height: number;
  profile_type: 'constant' | 'power_law' | 'logarithmic';
}

export interface SolverSettings {
  implicit_constant: number;
  target_log_decrement: number;
  minimum_damping: number;
  maximum_damping: number;
  variable_time_step: boolean;
}

export interface OutputSettings {
  output_interval: number;
  results_to_calculate: string[];
  components_to_monitor: string[];
}

export interface AnalysisMetadata {
  model_type: string;
  total_components: number;
  simulation_duration: number;
  file_size_mb: number;
  water_depth?: number;
  environment_type: 'offshore' | 'nearshore' | 'deep_water';
  tags: string[];
}

// Results types
export interface AnalysisResults {
  analysis_id: string;
  result_type: ResultType;
  component_results: ComponentResult[];
  summary_statistics: SummaryStatistics;
  extreme_values: ExtremeValues;
  fatigue_analysis?: FatigueAnalysis;
  frequency_domain?: FrequencyDomainResults;
  time_series: TimeSeriesResult[];
  generated_at: string;
}

export type ResultType = 
  | 'static'
  | 'dynamic'
  | 'frequency_domain'
  | 'fatigue'
  | 'extreme_response';

export interface ComponentResult {
  component_id: string;
  component_name: string;
  component_type: ComponentType;
  results: {
    tension?: TensionResult;
    curvature?: CurvatureResult;
    displacement?: DisplacementResult;
    velocity?: VelocityResult;
    acceleration?: AccelerationResult;
    force?: ForceResult;
    moment?: MomentResult;
  };
  statistics: ComponentStatistics;
}

export interface TensionResult {
  effective: number[];
  wall: number[];
  true: number[];
  units: string;
}

export interface CurvatureResult {
  x_direction: number[];
  y_direction: number[];
  total: number[];
  units: string;
}

export interface DisplacementResult {
  x: number[];
  y: number[];
  z: number[];
  units: string;
}

export interface VelocityResult {
  x: number[];
  y: number[];
  z: number[];
  units: string;
}

export interface AccelerationResult {
  x: number[];
  y: number[];
  z: number[];
  units: string;
}

export interface ForceResult {
  x: number[];
  y: number[];
  z: number[];
  units: string;
}

export interface MomentResult {
  x: number[];
  y: number[];
  z: number[];
  units: string;
}

export interface ComponentStatistics {
  max_values: Record<string, number>;
  min_values: Record<string, number>;
  mean_values: Record<string, number>;
  std_dev: Record<string, number>;
  rms_values: Record<string, number>;
}

export interface SummaryStatistics {
  peak_tensions: Record<string, number>;
  maximum_displacements: Record<string, number>;
  critical_curvatures: Record<string, number>;
  utilization_ratios: Record<string, number>;
}

export interface ExtremeValues {
  component_id: string;
  extreme_tensions: ExtremeValue[];
  extreme_curvatures: ExtremeValue[];
  extreme_displacements: ExtremeValue[];
}

export interface ExtremeValue {
  value: number;
  timestamp: number;
  location?: number;
  units: string;
}

export interface FatigueAnalysis {
  component_id: string;
  fatigue_life: number;
  damage_ratio: number;
  cycles_to_failure: number;
  stress_ranges: number[];
  cycle_counts: number[];
  safety_factor: number;
}

export interface FrequencyDomainResults {
  frequencies: number[];
  response_amplitude_operators: Record<string, number[]>;
  power_spectral_densities: Record<string, number[]>;
  transfer_functions: Record<string, ComplexNumber[]>;
}

export interface ComplexNumber {
  real: number;
  imaginary: number;
  magnitude: number;
  phase: number;
}

export interface TimeSeriesResult {
  timestamp: number[];
  component_id: string;
  result_type: string;
  values: number[];
  units: string;
}

// Component types
export interface Component {
  id: string;
  name: string;
  type: ComponentType;
  properties: ComponentProperties;
  position: Position3D;
  orientation: Orientation3D;
  connections: Connection[];
  material: Material;
  cross_section?: CrossSection;
  boundary_conditions: BoundaryCondition[];
  load_conditions: LoadCondition[];
}

export type ComponentType =
  | 'line'
  | 'vessel'
  | 'winch'
  | 'attachment'
  | 'constraint'
  | 'support'
  | 'buoy'
  | '6dbuoy'
  | 'shape';

export interface ComponentProperties {
  length?: number;
  mass_per_unit_length?: number;
  outer_diameter?: number;
  inner_diameter?: number;
  wall_thickness?: number;
  elastic_modulus?: number;
  tensile_strength?: number;
  yield_strength?: number;
  fatigue_capacity?: number;
}

export interface Position3D {
  x: number;
  y: number;
  z: number;
}

export interface Orientation3D {
  x: number;
  y: number;
  z: number;
}

export interface Connection {
  id: string;
  connected_to: string;
  connection_type: 'fixed' | 'pinned' | 'universal' | 'ball_joint';
  stiffness?: Stiffness6DOF;
  damping?: Damping6DOF;
}

export interface Stiffness6DOF {
  x: number;
  y: number;
  z: number;
  rx: number;
  ry: number;
  rz: number;
}

export interface Damping6DOF {
  x: number;
  y: number;
  z: number;
  rx: number;
  ry: number;
  rz: number;
}

export interface Material {
  name: string;
  density: number;
  elastic_modulus: number;
  poisson_ratio: number;
  yield_strength: number;
  ultimate_strength: number;
  fatigue_properties?: FatigueProperties;
}

export interface FatigueProperties {
  s_n_curve: SNPoint[];
  endurance_limit: number;
  fatigue_factor: number;
}

export interface SNPoint {
  stress: number;
  cycles: number;
}

export interface CrossSection {
  type: 'circular' | 'rectangular' | 'i_beam' | 'custom';
  outer_diameter?: number;
  inner_diameter?: number;
  wall_thickness?: number;
  width?: number;
  height?: number;
  area: number;
  moment_of_inertia: {
    ixx: number;
    iyy: number;
    izz: number;
  };
}

export interface BoundaryCondition {
  type: 'fixed' | 'pinned' | 'free' | 'spring' | 'damper';
  degrees_of_freedom: {
    x: boolean;
    y: boolean;
    z: boolean;
    rx: boolean;
    ry: boolean;
    rz: boolean;
  };
  values?: number[];
}

export interface LoadCondition {
  type: 'force' | 'moment' | 'pressure' | 'distributed';
  magnitude: number;
  direction: Position3D;
  application_point?: Position3D;
  time_variation?: 'constant' | 'harmonic' | 'random' | 'time_series';
}

// Request types
export interface CreateAnalysisRequest {
  name: string;
  description?: string;
  model_file_path: string;
  configuration: AnalysisConfiguration;
  metadata?: Partial<AnalysisMetadata>;
}

export interface UpdateAnalysisRequest {
  name?: string;
  description?: string;
  configuration?: Partial<AnalysisConfiguration>;
  metadata?: Partial<AnalysisMetadata>;
}

export interface RunAnalysisRequest {
  priority?: 'low' | 'normal' | 'high';
  notifications?: NotificationSettings;
}

export interface NotificationSettings {
  email_on_completion: boolean;
  email_on_failure: boolean;
  webhook_url?: string;
}

export interface GetAnalysesRequest {
  page?: number;
  limit?: number;
  status?: AnalysisStatus[];
  created_after?: string;
  created_before?: string;
  search?: string;
  sort_by?: 'created_at' | 'updated_at' | 'name';
  sort_order?: 'asc' | 'desc';
}

export interface GetResultsRequest {
  analysis_id: string;
  result_types?: ResultType[];
  component_ids?: string[];
  start_time?: number;
  end_time?: number;
  include_time_series?: boolean;
  include_statistics?: boolean;
  include_extremes?: boolean;
}

export interface ExportResultsRequest {
  analysis_id: string;
  format: 'csv' | 'excel' | 'json' | 'hdf5';
  components?: string[];
  result_types?: string[];
  time_range?: {
    start: number;
    end: number;
  };
  include_metadata?: boolean;
}

// WebSocket types
export interface WebSocketMessage {
  type: WebSocketMessageType;
  payload: any;
  timestamp: string;
}

export type WebSocketMessageType =
  | 'analysis_status_update'
  | 'analysis_progress_update'
  | 'results_available'
  | 'error'
  | 'connection_status';

export interface AnalysisStatusUpdate {
  analysis_id: string;
  status: AnalysisStatus;
  progress?: number;
  message?: string;
  error?: string;
}

export interface AnalysisProgressUpdate {
  analysis_id: string;
  progress: number;
  current_step: string;
  estimated_completion?: string;
}

export interface ResultsAvailable {
  analysis_id: string;
  result_types: ResultType[];
  file_size?: number;
}

// Chart and visualization types
export interface ChartData {
  labels: string[];
  datasets: Dataset[];
}

export interface Dataset {
  label: string;
  data: number[];
  backgroundColor?: string | string[];
  borderColor?: string | string[];
  borderWidth?: number;
  fill?: boolean;
  tension?: number;
  pointRadius?: number;
  pointHoverRadius?: number;
}

export interface ChartOptions {
  responsive: boolean;
  maintainAspectRatio: boolean;
  plugins: {
    legend: {
      display: boolean;
      position?: 'top' | 'bottom' | 'left' | 'right';
    };
    tooltip: {
      enabled: boolean;
      mode?: 'index' | 'nearest' | 'point';
    };
  };
  scales?: {
    x?: ScaleOptions;
    y?: ScaleOptions;
  };
}

export interface ScaleOptions {
  display: boolean;
  title: {
    display: boolean;
    text: string;
  };
  grid?: {
    display: boolean;
    color?: string;
  };
  ticks?: {
    callback?: (value: any, index: number, values: any[]) => string;
  };
}

// Filter and search types
export interface FilterOptions {
  status: AnalysisStatus[];
  dateRange: {
    start?: string;
    end?: string;
  };
  modelType: string[];
  tags: string[];
  resultTypes: ResultType[];
}

export interface SearchOptions {
  query: string;
  fields: string[];
  fuzzy: boolean;
  caseSensitive: boolean;
}

export interface SortOptions {
  field: string;
  direction: 'asc' | 'desc';
}

// Utility types
export type LoadingState = 'idle' | 'loading' | 'succeeded' | 'failed';

export interface AsyncState<T> {
  data: T | null;
  loading: LoadingState;
  error: string | null;
  lastUpdated: string | null;
}

export interface CacheOptions {
  ttl?: number; // Time to live in milliseconds
  invalidateOnUpdate?: boolean;
  persistToStorage?: boolean;
}