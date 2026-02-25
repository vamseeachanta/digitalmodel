// Time Series Data Types

export interface TimeSeriesData {
  time: number;
  [componentId: string]: number | undefined;
}

export interface TimeRange {
  start: number;
  end: number;
}

export interface ComponentGroup {
  id: string;
  name: string;
  type: 'struts' | 'jackets' | 'foundations' | 'default';
  componentIds: string[];
  color?: string;
  description?: string;
}

export interface StatisticalOverlay {
  componentId: string;
  label: string;
  value: number;
  type: 'mean' | 'stdDev' | 'rms' | 'min' | 'max' | 'p95' | 'p99' | 'median';
  color?: string;
}

export interface StatisticalSummary {
  componentId: string;
  count: number;
  mean: number;
  stdDev: number;
  variance: number;
  min: number;
  max: number;
  median: number;
  rms: number;
  p95: number;
  p99: number;
  skewness: number;
  kurtosis: number;
  range: number;
  cv: number; // Coefficient of variation
  [key: string]: number | string;
}

// Time Series Controls State
export interface TimeSeriesControlsState {
  selectedComponents: string[];
  showStatistics: boolean;
  showGrid: boolean;
  enableZoom: boolean;
  syncCharts: boolean;
  decimationFactor: number;
  chartHeight: number;
  statisticalTypes: string[];
  yAxisLabel: string;
  xAxisLabel: string;
}

// Chart Instance for Multi-Panel
export interface ChartInstance {
  id: string;
  title: string;
  selectedComponents: string[];
  yAxisLabel: string;
  xAxisLabel?: string;
  showStatistics: boolean;
  statisticalTypes: string[];
  timeRange?: TimeRange;
  decimationFactor?: number;
  syncId?: string;
}

// Multi Time Series Configuration
export interface MultiTimeSeriesConfig {
  charts: ChartInstance[];
  layout: 'grid' | 'column';
  synchronized: boolean;
  globalTimeRange: TimeRange | null;
  showMasterControls: boolean;
  decimationFactor: number;
}

// FFT Analysis Types
export interface FFTResult {
  frequencies: number[];
  magnitudes: number[];
  phases: number[];
  powerSpectralDensity: number[];
  dominantFrequency: number;
  totalPower: number;
}

export interface FrequencyDomainAnalysis {
  componentId: string;
  fftResult: FFTResult;
  windowFunction: 'hanning' | 'hamming' | 'blackman' | 'rectangular';
  samplingRate: number;
  nfft?: number;
  overlap?: number;
}

// Export Configuration
export interface ExportConfig {
  format: 'png' | 'svg' | 'pdf' | 'csv';
  resolution: number;
  width: number;
  height: number;
  includeData: boolean;
  includeStatistics: boolean;
  title?: string;
  subtitle?: string;
  watermark?: string;
}

// Data Processing Options
export interface DataProcessingOptions {
  decimationFactor: number;
  smoothingWindow?: number;
  filterType?: 'lowpass' | 'highpass' | 'bandpass' | 'notch';
  filterFrequency?: number | [number, number];
  removeOutliers?: boolean;
  outlierThreshold?: number; // Standard deviations
  interpolateGaps?: boolean;
}

// Performance Monitoring
export interface PerformanceMetrics {
  dataPoints: number;
  renderTime: number;
  memoryUsage: number;
  decimationApplied: boolean;
  lastUpdateTime: number;
}

// Animation State
export interface AnimationState {
  isPlaying: boolean;
  currentTime: number;
  playbackSpeed: number;
  loop: boolean;
  startTime: number;
  endTime: number;
}

// Cursor and Crosshair
export interface CursorState {
  active: boolean;
  time: number;
  values: { [componentId: string]: number };
  position: { x: number; y: number };
}

// Zoom State
export interface ZoomState {
  level: number;
  center: { time: number; value: number };
  history: TimeRange[];
  maxHistory: number;
}

// Chart Theme
export interface ChartTheme {
  backgroundColor: string;
  gridColor: string;
  textColor: string;
  lineColors: string[];
  highlightColor: string;
  selectionColor: string;
  fonts: {
    title: string;
    axis: string;
    legend: string;
  };
}

// Real-time Data Stream
export interface DataStream {
  id: string;
  name: string;
  isActive: boolean;
  latestTimestamp: number;
  updateFrequency: number; // Hz
  bufferSize: number;
  componentIds: string[];
}

// Alert/Threshold Configuration
export interface ThresholdAlert {
  id: string;
  componentId: string;
  type: 'above' | 'below' | 'outside' | 'inside';
  value: number | [number, number];
  severity: 'info' | 'warning' | 'error' | 'critical';
  message: string;
  isActive: boolean;
  lastTriggered?: number;
}

// Data Validation
export interface DataValidation {
  hasNaN: boolean;
  hasInfinite: boolean;
  hasNegative: boolean;
  missingTimestamps: number[];
  duplicateTimestamps: number[];
  timeGaps: { start: number; end: number; duration: number }[];
  samplingRateVariation: {
    nominal: number;
    actual: number;
    coefficient: number;
  };
}

// User Preferences
export interface UserPreferences {
  defaultChartHeight: number;
  defaultDecimationFactor: number;
  preferredStatistics: string[];
  autoSync: boolean;
  showGrid: boolean;
  enableAnimations: boolean;
  exportDefaults: Partial<ExportConfig>;
  theme: 'light' | 'dark' | 'auto';
}

// Loading States
export interface LoadingState {
  loading: boolean;
  progress?: number;
  stage?: string;
  error?: string;
}