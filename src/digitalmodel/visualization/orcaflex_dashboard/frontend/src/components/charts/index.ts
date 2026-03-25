/**
 * Interactive Chart Components for OrcaFlex Results Dashboard
 * 
 * Export index for all visualization components
 * Provides complete toolkit for marine engineering analysis
 */

// Polar Plot Components
export { default as PolarPlot } from './PolarPlot';
export { default as PolarPlotControls } from './PolarPlotControls';
export { default as MultiPolarGrid } from './MultiPolarGrid';
export { default as PolarLegend } from './PolarLegend';
export { default as PolarExport } from './PolarExport';

// Time Series Components
export { default as TimeSeries } from './TimeSeries';
export { default as TimeSeriesControls } from './TimeSeriesControls';
export { default as MultiTimeSeriesPanel } from './MultiTimeSeriesPanel';
export { default as TimeSeriesStatistics } from './TimeSeriesStatistics';
export { default as TimeRangeSelector } from './TimeRangeSelector';
export { default as ComponentGroupSelector } from './ComponentGroupSelector';

// Data Processing Utilities
export {
  // Polar Types
  type PolarDataPoint,
  type PolarDataSet,
  type PolarLimits,
  type StatisticalOverlay,
  type ProcessingOptions,
  
  // Core Functions
  cartesianToPolar,
  calculateRAO,
  generateHeadingArray,
  interpolatePolarData,
  calculateStatisticalOverlay,
  applySmoothingFilter,
  filterOutliers,
  normalizeAmplitudes,
  determineSeverity,
  groupByLoadCase,
  degreesToRadians,
  radiansToDegrees,
  processPolarData,
  exportPolarData
} from './PolarDataProcessor';

// Time Series Types
export type {
  TimeSeriesData,
  TimeRange,
  ComponentGroup,
  StatisticalOverlay as TimeSeriesStatisticalOverlay,
  StatisticalSummary,
  TimeSeriesControlsState,
  ChartInstance,
  MultiTimeSeriesConfig,
  FFTResult,
  FrequencyDomainAnalysis,
  ExportConfig,
  DataProcessingOptions,
  PerformanceMetrics,
  AnimationState,
  CursorState,
  ZoomState,
  ChartTheme,
  DataStream,
  ThresholdAlert,
  DataValidation,
  UserPreferences,
  LoadingState
} from './types';

// Component Props Types - Polar
export type { VisualizationOptions } from './PolarPlotControls';
export type { PolarPlotProps } from './PolarPlot';
export type { PolarPlotControlsProps } from './PolarPlotControls';
export type { MultiPolarGridProps } from './MultiPolarGrid';
export type { PolarLegendProps } from './PolarLegend';
export type { PolarExportProps } from './PolarExport';

/**
 * Default Configuration Constants
 */
export const DEFAULT_POLAR_CONFIG = {
  HEADING_INCREMENT: 15, // degrees
  TOTAL_HEADINGS: 24,    // 0° to 345° in 15° steps
  DEFAULT_COLORS: [
    '#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
    '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf'
  ],
  SEVERITY_COLORS: {
    safe: '#4caf50',
    caution: '#ff9800',
    critical: '#f44336'
  },
  PLOT_SIZES: {
    small: { width: 400, height: 400 },
    medium: { width: 600, height: 600 },
    large: { width: 800, height: 800 }
  }
} as const;

/**
 * Default Time Series Configuration
 */
export const DEFAULT_TIMESERIES_CONFIG = {
  CHART_HEIGHT: 400,
  DECIMATION_FACTOR: 1,
  DEFAULT_STATISTICS: ['mean', 'rms', 'stdDev'],
  PERFORMANCE_THRESHOLDS: {
    DATA_POINTS_WARNING: 10000,
    DATA_POINTS_CRITICAL: 50000,
    RENDER_TIME_WARNING: 100, // ms
    RENDER_TIME_CRITICAL: 500, // ms
  },
  DEFAULT_COLORS: {
    struts: '#1976d2',
    jackets: '#388e3c',
    foundations: '#d32f2f',
    default: '#757575'
  },
  STATISTICAL_FORMULAS: {
    rms: 'x_RMS = √[(1/n) Σx_i²]',
    mean: 'x̄ = (1/n) Σx_i',
    stdDev: 'σ = √[(1/n) Σ(x_i - x̄)²]',
    variance: 'σ² = (1/n) Σ(x_i - x̄)²'
  }
} as const;

/**
 * Engineering Constants for Marine Applications
 */
export const MARINE_ENGINEERING_CONSTANTS = {
  // Standard heading directions
  HEADINGS: {
    HEAD_SEAS: 0,      // θ = 0°
    BOW_QUARTER: 45,   // θ = 45°
    BEAM_SEAS: 90,     // θ = 90°
    STERN_QUARTER: 135, // θ = 135°
    FOLLOWING_SEAS: 180 // θ = 180°
  },
  
  // Response types commonly analyzed
  RESPONSE_TYPES: [
    'Heave',
    'Surge', 
    'Sway',
    'Roll',
    'Pitch',
    'Yaw',
    'Tension',
    'Bending Moment',
    'Shear Force',
    'Acceleration'
  ],
  
  // Typical operational limits (example values)
  TYPICAL_LIMITS: {
    HEAVE: { operational: 2.0, survival: 5.0, units: 'm' },
    PITCH: { operational: 5.0, survival: 15.0, units: 'deg' },
    ROLL: { operational: 10.0, survival: 25.0, units: 'deg' },
    ACCELERATION: { operational: 0.2, survival: 0.5, units: 'g' }
  }
} as const;

/**
 * Utility function to create sample polar data for testing/demo
 */
export function createSamplePolarData(
  responseType: string = 'Heave',
  amplitude: number = 1.0,
  units: string = 'm'
): PolarDataSet {
  const headings = generateHeadingArray(15);
  
  const data = headings.map(heading => {
    // Generate realistic response pattern (higher in beam seas)
    const headingRad = degreesToRadians(heading);
    const beamFactor = Math.abs(Math.sin(headingRad)); // Peak at 90° and 270°
    const headFactor = Math.abs(Math.cos(headingRad)); // Peak at 0° and 180°
    
    // Combine factors with some randomness
    const response = amplitude * (0.3 + 0.7 * beamFactor + 0.2 * headFactor + 0.1 * Math.random());
    
    return {
      heading,
      response,
      phase: heading * 2 + Math.random() * 10 - 5, // Some phase shift
      loadCase: 'Sample Load Case',
      severity: response > amplitude * 0.8 ? 'caution' as const : 'safe' as const
    };
  });
  
  return {
    id: `sample_${responseType.toLowerCase()}`,
    label: `Sample ${responseType}`,
    data,
    color: DEFAULT_POLAR_CONFIG.DEFAULT_COLORS[0],
    type: 'response',
    units,
    responseType,
    visible: true,
    lineWidth: 2,
    showMarkers: true
  };
}

/**
 * Utility function to create sample time series data for testing/demo
 */
export function createSampleTimeSeriesData(
  componentIds: string[] = ['Strut-1', 'Jacket-1', 'Foundation-1'],
  duration: number = 100, // seconds
  samplingRate: number = 10 // Hz
): TimeSeriesData[] {
  const dt = 1 / samplingRate;
  const data: TimeSeriesData[] = [];
  
  for (let t = 0; t <= duration; t += dt) {
    const point: TimeSeriesData = { time: t };
    
    componentIds.forEach((componentId, index) => {
      // Generate realistic time series with different characteristics
      const frequency1 = 0.1 + index * 0.05; // Primary frequency
      const frequency2 = 0.8 + index * 0.2;  // Secondary frequency
      const amplitude = 1.0 + index * 0.5;
      const noise = 0.1 * (Math.random() - 0.5);
      
      point[componentId] = 
        amplitude * Math.sin(2 * Math.PI * frequency1 * t) +
        0.3 * amplitude * Math.sin(2 * Math.PI * frequency2 * t) +
        noise;
    });
    
    data.push(point);
  }
  
  return data;
}

/**
 * Utility function to create sample component groups
 */
export function createSampleComponentGroups(): ComponentGroup[] {
  return [
    {
      id: 'struts-group',
      name: 'Strut Components',
      type: 'struts',
      componentIds: ['Strut-1', 'Strut-2', 'Strut-3', 'Strut-4'],
      description: 'Tension strut members'
    },
    {
      id: 'jackets-group', 
      name: 'Jacket Structure',
      type: 'jackets',
      componentIds: ['Jacket-1', 'Jacket-2', 'Jacket-Node-1', 'Jacket-Node-2'],
      description: 'Main jacket structure components'
    },
    {
      id: 'foundations-group',
      name: 'Foundation System',
      type: 'foundations', 
      componentIds: ['Foundation-1', 'Pile-1', 'Pile-2', 'Pile-3'],
      description: 'Foundation and piling system'
    }
  ];
}

/**
 * Utility function to create sample limits
 */
export function createSampleLimits(
  responseType: string = 'Heave'
): PolarLimits {
  const limits = MARINE_ENGINEERING_CONSTANTS.TYPICAL_LIMITS;
  const typicalLimit = limits[responseType.toUpperCase() as keyof typeof limits];
  
  if (typicalLimit) {
    return typicalLimit;
  }
  
  // Default limits if response type not found
  return {
    operational: 1.0,
    survival: 2.5,
    units: 'units'
  };
}

/**
 * Performance optimization utilities
 */
export const PERFORMANCE_UTILS = {
  /**
   * Calculate optimal decimation factor based on data size
   */
  calculateOptimalDecimation: (dataSize: number, targetSize: number = 5000): number => {
    if (dataSize <= targetSize) return 1;
    return Math.ceil(dataSize / targetSize);
  },
  
  /**
   * Estimate memory usage for time series data
   */
  estimateMemoryUsage: (dataPoints: number, componentsCount: number): number => {
    // Rough estimate: 8 bytes per number (Float64) + overhead
    const bytesPerPoint = (componentsCount + 1) * 8; // +1 for time
    return dataPoints * bytesPerPoint;
  },
  
  /**
   * Check if browser can handle data size
   */
  canHandleDataSize: (dataPoints: number, componentsCount: number): boolean => {
    const estimatedMemory = PERFORMANCE_UTILS.estimateMemoryUsage(dataPoints, componentsCount);
    const maxMemory = 100 * 1024 * 1024; // 100MB limit
    return estimatedMemory < maxMemory;
  }
};

// Re-export from PolarDataProcessor for convenience
export {
  generateHeadingArray,
  degreesToRadians,
  radiansToDegrees
} from './PolarDataProcessor';