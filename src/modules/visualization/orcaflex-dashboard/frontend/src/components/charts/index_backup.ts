/**
 * Interactive Polar Plot Components for OrcaFlex Results Dashboard
 * 
 * Export index for all polar plot visualization components
 * Provides complete toolkit for marine engineering polar response analysis
 */

// Core Components
export { default as PolarPlot } from './PolarPlot';
export { default as PolarPlotControls } from './PolarPlotControls';
export { default as MultiPolarGrid } from './MultiPolarGrid';
export { default as PolarLegend } from './PolarLegend';
export { default as PolarExport } from './PolarExport';

// Data Processing Utilities
export {
  // Types
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

// Control Types
export type { VisualizationOptions } from './PolarPlotControls';

// Component Props Types
export type {
  PolarPlotProps
} from './PolarPlot';

export type {
  PolarPlotControlsProps
} from './PolarPlotControls';

export type {
  MultiPolarGridProps
} from './MultiPolarGrid';

export type {
  PolarLegendProps
} from './PolarLegend';

export type {
  PolarExportProps
} from './PolarExport';

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

// Re-export from PolarDataProcessor for convenience
export {
  generateHeadingArray,
  degreesToRadians,
  radiansToDegrees
} from './PolarDataProcessor';