/**
 * Polar Data Processing Utilities for OrcaFlex Results Dashboard
 * 
 * Processes polar response data for marine engineering analysis
 * Mathematical foundations:
 * - R(θ) = |F(θ)| or R(θ) = √(x(θ)² + y(θ)²)  
 * - RAO(ω,θ) = X(ω,θ)/A(ω)
 * - Statistical overlays with confidence bands
 */

import { cloneDeep, groupBy, max, mean, min, sortBy } from 'lodash';

export interface PolarDataPoint {
  heading: number;       // θ in degrees [0°, 345°]
  response: number;      // R(θ) - response amplitude
  phase?: number;        // Phase angle in degrees
  frequency?: number;    // ω in rad/s or Hz
  loadCase?: string;     // Loading condition identifier
  severity?: 'safe' | 'caution' | 'critical'; // Color coding
  timestamp?: Date;      // For time-series data
}

export interface PolarDataSet {
  id: string;
  label: string;
  data: PolarDataPoint[];
  color: string;
  lineWidth?: number;
  showMarkers?: boolean;
  visible?: boolean;
  type: 'response' | 'rao' | 'statistical';
  units: string;
  responseType: string; // e.g., 'Heave', 'Pitch', 'Surge'
}

export interface PolarLimits {
  operational: number;   // R_op - operational limit
  survival: number;      // R_surv - survival limit
  units: string;
}

export interface StatisticalOverlay {
  mean: PolarDataPoint[];
  stdBands: {
    upper: PolarDataPoint[];
    lower: PolarDataPoint[];
  };
  confidence: number; // Confidence level (e.g., 95%)
}

export interface ProcessingOptions {
  headingIncrement: number;    // Default 15° for 24-point analysis
  interpolateGaps: boolean;    // Interpolate missing data points
  smoothing: boolean;          // Apply smoothing filter
  filterOutliers: boolean;     // Remove statistical outliers
  normalizeAmplitudes: boolean; // Normalize to maximum response
}

/**
 * Converts Cartesian coordinates to polar response
 * R(θ) = √(x(θ)² + y(θ)²)
 */
export function cartesianToPolar(
  x: number, 
  y: number
): { magnitude: number; phase: number } {
  const magnitude = Math.sqrt(x * x + y * y);
  const phase = Math.atan2(y, x) * (180 / Math.PI);
  return { magnitude, phase: phase < 0 ? phase + 360 : phase };
}

/**
 * Calculates Response Amplitude Operator (RAO)
 * RAO(ω,θ) = X(ω,θ)/A(ω)
 */
export function calculateRAO(
  responseAmplitude: number,
  waveAmplitude: number
): number {
  return waveAmplitude !== 0 ? responseAmplitude / waveAmplitude : 0;
}

/**
 * Generates standard 24-point heading array [0°, 15°, ..., 345°]
 */
export function generateHeadingArray(increment = 15): number[] {
  const headings: number[] = [];
  for (let heading = 0; heading < 360; heading += increment) {
    headings.push(heading);
  }
  return headings;
}

/**
 * Interpolates missing data points using linear interpolation
 */
export function interpolatePolarData(
  data: PolarDataPoint[],
  targetHeadings: number[]
): PolarDataPoint[] {
  const sortedData = sortBy(data, 'heading');
  const result: PolarDataPoint[] = [];

  targetHeadings.forEach(targetHeading => {
    const existing = sortedData.find(d => d.heading === targetHeading);
    
    if (existing) {
      result.push(existing);
      return;
    }

    // Find surrounding points for interpolation
    let lowerPoint: PolarDataPoint | undefined;
    let upperPoint: PolarDataPoint | undefined;

    for (let i = 0; i < sortedData.length; i++) {
      if (sortedData[i].heading < targetHeading) {
        lowerPoint = sortedData[i];
      } else if (sortedData[i].heading > targetHeading && !upperPoint) {
        upperPoint = sortedData[i];
        break;
      }
    }

    // Handle circular interpolation (e.g., 345° to 15°)
    if (!lowerPoint || !upperPoint) {
      const lastPoint = sortedData[sortedData.length - 1];
      const firstPoint = sortedData[0];
      
      if (targetHeading > lastPoint.heading) {
        lowerPoint = lastPoint;
        upperPoint = { ...firstPoint, heading: firstPoint.heading + 360 };
      } else {
        lowerPoint = { ...lastPoint, heading: lastPoint.heading - 360 };
        upperPoint = firstPoint;
      }
    }

    if (lowerPoint && upperPoint) {
      const ratio = (targetHeading - lowerPoint.heading) / 
                   (upperPoint.heading - lowerPoint.heading);
      
      const interpolatedResponse = lowerPoint.response + 
                                 ratio * (upperPoint.response - lowerPoint.response);
      
      result.push({
        heading: targetHeading,
        response: interpolatedResponse,
        phase: lowerPoint.phase !== undefined && upperPoint.phase !== undefined
          ? lowerPoint.phase + ratio * (upperPoint.phase - lowerPoint.phase)
          : undefined,
        frequency: lowerPoint.frequency,
        loadCase: lowerPoint.loadCase,
        severity: determineSeverity(interpolatedResponse, lowerPoint.loadCase || '')
      });
    }
  });

  return result;
}

/**
 * Calculates statistical overlays for multiple datasets
 */
export function calculateStatisticalOverlay(
  datasets: PolarDataSet[],
  confidenceLevel = 0.95
): StatisticalOverlay {
  const headings = generateHeadingArray();
  const meanData: PolarDataPoint[] = [];
  const upperBand: PolarDataPoint[] = [];
  const lowerBand: PolarDataPoint[] = [];

  headings.forEach(heading => {
    const responsesAtHeading = datasets
      .flatMap(ds => ds.data)
      .filter(point => point.heading === heading)
      .map(point => point.response);

    if (responsesAtHeading.length === 0) return;

    const meanResponse = mean(responsesAtHeading);
    const std = calculateStandardDeviation(responsesAtHeading);
    const zScore = getZScore(confidenceLevel);

    meanData.push({
      heading,
      response: meanResponse,
      severity: 'safe'
    });

    upperBand.push({
      heading,
      response: meanResponse + zScore * std,
      severity: 'caution'
    });

    lowerBand.push({
      heading,
      response: Math.max(0, meanResponse - zScore * std),
      severity: 'safe'
    });
  });

  return {
    mean: meanData,
    stdBands: { upper: upperBand, lower: lowerBand },
    confidence: confidenceLevel
  };
}

/**
 * Applies smoothing filter to polar data
 */
export function applySmoothingFilter(
  data: PolarDataPoint[],
  windowSize = 3
): PolarDataPoint[] {
  if (windowSize < 3 || windowSize % 2 === 0) {
    throw new Error('Window size must be odd and >= 3');
  }

  const halfWindow = Math.floor(windowSize / 2);
  const smoothedData = cloneDeep(data);
  const sortedData = sortBy(data, 'heading');

  sortedData.forEach((point, index) => {
    const windowData: number[] = [];
    
    for (let i = -halfWindow; i <= halfWindow; i++) {
      let targetIndex = index + i;
      
      // Handle circular boundary conditions
      if (targetIndex < 0) {
        targetIndex = sortedData.length + targetIndex;
      } else if (targetIndex >= sortedData.length) {
        targetIndex = targetIndex - sortedData.length;
      }
      
      windowData.push(sortedData[targetIndex].response);
    }
    
    const smoothedIndex = smoothedData.findIndex(d => d.heading === point.heading);
    if (smoothedIndex !== -1) {
      smoothedData[smoothedIndex].response = mean(windowData);
    }
  });

  return smoothedData;
}

/**
 * Filters outliers using IQR method
 */
export function filterOutliers(data: PolarDataPoint[]): PolarDataPoint[] {
  const responses = data.map(d => d.response).sort((a, b) => a - b);
  const q1Index = Math.floor(responses.length * 0.25);
  const q3Index = Math.floor(responses.length * 0.75);
  
  const q1 = responses[q1Index];
  const q3 = responses[q3Index];
  const iqr = q3 - q1;
  
  const lowerBound = q1 - 1.5 * iqr;
  const upperBound = q3 + 1.5 * iqr;
  
  return data.filter(d => 
    d.response >= lowerBound && d.response <= upperBound
  );
}

/**
 * Normalizes response amplitudes to maximum value
 */
export function normalizeAmplitudes(data: PolarDataPoint[]): PolarDataPoint[] {
  const maxResponse = max(data.map(d => d.response)) || 1;
  return data.map(point => ({
    ...point,
    response: point.response / maxResponse
  }));
}

/**
 * Determines severity level based on response magnitude and limits
 */
export function determineSeverity(
  response: number,
  loadCase: string,
  limits?: PolarLimits
): 'safe' | 'caution' | 'critical' {
  if (!limits) return 'safe';
  
  if (response >= limits.survival) return 'critical';
  if (response >= limits.operational) return 'caution';
  return 'safe';
}

/**
 * Groups datasets by loading condition
 */
export function groupByLoadCase(datasets: PolarDataSet[]): Record<string, PolarDataSet[]> {
  return groupBy(datasets, dataset => {
    const loadCase = dataset.data[0]?.loadCase || 'default';
    return loadCase;
  });
}

/**
 * Converts degrees to radians
 */
export function degreesToRadians(degrees: number): number {
  return degrees * (Math.PI / 180);
}

/**
 * Converts radians to degrees
 */
export function radiansToDegrees(radians: number): number {
  return radians * (180 / Math.PI);
}

/**
 * Calculates standard deviation
 */
function calculateStandardDeviation(values: number[]): number {
  const meanVal = mean(values);
  const squaredDiffs = values.map(value => Math.pow(value - meanVal, 2));
  return Math.sqrt(mean(squaredDiffs));
}

/**
 * Gets Z-score for confidence level
 */
function getZScore(confidenceLevel: number): number {
  const zScores: Record<string, number> = {
    '0.90': 1.645,
    '0.95': 1.96,
    '0.99': 2.576
  };
  return zScores[confidenceLevel.toString()] || 1.96;
}

/**
 * Processes raw polar data with specified options
 */
export function processPolarData(
  rawData: PolarDataPoint[],
  options: Partial<ProcessingOptions> = {}
): PolarDataPoint[] {
  const defaultOptions: ProcessingOptions = {
    headingIncrement: 15,
    interpolateGaps: true,
    smoothing: false,
    filterOutliers: false,
    normalizeAmplitudes: false
  };

  const opts = { ...defaultOptions, ...options };
  let processedData = cloneDeep(rawData);

  // Filter outliers first if requested
  if (opts.filterOutliers) {
    processedData = filterOutliers(processedData);
  }

  // Interpolate missing data points
  if (opts.interpolateGaps) {
    const targetHeadings = generateHeadingArray(opts.headingIncrement);
    processedData = interpolatePolarData(processedData, targetHeadings);
  }

  // Apply smoothing filter
  if (opts.smoothing) {
    processedData = applySmoothingFilter(processedData);
  }

  // Normalize amplitudes
  if (opts.normalizeAmplitudes) {
    processedData = normalizeAmplitudes(processedData);
  }

  return processedData;
}

/**
 * Exports processed data for external analysis
 */
export function exportPolarData(
  datasets: PolarDataSet[],
  format: 'csv' | 'json' = 'csv'
): string {
  if (format === 'json') {
    return JSON.stringify(datasets, null, 2);
  }

  // CSV format
  const headers = ['Dataset', 'Heading', 'Response', 'Phase', 'LoadCase', 'Severity'];
  const rows = [headers.join(',')];

  datasets.forEach(dataset => {
    dataset.data.forEach(point => {
      const row = [
        dataset.label,
        point.heading.toString(),
        point.response.toString(),
        point.phase?.toString() || '',
        point.loadCase || '',
        point.severity || ''
      ];
      rows.push(row.join(','));
    });
  });

  return rows.join('\n');
}