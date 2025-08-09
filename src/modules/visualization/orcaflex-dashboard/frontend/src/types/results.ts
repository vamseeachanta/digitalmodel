/**
 * Type definitions for simulation results and data visualization
 */

export interface TimeSeriesPoint {
  time: number;
  value: number;
}

export interface TimeSeriesData {
  variable_name: string;
  unit: string;
  object_name?: string;
  data_type: string;
  data: TimeSeriesPoint[];
  sample_rate?: number;
  duration?: number;
}

export interface StatisticalSummary {
  variable_name: string;
  count: number;
  mean: number;
  std: number;
  min: number;
  max: number;
  percentile_25: number;
  percentile_50: number; // median
  percentile_75: number;
  percentile_95: number;
  percentile_99: number;
}

export interface FrequencyDomainData {
  variable_name: string;
  frequencies: number[];
  magnitudes: number[];
  phases?: number[];
  psd?: number[]; // Power spectral density
}

export interface SimulationResult {
  analysis_id: string;
  result_id: string;
  name: string;
  description?: string;
  time_series: TimeSeriesData[];
  statistics: StatisticalSummary[];
  frequency_domain: FrequencyDomainData[];
  simulation_time?: number;
  time_step?: number;
  solver_info: Record<string, any>;
  source_files: string[];
  output_files: string[];
  created_at: string;
}

export interface ResultsQuery {
  analysis_id?: string;
  variable_names?: string[];
  object_names?: string[];
  data_types?: string[];
  time_range?: [number, number];
  include_statistics?: boolean;
  include_frequency_domain?: boolean;
}

export interface ResultsMetadata {
  analysis_id: string;
  total_variables: number;
  available_variables: string[];
  available_objects: string[];
  data_types: string[];
  time_range: [number, number];
  sample_rate: number;
  file_size: number;
}

export interface ResultsFilters {
  analysis_id?: string;
  variable_name?: string;
  object_name?: string;
  data_type?: string;
  date_range?: {
    start: string;
    end: string;
  };
}

export interface ResultsPagination {
  limit: number;
  offset: number;
}

export interface ResultsListResponse {
  results: SimulationResult[];
  total: number;
  limit: number;
  offset: number;
  has_more: boolean;
}

export interface ChartConfiguration {
  chart_type: 'line' | 'scatter' | 'bar' | 'heatmap' | 'contour';
  variables: string[];
  x_axis?: string;
  y_axis?: string;
  color_by?: string;
  time_range?: [number, number];
  title?: string;
  show_legend?: boolean;
  show_grid?: boolean;
  auto_scale?: boolean;
}

export interface ExportConfiguration {
  format: 'csv' | 'json' | 'excel' | 'pdf';
  variables?: string[];
  time_range?: [number, number];
  include_metadata?: boolean;
  include_statistics?: boolean;
}

export interface VisualizationData {
  charts: ChartConfiguration[];
  tables: string[];
  summary_cards: Array<{
    title: string;
    value: number;
    unit: string;
    trend?: 'up' | 'down' | 'stable';
  }>;
}