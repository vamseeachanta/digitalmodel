// Custom hook for results operations
// Provides state management and operations for analysis results

import { useState, useEffect, useCallback, useMemo } from 'react';
import { useSelector, useDispatch } from 'react-redux';
import {
  AnalysisResults,
  ResultType,
  GetResultsRequest,
  ExportResultsRequest,
  LoadingState,
  TimeSeriesResult,
  ComponentResult,
  SummaryStatistics,
  ExtremeValues,
  ChartData,
  Dataset
} from '../types/api';
import { apiClient } from '../services/api';
import {
  fetchResults,
  fetchResultsSummary,
  exportResults,
  clearResults,
  selectResults,
  selectResultsSummary,
  selectResultsLoading,
  selectResultsError
} from '../store/slices/resultsSlice';
import type { RootState, AppDispatch } from '../store';

export interface UseResultsOptions {
  analysisId: string;
  autoLoad?: boolean;
  defaultResultTypes?: ResultType[];
  defaultComponentIds?: string[];
  includeTimeSeries?: boolean;
  includeStatistics?: boolean;
  includeExtremes?: boolean;
  cacheResults?: boolean;
}

export interface UseResultsReturn {
  // Data
  results: AnalysisResults | null;
  summary: any | null;
  timeSeriesData: TimeSeriesResult[];
  componentResults: ComponentResult[];
  summaryStatistics: SummaryStatistics | null;
  extremeValues: ExtremeValues | null;
  
  // Loading states
  loading: LoadingState;
  summaryLoading: LoadingState;
  exporting: boolean;
  
  // Error states
  error: string | null;
  summaryError: string | null;
  exportError: string | null;
  
  // Operations
  loadResults: (params?: Partial<GetResultsRequest>) => Promise<void>;
  loadSummary: () => Promise<void>;
  exportData: (params: ExportResultsRequest) => Promise<Blob | null>;
  clearData: () => void;
  refreshResults: () => Promise<void>;
  
  // Chart data helpers
  getTimeSeriesChart: (componentId: string, resultType: string) => ChartData | null;
  getComponentComparisonChart: (resultType: string) => ChartData | null;
  getStatisticsChart: (statType: string) => ChartData | null;
  
  // Filtering and utilities
  getResultsByComponent: (componentId: string) => ComponentResult | null;
  getResultsByType: (resultType: string) => ComponentResult[];
  getAvailableResultTypes: () => ResultType[];
  getAvailableComponents: () => string[];
  
  // Data transformation
  transformToCSV: () => string;
  transformToJSON: () => string;
  
  // Configuration
  resultTypes: ResultType[];
  componentIds: string[];
  setResultTypes: (types: ResultType[]) => void;
  setComponentIds: (ids: string[]) => void;
}

const DEFAULT_CHART_COLORS = [
  '#8884d8', '#82ca9d', '#ffc658', '#ff7300', '#00ff00',
  '#ff00ff', '#00ffff', '#ff0000', '#0000ff', '#ffff00'
];

export const useResults = (options: UseResultsOptions): UseResultsReturn => {
  const {
    analysisId,
    autoLoad = true,
    defaultResultTypes = [],
    defaultComponentIds = [],
    includeTimeSeries = true,
    includeStatistics = true,
    includeExtremes = true,
    cacheResults = true
  } = options;

  const dispatch = useDispatch<AppDispatch>();
  
  // Redux selectors
  const results = useSelector((state: RootState) => selectResults(state, analysisId));
  const summary = useSelector((state: RootState) => selectResultsSummary(state, analysisId));
  const loading = useSelector((state: RootState) => selectResultsLoading(state));
  const error = useSelector((state: RootState) => selectResultsError(state));

  // Local state
  const [summaryLoading, setSummaryLoading] = useState<LoadingState>('idle');
  const [summaryError, setSummaryError] = useState<string | null>(null);
  const [exporting, setExporting] = useState(false);
  const [exportError, setExportError] = useState<string | null>(null);
  const [resultTypes, setResultTypes] = useState<ResultType[]>(defaultResultTypes);
  const [componentIds, setComponentIds] = useState<string[]>(defaultComponentIds);

  // Computed values
  const timeSeriesData = useMemo(() => {
    return results?.time_series || [];
  }, [results]);

  const componentResults = useMemo(() => {
    return results?.component_results || [];
  }, [results]);

  const summaryStatistics = useMemo(() => {
    return results?.summary_statistics || null;
  }, [results]);

  const extremeValues = useMemo(() => {
    return results?.extreme_values || null;
  }, [results]);

  // Load results
  const loadResults = useCallback(async (params?: Partial<GetResultsRequest>) => {
    if (!analysisId) return;

    const finalParams: GetResultsRequest = {
      analysis_id: analysisId,
      result_types: params?.result_types || resultTypes,
      component_ids: params?.component_ids || componentIds,
      include_time_series: params?.include_time_series ?? includeTimeSeries,
      include_statistics: params?.include_statistics ?? includeStatistics,
      include_extremes: params?.include_extremes ?? includeExtremes,
      ...params
    };

    try {
      await dispatch(fetchResults(finalParams)).unwrap();
    } catch (err) {
      console.error('Failed to load results:', err);
    }
  }, [dispatch, analysisId, resultTypes, componentIds, includeTimeSeries, includeStatistics, includeExtremes]);

  // Load summary
  const loadSummary = useCallback(async () => {
    if (!analysisId) return;

    setSummaryLoading('loading');
    setSummaryError(null);
    
    try {
      await dispatch(fetchResultsSummary(analysisId)).unwrap();
      setSummaryLoading('succeeded');
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to load summary';
      setSummaryError(errorMessage);
      setSummaryLoading('failed');
    }
  }, [dispatch, analysisId]);

  // Export results
  const exportData = useCallback(async (params: ExportResultsRequest): Promise<Blob | null> => {
    if (!analysisId) return null;

    setExporting(true);
    setExportError(null);

    try {
      const blob = await dispatch(exportResults({
        ...params,
        analysis_id: analysisId
      })).unwrap();
      
      return blob;
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Failed to export results';
      setExportError(errorMessage);
      return null;
    } finally {
      setExporting(false);
    }
  }, [dispatch, analysisId]);

  // Clear data
  const clearData = useCallback(() => {
    dispatch(clearResults(analysisId));
    setSummaryError(null);
    setExportError(null);
  }, [dispatch, analysisId]);

  // Refresh results
  const refreshResults = useCallback(async () => {
    await Promise.all([
      loadResults(),
      loadSummary()
    ]);
  }, [loadResults, loadSummary]);

  // Chart data helpers
  const getTimeSeriesChart = useCallback((componentId: string, resultType: string): ChartData | null => {
    const timeSeries = timeSeriesData.find(ts => 
      ts.component_id === componentId && ts.result_type === resultType
    );

    if (!timeSeries || !timeSeries.timestamp || !timeSeries.values) {
      return null;
    }

    return {
      labels: timeSeries.timestamp.map((t, i) => `${t.toFixed(2)}s`),
      datasets: [{
        label: `${componentId} - ${resultType}`,
        data: timeSeries.values,
        borderColor: DEFAULT_CHART_COLORS[0],
        backgroundColor: `${DEFAULT_CHART_COLORS[0]}20`,
        fill: false,
        tension: 0.1
      }]
    };
  }, [timeSeriesData]);

  const getComponentComparisonChart = useCallback((resultType: string): ChartData | null => {
    if (!summaryStatistics) return null;

    const relevantComponents = componentResults.filter(cr => 
      cr.results[resultType as keyof typeof cr.results]
    );

    if (relevantComponents.length === 0) return null;

    const labels = relevantComponents.map(cr => cr.component_name);
    const maxValues = relevantComponents.map(cr => 
      cr.statistics.max_values[resultType] || 0
    );

    return {
      labels,
      datasets: [{
        label: `Max ${resultType}`,
        data: maxValues,
        backgroundColor: DEFAULT_CHART_COLORS.slice(0, maxValues.length),
        borderColor: DEFAULT_CHART_COLORS.slice(0, maxValues.length),
        borderWidth: 1
      }]
    };
  }, [componentResults, summaryStatistics]);

  const getStatisticsChart = useCallback((statType: string): ChartData | null => {
    if (!summaryStatistics) return null;

    const data = summaryStatistics[statType as keyof SummaryStatistics];
    if (!data || typeof data !== 'object') return null;

    const labels = Object.keys(data);
    const values = Object.values(data).map(v => typeof v === 'number' ? v : 0);

    return {
      labels,
      datasets: [{
        label: statType.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase()),
        data: values,
        backgroundColor: DEFAULT_CHART_COLORS.slice(0, values.length),
        borderColor: DEFAULT_CHART_COLORS.slice(0, values.length),
        borderWidth: 1
      }]
    };
  }, [summaryStatistics]);

  // Filtering and utilities
  const getResultsByComponent = useCallback((componentId: string): ComponentResult | null => {
    return componentResults.find(cr => cr.component_id === componentId) || null;
  }, [componentResults]);

  const getResultsByType = useCallback((resultType: string): ComponentResult[] => {
    return componentResults.filter(cr => 
      cr.results[resultType as keyof typeof cr.results]
    );
  }, [componentResults]);

  const getAvailableResultTypes = useCallback((): ResultType[] => {
    if (!results) return [];
    
    const types = new Set<ResultType>();
    componentResults.forEach(cr => {
      Object.keys(cr.results).forEach(key => {
        if (cr.results[key as keyof typeof cr.results]) {
          types.add(key as ResultType);
        }
      });
    });
    
    return Array.from(types);
  }, [results, componentResults]);

  const getAvailableComponents = useCallback((): string[] => {
    return componentResults.map(cr => cr.component_id);
  }, [componentResults]);

  // Data transformation
  const transformToCSV = useCallback((): string => {
    if (!results) return '';

    const rows: string[] = [];
    
    // Header
    rows.push('Component,Type,Timestamp,Value,Units');
    
    // Time series data
    timeSeriesData.forEach(ts => {
      ts.values.forEach((value, index) => {
        const timestamp = ts.timestamp[index];
        rows.push(`${ts.component_id},${ts.result_type},${timestamp},${value},${ts.units}`);
      });
    });
    
    return rows.join('\n');
  }, [results, timeSeriesData]);

  const transformToJSON = useCallback((): string => {
    if (!results) return '{}';
    
    return JSON.stringify({
      analysis_id: results.analysis_id,
      generated_at: results.generated_at,
      summary_statistics: summaryStatistics,
      extreme_values: extremeValues,
      component_count: componentResults.length,
      time_series_count: timeSeriesData.length,
      available_result_types: getAvailableResultTypes(),
      available_components: getAvailableComponents()
    }, null, 2);
  }, [results, summaryStatistics, extremeValues, componentResults, timeSeriesData, getAvailableResultTypes, getAvailableComponents]);

  // Auto-load effect
  useEffect(() => {
    if (autoLoad && analysisId) {
      loadResults();
      loadSummary();
    }
  }, [autoLoad, analysisId, loadResults, loadSummary]);

  // Update results when configuration changes
  useEffect(() => {
    if (analysisId && (resultTypes.length > 0 || componentIds.length > 0)) {
      loadResults();
    }
  }, [analysisId, resultTypes, componentIds, loadResults]);

  return {
    // Data
    results,
    summary,
    timeSeriesData,
    componentResults,
    summaryStatistics,
    extremeValues,
    
    // Loading states
    loading,
    summaryLoading,
    exporting,
    
    // Error states
    error,
    summaryError,
    exportError,
    
    // Operations
    loadResults,
    loadSummary,
    exportData,
    clearData,
    refreshResults,
    
    // Chart data helpers
    getTimeSeriesChart,
    getComponentComparisonChart,
    getStatisticsChart,
    
    // Filtering and utilities
    getResultsByComponent,
    getResultsByType,
    getAvailableResultTypes,
    getAvailableComponents,
    
    // Data transformation
    transformToCSV,
    transformToJSON,
    
    // Configuration
    resultTypes,
    componentIds,
    setResultTypes,
    setComponentIds
  };
};

// Specialized hook for real-time results monitoring
export interface UseResultsMonitorOptions {
  analysisId: string;
  refreshInterval?: number;
  autoRefresh?: boolean;
}

export const useResultsMonitor = (options: UseResultsMonitorOptions) => {
  const { analysisId, refreshInterval = 30000, autoRefresh = false } = options;
  
  const [isMonitoring, setIsMonitoring] = useState(false);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  
  const resultsHook = useResults({
    analysisId,
    autoLoad: false,
    includeTimeSeries: false, // Monitor summary only for performance
    includeStatistics: true,
    includeExtremes: false
  });

  const startMonitoring = useCallback(() => {
    setIsMonitoring(true);
  }, []);

  const stopMonitoring = useCallback(() => {
    setIsMonitoring(false);
  }, []);

  // Auto-refresh effect
  useEffect(() => {
    if (!isMonitoring || !autoRefresh) return;

    const interval = setInterval(async () => {
      if (resultsHook.loading !== 'loading') {
        await resultsHook.refreshResults();
        setLastUpdate(new Date());
      }
    }, refreshInterval);

    return () => clearInterval(interval);
  }, [isMonitoring, autoRefresh, refreshInterval, resultsHook]);

  return {
    ...resultsHook,
    isMonitoring,
    startMonitoring,
    stopMonitoring,
    lastUpdate
  };
};

// Hook for results comparison
export interface UseResultsComparisonOptions {
  analysisIds: string[];
  resultType?: ResultType;
  componentId?: string;
}

export const useResultsComparison = (options: UseResultsComparisonOptions) => {
  const { analysisIds, resultType, componentId } = options;
  
  const [loading, setLoading] = useState<LoadingState>('idle');
  const [error, setError] = useState<string | null>(null);
  const [comparisonData, setComparisonData] = useState<any[]>([]);

  const loadComparison = useCallback(async () => {
    if (analysisIds.length === 0) return;

    setLoading('loading');
    setError(null);

    try {
      const promises = analysisIds.map(async (id) => {
        const response = await apiClient.getResults({
          analysis_id: id,
          result_types: resultType ? [resultType] : undefined,
          component_ids: componentId ? [componentId] : undefined,
          include_time_series: false,
          include_statistics: true,
          include_extremes: false
        });
        return { analysisId: id, results: response.data };
      });

      const results = await Promise.all(promises);
      setComparisonData(results);
      setLoading('succeeded');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load comparison data');
      setLoading('failed');
    }
  }, [analysisIds, resultType, componentId]);

  const getComparisonChart = useCallback((): ChartData | null => {
    if (comparisonData.length === 0) return null;

    const labels = comparisonData.map((_, index) => `Analysis ${index + 1}`);
    const datasets: Dataset[] = [];

    // Create dataset for each component if comparing across components
    if (!componentId) {
      // Get all unique component IDs
      const allComponentIds = new Set<string>();
      comparisonData.forEach(({ results }) => {
        results.component_results.forEach((cr: ComponentResult) => {
          allComponentIds.add(cr.component_id);
        });
      });

      Array.from(allComponentIds).forEach((compId, index) => {
        const data = comparisonData.map(({ results }) => {
          const componentResult = results.component_results.find((cr: ComponentResult) => cr.component_id === compId);
          return componentResult?.statistics.max_values[resultType || 'tension'] || 0;
        });

        datasets.push({
          label: compId,
          data,
          backgroundColor: DEFAULT_CHART_COLORS[index % DEFAULT_CHART_COLORS.length],
          borderColor: DEFAULT_CHART_COLORS[index % DEFAULT_CHART_COLORS.length],
          borderWidth: 1
        });
      });
    } else {
      // Single component comparison across analyses
      const data = comparisonData.map(({ results }) => {
        const componentResult = results.component_results.find((cr: ComponentResult) => cr.component_id === componentId);
        return componentResult?.statistics.max_values[resultType || 'tension'] || 0;
      });

      datasets.push({
        label: `${componentId} - ${resultType || 'tension'}`,
        data,
        backgroundColor: DEFAULT_CHART_COLORS[0],
        borderColor: DEFAULT_CHART_COLORS[0],
        borderWidth: 1
      });
    }

    return { labels, datasets };
  }, [comparisonData, componentId, resultType]);

  useEffect(() => {
    loadComparison();
  }, [loadComparison]);

  return {
    loading,
    error,
    comparisonData,
    loadComparison,
    getComparisonChart
  };
};