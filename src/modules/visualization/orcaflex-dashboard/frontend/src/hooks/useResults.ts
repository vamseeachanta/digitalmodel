/**
 * React hooks for results operations
 */

import { useQuery, useMutation, useQueryClient } from 'react-query';
import { toast } from 'react-toastify';

import { resultsService } from '@/services/resultsService';
import {
  SimulationResult,
  ResultsQuery,
  ResultsMetadata,
  TimeSeriesData,
  StatisticalSummary,
  ResultsFilters,
  ResultsPagination,
  ExportConfiguration,
} from '@/types/results';

// Query keys for cache management
export const resultsQueryKeys = {
  all: ['results'] as const,
  lists: () => [...resultsQueryKeys.all, 'list'] as const,
  list: (filters: ResultsFilters, pagination: ResultsPagination) =>
    [...resultsQueryKeys.lists(), filters, pagination] as const,
  details: () => [...resultsQueryKeys.all, 'detail'] as const,
  detail: (id: string) => [...resultsQueryKeys.details(), id] as const,
  metadata: (id: string) => [...resultsQueryKeys.detail(id), 'metadata'] as const,
  timeseries: (id: string, query?: Partial<ResultsQuery>) => 
    [...resultsQueryKeys.detail(id), 'timeseries', query] as const,
  statistics: (id: string, variables?: string[]) => 
    [...resultsQueryKeys.detail(id), 'statistics', variables] as const,
  query: (id: string, query: ResultsQuery) =>
    [...resultsQueryKeys.detail(id), 'query', query] as const,
};

// Get list of results
export const useResults = (
  filters: ResultsFilters = {},
  pagination: ResultsPagination = { limit: 50, offset: 0 }
) => {
  return useQuery({
    queryKey: resultsQueryKeys.list(filters, pagination),
    queryFn: () => resultsService.getResults(filters, pagination),
    staleTime: 30000, // 30 seconds
    cacheTime: 300000, // 5 minutes
    keepPreviousData: true,
  });
};

// Get single result
export const useResult = (id: string) => {
  return useQuery({
    queryKey: resultsQueryKeys.detail(id),
    queryFn: () => resultsService.getResult(id),
    enabled: !!id,
    staleTime: 300000, // 5 minutes (results don't change often)
    cacheTime: 600000, // 10 minutes
  });
};

// Get result metadata
export const useResultMetadata = (id: string) => {
  return useQuery({
    queryKey: resultsQueryKeys.metadata(id),
    queryFn: () => resultsService.getResultMetadata(id),
    enabled: !!id,
    staleTime: 300000, // 5 minutes
    cacheTime: 600000, // 10 minutes
  });
};

// Get time series data
export const useTimeSeriesData = (
  id: string,
  query: Partial<ResultsQuery> = {}
) => {
  return useQuery({
    queryKey: resultsQueryKeys.timeseries(id, query),
    queryFn: () => resultsService.getTimeSeriesData(id, query),
    enabled: !!id,
    staleTime: 300000, // 5 minutes
    cacheTime: 600000, // 10 minutes
  });
};

// Get statistics
export const useStatistics = (id: string, variables?: string[]) => {
  return useQuery({
    queryKey: resultsQueryKeys.statistics(id, variables),
    queryFn: () => resultsService.getStatistics(id, variables),
    enabled: !!id,
    staleTime: 300000, // 5 minutes
    cacheTime: 600000, // 10 minutes
  });
};

// Query result data with filters
export const useQueryResultData = () => {
  return useMutation({
    mutationFn: ({ id, query }: { id: string; query: ResultsQuery }) =>
      resultsService.queryResultData(id, query),
    onError: (error: any) => {
      toast.error(`Failed to query data: ${error.message}`);
    },
  });
};

// Export result data
export const useExportResult = () => {
  return useMutation({
    mutationFn: ({ id, config }: { id: string; config: ExportConfiguration }) =>
      resultsService.exportResult(id, config),
    onSuccess: (blob: Blob, { config }) => {
      // Create download link
      const url = window.URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = url;
      link.download = `export.${config.format}`;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
      window.URL.revokeObjectURL(url);
      
      toast.success('Export completed successfully');
    },
    onError: (error: any) => {
      toast.error(`Failed to export data: ${error.message}`);
    },
  });
};

// Delete result
export const useDeleteResult = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (id: string) => resultsService.deleteResult(id),
    onSuccess: (_, id) => {
      // Remove from cache
      queryClient.removeQueries(resultsQueryKeys.detail(id));
      
      // Invalidate lists
      queryClient.invalidateQueries(resultsQueryKeys.lists());
      
      toast.success('Result deleted successfully');
    },
    onError: (error: any) => {
      toast.error(`Failed to delete result: ${error.message}`);
    },
  });
};

// Custom hook for result data visualization
export const useResultVisualization = (id: string) => {
  const { data: result, isLoading: resultLoading } = useResult(id);
  const { data: metadata, isLoading: metadataLoading } = useResultMetadata(id);
  
  // Get available visualization options
  const availableVariables = metadata?.available_variables || [];
  const availableObjects = metadata?.available_objects || [];
  const dataTypes = metadata?.data_types || [];
  const timeRange = metadata?.time_range || [0, 0];
  
  // Helper functions for building queries
  const buildTimeSeriesQuery = (variables: string[], timeRange?: [number, number]) => ({
    variable_names: variables,
    time_range: timeRange,
    include_statistics: false,
    include_frequency_domain: false,
  });
  
  const buildStatisticsQuery = (variables?: string[]) => ({
    variable_names: variables,
    include_statistics: true,
    include_frequency_domain: false,
  });
  
  const buildFrequencyQuery = (variables: string[]) => ({
    variable_names: variables,
    include_statistics: false,
    include_frequency_domain: true,
  });
  
  return {
    result,
    metadata,
    isLoading: resultLoading || metadataLoading,
    availableVariables,
    availableObjects,
    dataTypes,
    timeRange,
    buildTimeSeriesQuery,
    buildStatisticsQuery,
    buildFrequencyQuery,
  };
};

// Custom hook for data filtering
export const useResultFilters = (id: string) => {
  const { data: metadata } = useResultMetadata(id);
  
  const createFilter = (
    variables?: string[],
    objects?: string[],
    dataTypes?: string[],
    timeRange?: [number, number]
  ): ResultsQuery => ({
    variable_names: variables,
    object_names: objects,
    data_types: dataTypes,
    time_range: timeRange,
    include_statistics: true,
    include_frequency_domain: false,
  });
  
  const availableOptions = {
    variables: metadata?.available_variables || [],
    objects: metadata?.available_objects || [],
    dataTypes: metadata?.data_types || [],
    timeRange: metadata?.time_range || [0, 0],
  };
  
  return {
    createFilter,
    availableOptions,
  };
};

// Custom hook for real-time data updates (if streaming is implemented)
export const useRealtimeResult = (id: string, enabled: boolean = false) => {
  return useQuery({
    queryKey: [...resultsQueryKeys.detail(id), 'realtime'],
    queryFn: () => resultsService.getResult(id),
    enabled: enabled && !!id,
    refetchInterval: enabled ? 5000 : false, // Poll every 5 seconds when enabled
    staleTime: 0, // Always fresh for real-time data
    cacheTime: 0, // Don't cache real-time data
  });
};