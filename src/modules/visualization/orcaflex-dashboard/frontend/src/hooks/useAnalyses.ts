/**
 * React hooks for analysis operations
 */

import { useQuery, useMutation, useQueryClient } from 'react-query';
import { toast } from 'react-toastify';

import { analysisService } from '@/services/analysisService';
import {
  Analysis,
  AnalysisRequest,
  AnalysisSummary,
  AnalysisResult,
  AnalysisProgress,
  AnalysisFilters,
  AnalysisPagination,
  FileUploadResponse,
} from '@/types/analysis';

// Query keys for cache management
export const analysisQueryKeys = {
  all: ['analyses'] as const,
  lists: () => [...analysisQueryKeys.all, 'list'] as const,
  list: (filters: AnalysisFilters, pagination: AnalysisPagination) =>
    [...analysisQueryKeys.lists(), filters, pagination] as const,
  details: () => [...analysisQueryKeys.all, 'detail'] as const,
  detail: (id: string) => [...analysisQueryKeys.details(), id] as const,
  results: (id: string) => [...analysisQueryKeys.detail(id), 'results'] as const,
  progress: (id: string) => [...analysisQueryKeys.detail(id), 'progress'] as const,
  logs: (id: string) => [...analysisQueryKeys.detail(id), 'logs'] as const,
};

// Get list of analyses
export const useAnalyses = (
  filters: AnalysisFilters = {},
  pagination: AnalysisPagination = { limit: 50, offset: 0 }
) => {
  return useQuery({
    queryKey: analysisQueryKeys.list(filters, pagination),
    queryFn: () => analysisService.getAnalyses(filters, pagination),
    staleTime: 30000, // 30 seconds
    cacheTime: 300000, // 5 minutes
    keepPreviousData: true,
  });
};

// Get single analysis
export const useAnalysis = (id: string) => {
  return useQuery({
    queryKey: analysisQueryKeys.detail(id),
    queryFn: () => analysisService.getAnalysis(id),
    enabled: !!id,
    staleTime: 60000, // 1 minute
    cacheTime: 300000, // 5 minutes
  });
};

// Get analysis results
export const useAnalysisResults = (id: string) => {
  return useQuery({
    queryKey: analysisQueryKeys.results(id),
    queryFn: () => analysisService.getAnalysisResults(id),
    enabled: !!id,
    staleTime: 60000, // 1 minute
    cacheTime: 600000, // 10 minutes
  });
};

// Get analysis progress (for running analyses)
export const useAnalysisProgress = (id: string, enabled: boolean = false) => {
  return useQuery({
    queryKey: analysisQueryKeys.progress(id),
    queryFn: () => analysisService.getAnalysisProgress(id),
    enabled: enabled && !!id,
    refetchInterval: enabled ? 2000 : false, // Poll every 2 seconds when enabled
    staleTime: 0, // Always fresh for progress updates
    cacheTime: 0, // Don't cache progress data
  });
};

// Get analysis logs
export const useAnalysisLogs = (id: string, lines: number = 100) => {
  return useQuery({
    queryKey: [...analysisQueryKeys.logs(id), lines],
    queryFn: () => analysisService.getAnalysisLogs(id, lines),
    enabled: !!id,
    staleTime: 10000, // 10 seconds
    cacheTime: 60000, // 1 minute
  });
};

// Create new analysis
export const useCreateAnalysis = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (request: AnalysisRequest) => analysisService.createAnalysis(request),
    onSuccess: (data: Analysis) => {
      // Invalidate analyses list
      queryClient.invalidateQueries(analysisQueryKeys.lists());
      
      // Add to cache
      queryClient.setQueryData(analysisQueryKeys.detail(data.id), data);
      
      toast.success('Analysis created successfully');
    },
    onError: (error: any) => {
      toast.error(`Failed to create analysis: ${error.message}`);
    },
  });
};

// Update analysis
export const useUpdateAnalysis = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: ({ id, request }: { id: string; request: AnalysisRequest }) =>
      analysisService.updateAnalysis(id, request),
    onSuccess: (data: Analysis) => {
      // Update cache
      queryClient.setQueryData(analysisQueryKeys.detail(data.id), data);
      
      // Invalidate lists
      queryClient.invalidateQueries(analysisQueryKeys.lists());
      
      toast.success('Analysis updated successfully');
    },
    onError: (error: any) => {
      toast.error(`Failed to update analysis: ${error.message}`);
    },
  });
};

// Delete analysis
export const useDeleteAnalysis = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (id: string) => analysisService.deleteAnalysis(id),
    onSuccess: (_, id) => {
      // Remove from cache
      queryClient.removeQueries(analysisQueryKeys.detail(id));
      
      // Invalidate lists
      queryClient.invalidateQueries(analysisQueryKeys.lists());
      
      toast.success('Analysis deleted successfully');
    },
    onError: (error: any) => {
      toast.error(`Failed to delete analysis: ${error.message}`);
    },
  });
};

// Run analysis
export const useRunAnalysis = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (id: string) => analysisService.runAnalysis(id),
    onSuccess: (_, id) => {
      // Invalidate analysis detail to get updated status
      queryClient.invalidateQueries(analysisQueryKeys.detail(id));
      
      // Invalidate lists
      queryClient.invalidateQueries(analysisQueryKeys.lists());
      
      toast.success('Analysis started successfully');
    },
    onError: (error: any) => {
      toast.error(`Failed to start analysis: ${error.message}`);
    },
  });
};

// Cancel analysis
export const useCancelAnalysis = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (id: string) => analysisService.cancelAnalysis(id),
    onSuccess: (_, id) => {
      // Invalidate analysis detail
      queryClient.invalidateQueries(analysisQueryKeys.detail(id));
      
      // Invalidate lists
      queryClient.invalidateQueries(analysisQueryKeys.lists());
      
      toast.success('Analysis cancelled successfully');
    },
    onError: (error: any) => {
      toast.error(`Failed to cancel analysis: ${error.message}`);
    },
  });
};

// Upload file
export const useUploadFile = () => {
  return useMutation({
    mutationFn: (file: File) => analysisService.uploadFile(file),
    onSuccess: (data: FileUploadResponse) => {
      toast.success(`File uploaded: ${data.filename}`);
    },
    onError: (error: any) => {
      toast.error(`Failed to upload file: ${error.message}`);
    },
  });
};

// Custom hook for managing analysis lifecycle
export const useAnalysisLifecycle = (id: string) => {
  const { data: analysis } = useAnalysis(id);
  const { data: progress } = useAnalysisProgress(
    id, 
    analysis?.status === 'running'
  );
  const { data: results } = useAnalysisResults(id);
  
  const runMutation = useRunAnalysis();
  const cancelMutation = useCancelAnalysis();
  
  const canRun = analysis?.status === 'pending' || analysis?.status === 'failed';
  const canCancel = analysis?.status === 'running';
  const isCompleted = analysis?.status === 'completed';
  const hasResults = isCompleted && !!results;
  
  return {
    analysis,
    progress,
    results,
    canRun,
    canCancel,
    isCompleted,
    hasResults,
    run: () => runMutation.mutate(id),
    cancel: () => cancelMutation.mutate(id),
    isRunning: runMutation.isLoading,
    isCancelling: cancelMutation.isLoading,
  };
};