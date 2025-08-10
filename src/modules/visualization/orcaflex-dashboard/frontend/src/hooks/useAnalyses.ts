// Custom hook for analysis operations
// Provides state management, CRUD operations, and real-time updates for analyses

import { useState, useEffect, useCallback, useMemo } from 'react';
import { useSelector, useDispatch } from 'react-redux';
import {
  Analysis,
  AnalysisStatus,
  CreateAnalysisRequest,
  UpdateAnalysisRequest,
  RunAnalysisRequest,
  GetAnalysesRequest,
  LoadingState,
  AnalysisStatusUpdate,
  AnalysisProgressUpdate
} from '../types/api';
import { apiClient, wsClient } from '../services/api';
import {
  fetchAnalyses,
  fetchAnalysis,
  createAnalysis,
  updateAnalysis,
  deleteAnalysis,
  runAnalysis,
  cancelAnalysis,
  updateAnalysisStatus,
  updateAnalysisProgress,
  selectAnalyses,
  selectAnalysisById,
  selectAnalysesLoading,
  selectAnalysesError
} from '../store/slices/analysisSlice';
import type { RootState, AppDispatch } from '../store';

export interface UseAnalysesOptions {
  autoRefresh?: boolean;
  refreshInterval?: number;
  subscribeToUpdates?: boolean;
  initialFilters?: Partial<GetAnalysesRequest>;
}

export interface UseAnalysesReturn {
  // Data
  analyses: Analysis[];
  totalCount: number;
  currentPage: number;
  totalPages: number;
  
  // Loading states
  loading: LoadingState;
  creating: boolean;
  updating: boolean;
  deleting: boolean;
  running: boolean;
  
  // Error states
  error: string | null;
  
  // Filter and pagination
  filters: GetAnalysesRequest;
  setFilters: (filters: Partial<GetAnalysesRequest>) => void;
  resetFilters: () => void;
  
  // CRUD operations
  loadAnalyses: (params?: GetAnalysesRequest) => Promise<void>;
  loadAnalysis: (id: string) => Promise<Analysis | null>;
  createNewAnalysis: (data: CreateAnalysisRequest) => Promise<Analysis | null>;
  updateExistingAnalysis: (id: string, data: UpdateAnalysisRequest) => Promise<Analysis | null>;
  deleteExistingAnalysis: (id: string) => Promise<boolean>;
  
  // Analysis control operations
  runExistingAnalysis: (id: string, options?: RunAnalysisRequest) => Promise<boolean>;
  cancelExistingAnalysis: (id: string) => Promise<boolean>;
  
  // Utility functions
  refreshAnalyses: () => Promise<void>;
  getAnalysisByStatus: (status: AnalysisStatus) => Analysis[];
  getRunningAnalyses: () => Analysis[];
  getCompletedAnalyses: () => Analysis[];
  getFailedAnalyses: () => Analysis[];
  
  // Pagination
  goToPage: (page: number) => void;
  goToNextPage: () => void;
  goToPreviousPage: () => void;
  
  // Search and sorting
  searchAnalyses: (query: string) => void;
  sortAnalyses: (field: string, direction?: 'asc' | 'desc') => void;
  
  // Real-time updates
  isConnected: boolean;
  connectionError: string | null;
}

const DEFAULT_FILTERS: GetAnalysesRequest = {
  page: 1,
  limit: 20,
  sort_by: 'updated_at',
  sort_order: 'desc'
};

export const useAnalyses = (options: UseAnalysesOptions = {}): UseAnalysesReturn => {
  const {
    autoRefresh = false,
    refreshInterval = 30000, // 30 seconds
    subscribeToUpdates = true,
    initialFilters = {}
  } = options;

  const dispatch = useDispatch<AppDispatch>();
  
  // Redux selectors
  const analyses = useSelector((state: RootState) => selectAnalyses(state));
  const loading = useSelector((state: RootState) => selectAnalysesLoading(state));
  const error = useSelector((state: RootState) => selectAnalysesError(state));

  // Local state
  const [filters, setFiltersState] = useState<GetAnalysesRequest>({
    ...DEFAULT_FILTERS,
    ...initialFilters
  });
  const [creating, setCreating] = useState(false);
  const [updating, setUpdating] = useState(false);
  const [deleting, setDeleting] = useState(false);
  const [running, setRunning] = useState(false);
  const [isConnected, setIsConnected] = useState(false);
  const [connectionError, setConnectionError] = useState<string | null>(null);

  // Computed values
  const totalCount = useSelector((state: RootState) => state.analyses.totalCount);
  const currentPage = filters.page || 1;
  const totalPages = Math.ceil(totalCount / (filters.limit || 20));

  // Filter management
  const setFilters = useCallback((newFilters: Partial<GetAnalysesRequest>) => {
    setFiltersState(prev => ({
      ...prev,
      ...newFilters,
      page: newFilters.page !== undefined ? newFilters.page : 1 // Reset to page 1 unless explicitly set
    }));
  }, []);

  const resetFilters = useCallback(() => {
    setFiltersState({ ...DEFAULT_FILTERS, ...initialFilters });
  }, [initialFilters]);

  // Load analyses
  const loadAnalyses = useCallback(async (params?: GetAnalysesRequest) => {
    const finalParams = { ...filters, ...params };
    try {
      await dispatch(fetchAnalyses(finalParams)).unwrap();
    } catch (err) {
      console.error('Failed to load analyses:', err);
    }
  }, [dispatch, filters]);

  // Load single analysis
  const loadAnalysis = useCallback(async (id: string): Promise<Analysis | null> => {
    try {
      const result = await dispatch(fetchAnalysis(id)).unwrap();
      return result;
    } catch (err) {
      console.error('Failed to load analysis:', err);
      return null;
    }
  }, [dispatch]);

  // Create analysis
  const createNewAnalysis = useCallback(async (data: CreateAnalysisRequest): Promise<Analysis | null> => {
    setCreating(true);
    try {
      const result = await dispatch(createAnalysis(data)).unwrap();
      await loadAnalyses(); // Refresh the list
      return result;
    } catch (err) {
      console.error('Failed to create analysis:', err);
      return null;
    } finally {
      setCreating(false);
    }
  }, [dispatch, loadAnalyses]);

  // Update analysis
  const updateExistingAnalysis = useCallback(async (id: string, data: UpdateAnalysisRequest): Promise<Analysis | null> => {
    setUpdating(true);
    try {
      const result = await dispatch(updateAnalysis({ id, data })).unwrap();
      return result;
    } catch (err) {
      console.error('Failed to update analysis:', err);
      return null;
    } finally {
      setUpdating(false);
    }
  }, [dispatch]);

  // Delete analysis
  const deleteExistingAnalysis = useCallback(async (id: string): Promise<boolean> => {
    setDeleting(true);
    try {
      await dispatch(deleteAnalysis(id)).unwrap();
      await loadAnalyses(); // Refresh the list
      return true;
    } catch (err) {
      console.error('Failed to delete analysis:', err);
      return false;
    } finally {
      setDeleting(false);
    }
  }, [dispatch, loadAnalyses]);

  // Run analysis
  const runExistingAnalysis = useCallback(async (id: string, options?: RunAnalysisRequest): Promise<boolean> => {
    setRunning(true);
    try {
      await dispatch(runAnalysis({ id, options })).unwrap();
      return true;
    } catch (err) {
      console.error('Failed to run analysis:', err);
      return false;
    } finally {
      setRunning(false);
    }
  }, [dispatch]);

  // Cancel analysis
  const cancelExistingAnalysis = useCallback(async (id: string): Promise<boolean> => {
    try {
      await dispatch(cancelAnalysis(id)).unwrap();
      return true;
    } catch (err) {
      console.error('Failed to cancel analysis:', err);
      return false;
    }
  }, [dispatch]);

  // Refresh analyses
  const refreshAnalyses = useCallback(async () => {
    await loadAnalyses();
  }, [loadAnalyses]);

  // Utility functions
  const getAnalysisByStatus = useCallback((status: AnalysisStatus): Analysis[] => {
    return analyses.filter(analysis => analysis.status === status);
  }, [analyses]);

  const getRunningAnalyses = useCallback((): Analysis[] => {
    return analyses.filter(analysis => 
      analysis.status === 'running' || analysis.status === 'queued'
    );
  }, [analyses]);

  const getCompletedAnalyses = useCallback((): Analysis[] => {
    return getAnalysisByStatus('completed');
  }, [getAnalysisByStatus]);

  const getFailedAnalyses = useCallback((): Analysis[] => {
    return getAnalysisByStatus('failed');
  }, [getAnalysisByStatus]);

  // Pagination functions
  const goToPage = useCallback((page: number) => {
    if (page >= 1 && page <= totalPages) {
      setFilters({ page });
    }
  }, [totalPages, setFilters]);

  const goToNextPage = useCallback(() => {
    if (currentPage < totalPages) {
      goToPage(currentPage + 1);
    }
  }, [currentPage, totalPages, goToPage]);

  const goToPreviousPage = useCallback(() => {
    if (currentPage > 1) {
      goToPage(currentPage - 1);
    }
  }, [currentPage, goToPage]);

  // Search and sorting
  const searchAnalyses = useCallback((query: string) => {
    setFilters({ search: query, page: 1 });
  }, [setFilters]);

  const sortAnalyses = useCallback((field: string, direction: 'asc' | 'desc' = 'desc') => {
    setFilters({ 
      sort_by: field as any,
      sort_order: direction,
      page: 1
    });
  }, [setFilters]);

  // WebSocket subscription for real-time updates
  useEffect(() => {
    if (!subscribeToUpdates) return;

    const unsubscribeStatus = wsClient.subscribe('analysis_status_update', (data: AnalysisStatusUpdate) => {
      dispatch(updateAnalysisStatus(data));
    });

    const unsubscribeProgress = wsClient.subscribe('analysis_progress_update', (data: AnalysisProgressUpdate) => {
      dispatch(updateAnalysisProgress(data));
    });

    const unsubscribeConnection = wsClient.subscribe('connection_status', (data: { connected: boolean }) => {
      setIsConnected(data.connected);
      setConnectionError(data.connected ? null : 'WebSocket connection lost');
    });

    const unsubscribeError = wsClient.subscribe('error', (data: { error: string }) => {
      setConnectionError(data.error);
    });

    // Connect WebSocket
    wsClient.connect();

    return () => {
      unsubscribeStatus();
      unsubscribeProgress();
      unsubscribeConnection();
      unsubscribeError();
    };
  }, [subscribeToUpdates, dispatch]);

  // Auto-refresh effect
  useEffect(() => {
    if (!autoRefresh) return;

    const interval = setInterval(() => {
      // Only refresh if not currently loading
      if (loading !== 'loading') {
        refreshAnalyses();
      }
    }, refreshInterval);

    return () => clearInterval(interval);
  }, [autoRefresh, refreshInterval, loading, refreshAnalyses]);

  // Load analyses when filters change
  useEffect(() => {
    loadAnalyses(filters);
  }, [filters, loadAnalyses]);

  // Initial load
  useEffect(() => {
    loadAnalyses();
  }, []);

  return {
    // Data
    analyses,
    totalCount,
    currentPage,
    totalPages,
    
    // Loading states
    loading,
    creating,
    updating,
    deleting,
    running,
    
    // Error states
    error,
    
    // Filter and pagination
    filters,
    setFilters,
    resetFilters,
    
    // CRUD operations
    loadAnalyses,
    loadAnalysis,
    createNewAnalysis,
    updateExistingAnalysis,
    deleteExistingAnalysis,
    
    // Analysis control operations
    runExistingAnalysis,
    cancelExistingAnalysis,
    
    // Utility functions
    refreshAnalyses,
    getAnalysisByStatus,
    getRunningAnalyses,
    getCompletedAnalyses,
    getFailedAnalyses,
    
    // Pagination
    goToPage,
    goToNextPage,
    goToPreviousPage,
    
    // Search and sorting
    searchAnalyses,
    sortAnalyses,
    
    // Real-time updates
    isConnected,
    connectionError
  };
};

// Specialized hook for single analysis
export interface UseSingleAnalysisOptions {
  analysisId: string;
  subscribeToUpdates?: boolean;
  autoRefresh?: boolean;
  refreshInterval?: number;
}

export const useSingleAnalysis = (options: UseSingleAnalysisOptions) => {
  const { analysisId, subscribeToUpdates = true, autoRefresh = false, refreshInterval = 30000 } = options;
  
  const dispatch = useDispatch<AppDispatch>();
  const analysis = useSelector((state: RootState) => selectAnalysisById(state, analysisId));
  
  const [loading, setLoading] = useState<LoadingState>('idle');
  const [error, setError] = useState<string | null>(null);

  const loadAnalysis = useCallback(async () => {
    if (!analysisId) return;
    
    setLoading('loading');
    try {
      await dispatch(fetchAnalysis(analysisId)).unwrap();
      setError(null);
      setLoading('succeeded');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load analysis');
      setLoading('failed');
    }
  }, [dispatch, analysisId]);

  // WebSocket subscription for this specific analysis
  useEffect(() => {
    if (!subscribeToUpdates || !analysisId) return;

    const unsubscribeStatus = wsClient.subscribe('analysis_status_update', (data: AnalysisStatusUpdate) => {
      if (data.analysis_id === analysisId) {
        dispatch(updateAnalysisStatus(data));
      }
    });

    const unsubscribeProgress = wsClient.subscribe('analysis_progress_update', (data: AnalysisProgressUpdate) => {
      if (data.analysis_id === analysisId) {
        dispatch(updateAnalysisProgress(data));
      }
    });

    return () => {
      unsubscribeStatus();
      unsubscribeProgress();
    };
  }, [subscribeToUpdates, analysisId, dispatch]);

  // Auto-refresh effect
  useEffect(() => {
    if (!autoRefresh || !analysisId) return;

    const interval = setInterval(() => {
      if (loading !== 'loading') {
        loadAnalysis();
      }
    }, refreshInterval);

    return () => clearInterval(interval);
  }, [autoRefresh, refreshInterval, loading, loadAnalysis, analysisId]);

  // Initial load
  useEffect(() => {
    if (analysisId) {
      loadAnalysis();
    }
  }, [analysisId, loadAnalysis]);

  return {
    analysis,
    loading,
    error,
    refresh: loadAnalysis,
    isRunning: analysis?.status === 'running' || analysis?.status === 'queued',
    isCompleted: analysis?.status === 'completed',
    isFailed: analysis?.status === 'failed',
    progress: analysis?.progress || 0
  };
};