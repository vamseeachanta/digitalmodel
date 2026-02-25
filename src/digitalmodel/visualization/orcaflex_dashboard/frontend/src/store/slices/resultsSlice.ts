// Redux slice for results state management
// Manages analysis results, summaries, exports, and caching

import { createSlice, createAsyncThunk, PayloadAction } from '@reduxjs/toolkit';
import {
  AnalysisResults,
  GetResultsRequest,
  ExportResultsRequest,
  LoadingState,
  ApiResponse
} from '../../types/api';
import { apiClient } from '../../services/api';
import type { RootState } from '../index';

// State interface
export interface ResultsState {
  // Data - organized by analysis ID
  results: Record<string, AnalysisResults>;
  summaries: Record<string, any>;
  
  // Loading states
  loading: LoadingState;
  summaryLoading: LoadingState;
  exportLoading: LoadingState;
  
  // Error states
  error: string | null;
  summaryError: string | null;
  exportError: string | null;
  
  // Export management
  pendingExports: Record<string, ExportStatus>;
  completedExports: Record<string, ExportResult>;
  
  // Cache management
  lastFetched: Record<string, string>;
  cacheValid: Record<string, boolean>;
  
  // Real-time updates
  liveResults: Record<string, boolean>;
}

interface ExportStatus {
  analysisId: string;
  format: string;
  progress: number;
  startTime: string;
  estimatedCompletion?: string;
}

interface ExportResult {
  analysisId: string;
  format: string;
  blob: Blob;
  filename: string;
  completedAt: string;
}

// Initial state
const initialState: ResultsState = {
  results: {},
  summaries: {},
  
  loading: 'idle',
  summaryLoading: 'idle',
  exportLoading: 'idle',
  
  error: null,
  summaryError: null,
  exportError: null,
  
  pendingExports: {},
  completedExports: {},
  
  lastFetched: {},
  cacheValid: {},
  
  liveResults: {}
};

// Async thunks
export const fetchResults = createAsyncThunk<
  { analysisId: string; results: AnalysisResults },
  GetResultsRequest,
  { rejectValue: string }
>(
  'results/fetchResults',
  async (params, { rejectWithValue }) => {
    try {
      const response = await apiClient.getResults(params);
      return {
        analysisId: params.analysis_id,
        results: response.data
      };
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to fetch results');
    }
  }
);

export const fetchResultsSummary = createAsyncThunk<
  { analysisId: string; summary: any },
  string,
  { rejectValue: string }
>(
  'results/fetchResultsSummary',
  async (analysisId, { rejectWithValue }) => {
    try {
      const response = await apiClient.getResultsSummary(analysisId);
      return {
        analysisId,
        summary: response.data
      };
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to fetch results summary');
    }
  }
);

export const exportResults = createAsyncThunk<
  { analysisId: string; blob: Blob; format: string },
  ExportResultsRequest,
  { rejectValue: string }
>(
  'results/exportResults',
  async (params, { rejectWithValue, dispatch }) => {
    try {
      // Start tracking export progress
      dispatch(startExport({
        analysisId: params.analysis_id,
        format: params.format,
        startTime: new Date().toISOString()
      }));

      const blob = await apiClient.exportResults(params);
      
      return {
        analysisId: params.analysis_id,
        blob,
        format: params.format
      };
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to export results');
    }
  }
);

// Slice
const resultsSlice = createSlice({
  name: 'results',
  initialState,
  reducers: {
    // Cache management
    invalidateResults: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      state.cacheValid[analysisId] = false;
      delete state.lastFetched[analysisId];
    },
    
    clearResults: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      delete state.results[analysisId];
      delete state.summaries[analysisId];
      delete state.lastFetched[analysisId];
      delete state.cacheValid[analysisId];
      delete state.liveResults[analysisId];
    },
    
    clearAllResults: (state) => {
      state.results = {};
      state.summaries = {};
      state.lastFetched = {};
      state.cacheValid = {};
      state.liveResults = {};
    },
    
    // Live results management
    enableLiveResults: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      state.liveResults[analysisId] = true;
    },
    
    disableLiveResults: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      state.liveResults[analysisId] = false;
    },
    
    // Export management
    startExport: (state, action: PayloadAction<{ analysisId: string; format: string; startTime: string }>) => {
      const { analysisId, format, startTime } = action.payload;
      state.pendingExports[`${analysisId}_${format}`] = {
        analysisId,
        format,
        progress: 0,
        startTime
      };
    },
    
    updateExportProgress: (state, action: PayloadAction<{ analysisId: string; format: string; progress: number; estimatedCompletion?: string }>) => {
      const { analysisId, format, progress, estimatedCompletion } = action.payload;
      const key = `${analysisId}_${format}`;
      
      if (state.pendingExports[key]) {
        state.pendingExports[key].progress = progress;
        if (estimatedCompletion) {
          state.pendingExports[key].estimatedCompletion = estimatedCompletion;
        }
      }
    },
    
    completeExport: (state, action: PayloadAction<{ analysisId: string; format: string; blob: Blob; filename: string }>) => {
      const { analysisId, format, blob, filename } = action.payload;
      const key = `${analysisId}_${format}`;
      
      // Remove from pending
      delete state.pendingExports[key];
      
      // Add to completed
      state.completedExports[key] = {
        analysisId,
        format,
        blob,
        filename,
        completedAt: new Date().toISOString()
      };
    },
    
    removeCompletedExport: (state, action: PayloadAction<{ analysisId: string; format: string }>) => {
      const { analysisId, format } = action.payload;
      const key = `${analysisId}_${format}`;
      delete state.completedExports[key];
    },
    
    // Real-time result updates
    updateResultsData: (state, action: PayloadAction<{ analysisId: string; updates: Partial<AnalysisResults> }>) => {
      const { analysisId, updates } = action.payload;
      
      if (state.results[analysisId] && state.liveResults[analysisId]) {
        state.results[analysisId] = {
          ...state.results[analysisId],
          ...updates
        };
      }
    },
    
    // Error management
    clearError: (state, action: PayloadAction<'error' | 'summaryError' | 'exportError'>) => {
      state[action.payload] = null;
    },
    
    clearAllErrors: (state) => {
      state.error = null;
      state.summaryError = null;
      state.exportError = null;
    }
  },
  extraReducers: (builder) => {
    // Fetch results
    builder
      .addCase(fetchResults.pending, (state) => {
        state.loading = 'loading';
        state.error = null;
      })
      .addCase(fetchResults.fulfilled, (state, action) => {
        state.loading = 'succeeded';
        const { analysisId, results } = action.payload;
        
        state.results[analysisId] = results;
        state.lastFetched[analysisId] = new Date().toISOString();
        state.cacheValid[analysisId] = true;
      })
      .addCase(fetchResults.rejected, (state, action) => {
        state.loading = 'failed';
        state.error = action.payload || 'Failed to fetch results';
      });

    // Fetch results summary
    builder
      .addCase(fetchResultsSummary.pending, (state) => {
        state.summaryLoading = 'loading';
        state.summaryError = null;
      })
      .addCase(fetchResultsSummary.fulfilled, (state, action) => {
        state.summaryLoading = 'succeeded';
        const { analysisId, summary } = action.payload;
        
        state.summaries[analysisId] = summary;
      })
      .addCase(fetchResultsSummary.rejected, (state, action) => {
        state.summaryLoading = 'failed';
        state.summaryError = action.payload || 'Failed to fetch results summary';
      });

    // Export results
    builder
      .addCase(exportResults.pending, (state) => {
        state.exportLoading = 'loading';
        state.exportError = null;
      })
      .addCase(exportResults.fulfilled, (state, action) => {
        state.exportLoading = 'succeeded';
        const { analysisId, blob, format } = action.payload;
        
        // Generate filename
        const timestamp = new Date().toISOString().split('T')[0];
        const filename = `analysis_${analysisId}_results_${timestamp}.${format}`;
        
        resultsSlice.caseReducers.completeExport(state, {
          type: 'results/completeExport',
          payload: { analysisId, format, blob, filename }
        });
      })
      .addCase(exportResults.rejected, (state, action) => {
        state.exportLoading = 'failed';
        state.exportError = action.payload || 'Failed to export results';
      });
  }
});

// Selectors
export const selectResults = (state: RootState, analysisId: string) => 
  state.results.results[analysisId] || null;

export const selectResultsSummary = (state: RootState, analysisId: string) =>
  state.results.summaries[analysisId] || null;

export const selectResultsLoading = (state: RootState) => state.results.loading;
export const selectResultsSummaryLoading = (state: RootState) => state.results.summaryLoading;
export const selectResultsExportLoading = (state: RootState) => state.results.exportLoading;

export const selectResultsError = (state: RootState) => state.results.error;
export const selectResultsSummaryError = (state: RootState) => state.results.summaryError;
export const selectResultsExportError = (state: RootState) => state.results.exportError;

export const selectResultsCached = (state: RootState, analysisId: string) =>
  state.results.cacheValid[analysisId] || false;

export const selectResultsLastFetched = (state: RootState, analysisId: string) =>
  state.results.lastFetched[analysisId] || null;

export const selectLiveResultsEnabled = (state: RootState, analysisId: string) =>
  state.results.liveResults[analysisId] || false;

// Export selectors
export const selectPendingExports = (state: RootState) => state.results.pendingExports;
export const selectCompletedExports = (state: RootState) => state.results.completedExports;

export const selectPendingExportsByAnalysis = (state: RootState, analysisId: string) =>
  Object.values(state.results.pendingExports).filter(exp => exp.analysisId === analysisId);

export const selectCompletedExportsByAnalysis = (state: RootState, analysisId: string) =>
  Object.values(state.results.completedExports).filter(exp => exp.analysisId === analysisId);

export const selectExportStatus = (state: RootState, analysisId: string, format: string) =>
  state.results.pendingExports[`${analysisId}_${format}`] || null;

export const selectCompletedExport = (state: RootState, analysisId: string, format: string) =>
  state.results.completedExports[`${analysisId}_${format}`] || null;

// Derived selectors
export const selectResultsAvailable = (state: RootState, analysisId: string) =>
  !!state.results.results[analysisId];

export const selectResultsStale = (state: RootState, analysisId: string, maxAgeMs = 5 * 60 * 1000) => {
  const lastFetched = state.results.lastFetched[analysisId];
  if (!lastFetched) return true;
  
  const age = Date.now() - new Date(lastFetched).getTime();
  return age > maxAgeMs;
};

export const selectResultsComponentCount = (state: RootState, analysisId: string) => {
  const results = state.results.results[analysisId];
  return results?.component_results?.length || 0;
};

export const selectResultsTimeSeriesCount = (state: RootState, analysisId: string) => {
  const results = state.results.results[analysisId];
  return results?.time_series?.length || 0;
};

export const selectResultsTypes = (state: RootState, analysisId: string) => {
  const results = state.results.results[analysisId];
  if (!results?.component_results) return [];
  
  const types = new Set<string>();
  results.component_results.forEach(cr => {
    Object.keys(cr.results).forEach(type => {
      if (cr.results[type as keyof typeof cr.results]) {
        types.add(type);
      }
    });
  });
  
  return Array.from(types);
};

export const selectResultsComponents = (state: RootState, analysisId: string) => {
  const results = state.results.results[analysisId];
  return results?.component_results?.map(cr => ({
    id: cr.component_id,
    name: cr.component_name,
    type: cr.component_type
  })) || [];
};

// Statistics selectors
export const selectResultsStats = (state: RootState, analysisId: string) => {
  const results = state.results.results[analysisId];
  const summary = state.results.summaries[analysisId];
  
  if (!results) return null;
  
  return {
    componentCount: results.component_results?.length || 0,
    timeSeriesCount: results.time_series?.length || 0,
    hasStatistics: !!results.summary_statistics,
    hasExtremes: !!results.extreme_values,
    hasFatigue: !!results.fatigue_analysis,
    hasFrequencyDomain: !!results.frequency_domain,
    generatedAt: results.generated_at,
    summaryAvailable: !!summary
  };
};

// Performance selectors (memoized for expensive operations)
export const selectResultsChartData = (state: RootState, analysisId: string, componentId: string, resultType: string) => {
  const results = state.results.results[analysisId];
  if (!results?.time_series) return null;
  
  const timeSeries = results.time_series.find(ts => 
    ts.component_id === componentId && ts.result_type === resultType
  );
  
  if (!timeSeries) return null;
  
  return {
    labels: timeSeries.timestamp.map(t => t.toFixed(2)),
    data: timeSeries.values,
    units: timeSeries.units
  };
};

// Export actions
export const {
  invalidateResults,
  clearResults,
  clearAllResults,
  enableLiveResults,
  disableLiveResults,
  startExport,
  updateExportProgress,
  completeExport,
  removeCompletedExport,
  updateResultsData,
  clearError,
  clearAllErrors
} = resultsSlice.actions;

// Export reducer
export default resultsSlice.reducer;