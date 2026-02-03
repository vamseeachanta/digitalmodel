// Redux slice for analysis state management
// Manages analysis CRUD operations, status updates, and real-time progress tracking

import { createSlice, createAsyncThunk, PayloadAction } from '@reduxjs/toolkit';
import {
  Analysis,
  AnalysisStatus,
  CreateAnalysisRequest,
  UpdateAnalysisRequest,
  RunAnalysisRequest,
  GetAnalysesRequest,
  PaginatedResponse,
  ApiResponse,
  LoadingState,
  AnalysisStatusUpdate,
  AnalysisProgressUpdate
} from '../../types/api';
import { apiClient } from '../../services/api';
import type { RootState } from '../index';

// State interface
export interface AnalysisState {
  // Data
  analyses: Analysis[];
  analysisMap: Record<string, Analysis>;
  totalCount: number;
  currentPage: number;
  totalPages: number;
  
  // Loading states
  loading: LoadingState;
  createLoading: LoadingState;
  updateLoading: LoadingState;
  deleteLoading: LoadingState;
  runLoading: LoadingState;
  
  // Error states
  error: string | null;
  createError: string | null;
  updateError: string | null;
  deleteError: string | null;
  runError: string | null;
  
  // Filters and pagination
  filters: GetAnalysesRequest;
  
  // Real-time updates
  statusUpdates: Record<string, AnalysisStatusUpdate>;
  progressUpdates: Record<string, AnalysisProgressUpdate>;
  
  // Cache management
  lastFetched: string | null;
  cacheValid: boolean;
}

// Initial state
const initialState: AnalysisState = {
  analyses: [],
  analysisMap: {},
  totalCount: 0,
  currentPage: 1,
  totalPages: 1,
  
  loading: 'idle',
  createLoading: 'idle',
  updateLoading: 'idle',
  deleteLoading: 'idle',
  runLoading: 'idle',
  
  error: null,
  createError: null,
  updateError: null,
  deleteError: null,
  runError: null,
  
  filters: {
    page: 1,
    limit: 20,
    sort_by: 'updated_at',
    sort_order: 'desc'
  },
  
  statusUpdates: {},
  progressUpdates: {},
  
  lastFetched: null,
  cacheValid: false
};

// Async thunks
export const fetchAnalyses = createAsyncThunk<
  PaginatedResponse<Analysis>,
  GetAnalysesRequest,
  { rejectValue: string }
>(
  'analysis/fetchAnalyses',
  async (params, { rejectWithValue }) => {
    try {
      const response = await apiClient.getAnalyses(params);
      return response;
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to fetch analyses');
    }
  }
);

export const fetchAnalysis = createAsyncThunk<
  Analysis,
  string,
  { rejectValue: string }
>(
  'analysis/fetchAnalysis',
  async (id, { rejectWithValue }) => {
    try {
      const response = await apiClient.getAnalysis(id);
      return response.data;
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to fetch analysis');
    }
  }
);

export const createAnalysis = createAsyncThunk<
  Analysis,
  CreateAnalysisRequest,
  { rejectValue: string }
>(
  'analysis/createAnalysis',
  async (data, { rejectWithValue }) => {
    try {
      const response = await apiClient.createAnalysis(data);
      return response.data;
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to create analysis');
    }
  }
);

export const updateAnalysis = createAsyncThunk<
  Analysis,
  { id: string; data: UpdateAnalysisRequest },
  { rejectValue: string }
>(
  'analysis/updateAnalysis',
  async ({ id, data }, { rejectWithValue }) => {
    try {
      const response = await apiClient.updateAnalysis(id, data);
      return response.data;
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to update analysis');
    }
  }
);

export const deleteAnalysis = createAsyncThunk<
  string,
  string,
  { rejectValue: string }
>(
  'analysis/deleteAnalysis',
  async (id, { rejectWithValue }) => {
    try {
      await apiClient.deleteAnalysis(id);
      return id;
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to delete analysis');
    }
  }
);

export const runAnalysis = createAsyncThunk<
  Analysis,
  { id: string; options?: RunAnalysisRequest },
  { rejectValue: string }
>(
  'analysis/runAnalysis',
  async ({ id, options }, { rejectWithValue }) => {
    try {
      const response = await apiClient.runAnalysis(id, options);
      return response.data;
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to run analysis');
    }
  }
);

export const cancelAnalysis = createAsyncThunk<
  Analysis,
  string,
  { rejectValue: string }
>(
  'analysis/cancelAnalysis',
  async (id, { rejectWithValue }) => {
    try {
      const response = await apiClient.cancelAnalysis(id);
      return response.data;
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to cancel analysis');
    }
  }
);

// Slice
const analysisSlice = createSlice({
  name: 'analysis',
  initialState,
  reducers: {
    // Filter management
    setFilters: (state, action: PayloadAction<Partial<GetAnalysesRequest>>) => {
      state.filters = { ...state.filters, ...action.payload };
      state.cacheValid = false;
    },
    
    resetFilters: (state) => {
      state.filters = {
        page: 1,
        limit: 20,
        sort_by: 'updated_at',
        sort_order: 'desc'
      };
      state.cacheValid = false;
    },
    
    // Real-time updates
    updateAnalysisStatus: (state, action: PayloadAction<AnalysisStatusUpdate>) => {
      const { analysis_id, status, progress, message, error } = action.payload;
      
      // Update in analyses array
      const analysisIndex = state.analyses.findIndex(a => a.id === analysis_id);
      if (analysisIndex !== -1) {
        state.analyses[analysisIndex].status = status;
        if (progress !== undefined) {
          state.analyses[analysisIndex].progress = progress;
        }
        if (error) {
          state.analyses[analysisIndex].error_message = error;
        }
        state.analyses[analysisIndex].updated_at = new Date().toISOString();
      }
      
      // Update in analysis map
      if (state.analysisMap[analysis_id]) {
        state.analysisMap[analysis_id].status = status;
        if (progress !== undefined) {
          state.analysisMap[analysis_id].progress = progress;
        }
        if (error) {
          state.analysisMap[analysis_id].error_message = error;
        }
        state.analysisMap[analysis_id].updated_at = new Date().toISOString();
      }
      
      // Store status update
      state.statusUpdates[analysis_id] = action.payload;
    },
    
    updateAnalysisProgress: (state, action: PayloadAction<AnalysisProgressUpdate>) => {
      const { analysis_id, progress, current_step, estimated_completion } = action.payload;
      
      // Update in analyses array
      const analysisIndex = state.analyses.findIndex(a => a.id === analysis_id);
      if (analysisIndex !== -1) {
        state.analyses[analysisIndex].progress = progress;
        state.analyses[analysisIndex].updated_at = new Date().toISOString();
      }
      
      // Update in analysis map
      if (state.analysisMap[analysis_id]) {
        state.analysisMap[analysis_id].progress = progress;
        state.analysisMap[analysis_id].updated_at = new Date().toISOString();
      }
      
      // Store progress update
      state.progressUpdates[analysis_id] = action.payload;
    },
    
    // Cache management
    invalidateCache: (state) => {
      state.cacheValid = false;
    },
    
    clearStatusUpdates: (state, action: PayloadAction<string>) => {
      delete state.statusUpdates[action.payload];
      delete state.progressUpdates[action.payload];
    },
    
    // Clear errors
    clearErrors: (state) => {
      state.error = null;
      state.createError = null;
      state.updateError = null;
      state.deleteError = null;
      state.runError = null;
    },
    
    clearError: (state, action: PayloadAction<keyof Pick<AnalysisState, 'error' | 'createError' | 'updateError' | 'deleteError' | 'runError'>>) => {
      state[action.payload] = null;
    }
  },
  extraReducers: (builder) => {
    // Fetch analyses
    builder
      .addCase(fetchAnalyses.pending, (state) => {
        state.loading = 'loading';
        state.error = null;
      })
      .addCase(fetchAnalyses.fulfilled, (state, action) => {
        state.loading = 'succeeded';
        state.analyses = action.payload.data;
        state.totalCount = action.payload.pagination.total;
        state.currentPage = action.payload.pagination.page;
        state.totalPages = action.payload.pagination.pages;
        state.lastFetched = new Date().toISOString();
        state.cacheValid = true;
        
        // Update analysis map
        action.payload.data.forEach(analysis => {
          state.analysisMap[analysis.id] = analysis;
        });
      })
      .addCase(fetchAnalyses.rejected, (state, action) => {
        state.loading = 'failed';
        state.error = action.payload || 'Failed to fetch analyses';
      });

    // Fetch single analysis
    builder
      .addCase(fetchAnalysis.pending, (state) => {
        state.loading = 'loading';
        state.error = null;
      })
      .addCase(fetchAnalysis.fulfilled, (state, action) => {
        state.loading = 'succeeded';
        const analysis = action.payload;
        
        // Update in analyses array if it exists
        const existingIndex = state.analyses.findIndex(a => a.id === analysis.id);
        if (existingIndex !== -1) {
          state.analyses[existingIndex] = analysis;
        }
        
        // Always update in analysis map
        state.analysisMap[analysis.id] = analysis;
      })
      .addCase(fetchAnalysis.rejected, (state, action) => {
        state.loading = 'failed';
        state.error = action.payload || 'Failed to fetch analysis';
      });

    // Create analysis
    builder
      .addCase(createAnalysis.pending, (state) => {
        state.createLoading = 'loading';
        state.createError = null;
      })
      .addCase(createAnalysis.fulfilled, (state, action) => {
        state.createLoading = 'succeeded';
        const newAnalysis = action.payload;
        
        // Add to analyses array (prepend for newest first)
        state.analyses.unshift(newAnalysis);
        state.totalCount += 1;
        
        // Add to analysis map
        state.analysisMap[newAnalysis.id] = newAnalysis;
        
        // Invalidate cache
        state.cacheValid = false;
      })
      .addCase(createAnalysis.rejected, (state, action) => {
        state.createLoading = 'failed';
        state.createError = action.payload || 'Failed to create analysis';
      });

    // Update analysis
    builder
      .addCase(updateAnalysis.pending, (state) => {
        state.updateLoading = 'loading';
        state.updateError = null;
      })
      .addCase(updateAnalysis.fulfilled, (state, action) => {
        state.updateLoading = 'succeeded';
        const updatedAnalysis = action.payload;
        
        // Update in analyses array
        const existingIndex = state.analyses.findIndex(a => a.id === updatedAnalysis.id);
        if (existingIndex !== -1) {
          state.analyses[existingIndex] = updatedAnalysis;
        }
        
        // Update in analysis map
        state.analysisMap[updatedAnalysis.id] = updatedAnalysis;
      })
      .addCase(updateAnalysis.rejected, (state, action) => {
        state.updateLoading = 'failed';
        state.updateError = action.payload || 'Failed to update analysis';
      });

    // Delete analysis
    builder
      .addCase(deleteAnalysis.pending, (state) => {
        state.deleteLoading = 'loading';
        state.deleteError = null;
      })
      .addCase(deleteAnalysis.fulfilled, (state, action) => {
        state.deleteLoading = 'succeeded';
        const deletedId = action.payload;
        
        // Remove from analyses array
        state.analyses = state.analyses.filter(a => a.id !== deletedId);
        state.totalCount = Math.max(0, state.totalCount - 1);
        
        // Remove from analysis map
        delete state.analysisMap[deletedId];
        
        // Clear related updates
        delete state.statusUpdates[deletedId];
        delete state.progressUpdates[deletedId];
        
        // Invalidate cache
        state.cacheValid = false;
      })
      .addCase(deleteAnalysis.rejected, (state, action) => {
        state.deleteLoading = 'failed';
        state.deleteError = action.payload || 'Failed to delete analysis';
      });

    // Run analysis
    builder
      .addCase(runAnalysis.pending, (state) => {
        state.runLoading = 'loading';
        state.runError = null;
      })
      .addCase(runAnalysis.fulfilled, (state, action) => {
        state.runLoading = 'succeeded';
        const updatedAnalysis = action.payload;
        
        // Update in analyses array
        const existingIndex = state.analyses.findIndex(a => a.id === updatedAnalysis.id);
        if (existingIndex !== -1) {
          state.analyses[existingIndex] = updatedAnalysis;
        }
        
        // Update in analysis map
        state.analysisMap[updatedAnalysis.id] = updatedAnalysis;
      })
      .addCase(runAnalysis.rejected, (state, action) => {
        state.runLoading = 'failed';
        state.runError = action.payload || 'Failed to run analysis';
      });

    // Cancel analysis
    builder
      .addCase(cancelAnalysis.fulfilled, (state, action) => {
        const updatedAnalysis = action.payload;
        
        // Update in analyses array
        const existingIndex = state.analyses.findIndex(a => a.id === updatedAnalysis.id);
        if (existingIndex !== -1) {
          state.analyses[existingIndex] = updatedAnalysis;
        }
        
        // Update in analysis map
        state.analysisMap[updatedAnalysis.id] = updatedAnalysis;
      });
  }
});

// Selectors
export const selectAnalyses = (state: RootState) => state.analyses.analyses;
export const selectAnalysisMap = (state: RootState) => state.analyses.analysisMap;
export const selectAnalysisById = (state: RootState, id: string) => state.analyses.analysisMap[id] || null;
export const selectAnalysesLoading = (state: RootState) => state.analyses.loading;
export const selectAnalysesError = (state: RootState) => state.analyses.error;
export const selectAnalysesTotalCount = (state: RootState) => state.analyses.totalCount;
export const selectAnalysesCurrentPage = (state: RootState) => state.analyses.currentPage;
export const selectAnalysesTotalPages = (state: RootState) => state.analyses.totalPages;
export const selectAnalysesFilters = (state: RootState) => state.analyses.filters;

// Specialized selectors
export const selectAnalysesByStatus = (state: RootState, status: AnalysisStatus) =>
  state.analyses.analyses.filter(analysis => analysis.status === status);

export const selectRunningAnalyses = (state: RootState) =>
  state.analyses.analyses.filter(analysis => 
    analysis.status === 'running' || analysis.status === 'queued'
  );

export const selectCompletedAnalyses = (state: RootState) =>
  state.analyses.analyses.filter(analysis => analysis.status === 'completed');

export const selectFailedAnalyses = (state: RootState) =>
  state.analyses.analyses.filter(analysis => analysis.status === 'failed');

export const selectRecentAnalyses = (state: RootState, limit = 5) =>
  [...state.analyses.analyses]
    .sort((a, b) => new Date(b.updated_at).getTime() - new Date(a.updated_at).getTime())
    .slice(0, limit);

export const selectAnalysisStatusUpdate = (state: RootState, id: string) =>
  state.analyses.statusUpdates[id] || null;

export const selectAnalysisProgressUpdate = (state: RootState, id: string) =>
  state.analyses.progressUpdates[id] || null;

// Loading state selectors
export const selectCreateAnalysisLoading = (state: RootState) => state.analyses.createLoading;
export const selectUpdateAnalysisLoading = (state: RootState) => state.analyses.updateLoading;
export const selectDeleteAnalysisLoading = (state: RootState) => state.analyses.deleteLoading;
export const selectRunAnalysisLoading = (state: RootState) => state.analyses.runLoading;

// Error selectors
export const selectCreateAnalysisError = (state: RootState) => state.analyses.createError;
export const selectUpdateAnalysisError = (state: RootState) => state.analyses.updateError;
export const selectDeleteAnalysisError = (state: RootState) => state.analyses.deleteError;
export const selectRunAnalysisError = (state: RootState) => state.analyses.runError;

// Cache selectors
export const selectAnalysesCacheValid = (state: RootState) => state.analyses.cacheValid;
export const selectAnalysesLastFetched = (state: RootState) => state.analyses.lastFetched;

// Export actions
export const {
  setFilters,
  resetFilters,
  updateAnalysisStatus,
  updateAnalysisProgress,
  invalidateCache,
  clearStatusUpdates,
  clearErrors,
  clearError
} = analysisSlice.actions;

// Export reducer
export default analysisSlice.reducer;