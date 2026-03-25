import React, { createContext, useContext, useReducer, useCallback, useEffect } from 'react';
import { useSearchParams } from 'react-router-dom';

// Types for filter state
export interface LoadingCondition {
  waterLevel: 'hwl' | 'lwl';
  volume: '125km3' | '180km3';
  side: 'pb' | 'sb';
}

export interface FilterState {
  analysisCase: string[];
  loadingCondition: LoadingCondition[];
  heading: number[];
  isLoading: boolean;
  error: string | null;
  lastUpdated: Date | null;
}

export interface FilterPreset {
  id: string;
  name: string;
  description: string;
  filters: Omit<FilterState, 'isLoading' | 'error' | 'lastUpdated'>;
  createdAt: Date;
}

interface FilterContextType {
  state: FilterState;
  actions: {
    setAnalysisCase: (cases: string[]) => void;
    setLoadingCondition: (conditions: LoadingCondition[]) => void;
    setHeading: (headings: number[]) => void;
    setLoading: (loading: boolean) => void;
    setError: (error: string | null) => void;
    resetFilters: () => void;
    applyPreset: (preset: FilterPreset) => void;
    savePreset: (name: string, description: string) => FilterPreset;
    deletePreset: (id: string) => void;
    validateFilters: () => boolean;
  };
  presets: FilterPreset[];
}

// Action types
type FilterAction =
  | { type: 'SET_ANALYSIS_CASE'; payload: string[] }
  | { type: 'SET_LOADING_CONDITION'; payload: LoadingCondition[] }
  | { type: 'SET_HEADING'; payload: number[] }
  | { type: 'SET_LOADING'; payload: boolean }
  | { type: 'SET_ERROR'; payload: string | null }
  | { type: 'RESET_FILTERS' }
  | { type: 'APPLY_PRESET'; payload: FilterPreset };

// Initial state
const initialState: FilterState = {
  analysisCase: [],
  loadingCondition: [],
  heading: [],
  isLoading: false,
  error: null,
  lastUpdated: null,
};

// Reducer
const filterReducer = (state: FilterState, action: FilterAction): FilterState => {
  switch (action.type) {
    case 'SET_ANALYSIS_CASE':
      return {
        ...state,
        analysisCase: action.payload,
        lastUpdated: new Date(),
        error: null,
      };
    case 'SET_LOADING_CONDITION':
      return {
        ...state,
        loadingCondition: action.payload,
        lastUpdated: new Date(),
        error: null,
      };
    case 'SET_HEADING':
      return {
        ...state,
        heading: action.payload,
        lastUpdated: new Date(),
        error: null,
      };
    case 'SET_LOADING':
      return { ...state, isLoading: action.payload };
    case 'SET_ERROR':
      return { ...state, error: action.payload, isLoading: false };
    case 'RESET_FILTERS':
      return {
        ...initialState,
        lastUpdated: new Date(),
      };
    case 'APPLY_PRESET':
      return {
        ...action.payload.filters,
        isLoading: false,
        error: null,
        lastUpdated: new Date(),
      };
    default:
      return state;
  }
};

// URL parameter serialization helpers
const serializeFilters = (filters: FilterState): string => {
  const params = new URLSearchParams();
  
  if (filters.analysisCase.length) {
    params.set('analysisCase', filters.analysisCase.join(','));
  }
  
  if (filters.loadingCondition.length) {
    params.set('loadingCondition', JSON.stringify(filters.loadingCondition));
  }
  
  if (filters.heading.length) {
    params.set('heading', filters.heading.join(','));
  }
  
  return params.toString();
};

const deserializeFilters = (searchParams: URLSearchParams): Partial<FilterState> => {
  const filters: Partial<FilterState> = {};
  
  const analysisCase = searchParams.get('analysisCase');
  if (analysisCase) {
    filters.analysisCase = analysisCase.split(',');
  }
  
  const loadingCondition = searchParams.get('loadingCondition');
  if (loadingCondition) {
    try {
      filters.loadingCondition = JSON.parse(loadingCondition);
    } catch (e) {
      console.warn('Failed to parse loading condition from URL');
    }
  }
  
  const heading = searchParams.get('heading');
  if (heading) {
    filters.heading = heading.split(',').map(Number).filter(n => !isNaN(n));
  }
  
  return filters;
};

// Context creation
const FilterContext = createContext<FilterContextType | null>(null);

export const useFilterContext = (): FilterContextType => {
  const context = useContext(FilterContext);
  if (!context) {
    throw new Error('useFilterContext must be used within a FilterProvider');
  }
  return context;
};

// Provider component
interface FilterProviderProps {
  children: React.ReactNode;
}

export const FilterProvider: React.FC<FilterProviderProps> = ({ children }) => {
  const [searchParams, setSearchParams] = useSearchParams();
  const [state, dispatch] = useReducer(filterReducer, initialState);
  const [presets, setPresets] = React.useState<FilterPreset[]>([]);

  // Load presets from localStorage on mount
  useEffect(() => {
    const savedPresets = localStorage.getItem('orcaflex-filter-presets');
    if (savedPresets) {
      try {
        const parsedPresets = JSON.parse(savedPresets).map((preset: any) => ({
          ...preset,
          createdAt: new Date(preset.createdAt),
        }));
        setPresets(parsedPresets);
      } catch (e) {
        console.warn('Failed to load filter presets from localStorage');
      }
    }
  }, []);

  // Load filters from URL on mount
  useEffect(() => {
    const urlFilters = deserializeFilters(searchParams);
    if (Object.keys(urlFilters).length > 0) {
      Object.entries(urlFilters).forEach(([key, value]) => {
        switch (key) {
          case 'analysisCase':
            dispatch({ type: 'SET_ANALYSIS_CASE', payload: value as string[] });
            break;
          case 'loadingCondition':
            dispatch({ type: 'SET_LOADING_CONDITION', payload: value as LoadingCondition[] });
            break;
          case 'heading':
            dispatch({ type: 'SET_HEADING', payload: value as number[] });
            break;
        }
      });
    }
  }, [searchParams]);

  // Update URL when filters change
  useEffect(() => {
    if (state.lastUpdated) {
      const serialized = serializeFilters(state);
      setSearchParams(serialized ? `?${serialized}` : '');
    }
  }, [state, setSearchParams]);

  // Save presets to localStorage when they change
  useEffect(() => {
    localStorage.setItem('orcaflex-filter-presets', JSON.stringify(presets));
  }, [presets]);

  // Filter validation
  const validateFilters = useCallback((): boolean => {
    // Check for invalid combinations
    if (state.analysisCase.length === 0) {
      dispatch({ type: 'SET_ERROR', payload: 'At least one analysis case must be selected' });
      return false;
    }

    // Validate heading range
    const invalidHeadings = state.heading.filter(h => h < 0 || h > 345 || h % 15 !== 0);
    if (invalidHeadings.length > 0) {
      dispatch({ 
        type: 'SET_ERROR', 
        payload: `Invalid headings: ${invalidHeadings.join(', ')}. Headings must be between 0° and 345° in 15° increments.` 
      });
      return false;
    }

    // Check for loading condition compatibility
    if (state.loadingCondition.length > 0 && state.analysisCase.length > 0) {
      // Add custom validation logic here based on your data structure
      // For now, assume all combinations are valid
    }

    dispatch({ type: 'SET_ERROR', payload: null });
    return true;
  }, [state.analysisCase, state.heading, state.loadingCondition]);

  // Actions
  const actions = {
    setAnalysisCase: useCallback((cases: string[]) => {
      dispatch({ type: 'SET_ANALYSIS_CASE', payload: cases });
    }, []),

    setLoadingCondition: useCallback((conditions: LoadingCondition[]) => {
      dispatch({ type: 'SET_LOADING_CONDITION', payload: conditions });
    }, []),

    setHeading: useCallback((headings: number[]) => {
      dispatch({ type: 'SET_HEADING', payload: headings });
    }, []),

    setLoading: useCallback((loading: boolean) => {
      dispatch({ type: 'SET_LOADING', payload: loading });
    }, []),

    setError: useCallback((error: string | null) => {
      dispatch({ type: 'SET_ERROR', payload: error });
    }, []),

    resetFilters: useCallback(() => {
      dispatch({ type: 'RESET_FILTERS' });
    }, []),

    applyPreset: useCallback((preset: FilterPreset) => {
      dispatch({ type: 'APPLY_PRESET', payload: preset });
    }, []),

    savePreset: useCallback((name: string, description: string): FilterPreset => {
      const preset: FilterPreset = {
        id: `preset-${Date.now()}`,
        name,
        description,
        filters: {
          analysisCase: state.analysisCase,
          loadingCondition: state.loadingCondition,
          heading: state.heading,
        },
        createdAt: new Date(),
      };
      
      setPresets(prev => [...prev, preset]);
      return preset;
    }, [state.analysisCase, state.loadingCondition, state.heading]),

    deletePreset: useCallback((id: string) => {
      setPresets(prev => prev.filter(p => p.id !== id));
    }, []),

    validateFilters,
  };

  const contextValue: FilterContextType = {
    state,
    actions,
    presets,
  };

  return (
    <FilterContext.Provider value={contextValue}>
      {children}
    </FilterContext.Provider>
  );
};

export default FilterContext;