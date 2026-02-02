// Filter Components
export { default as FilterPanel } from './FilterPanel';
export { default as AnalysisCaseFilter } from './AnalysisCaseFilter';
export { default as LoadingConditionFilter } from './LoadingConditionFilter';
export { default as HeadingFilter } from './HeadingFilter';
export { default as FilterPresets } from './FilterPresets';

// Context and Types
export { 
  FilterProvider, 
  useFilterContext,
  type FilterState,
  type LoadingCondition,
  type FilterPreset 
} from './FilterContext';

// Re-export commonly used types
export interface FilterComponentProps {
  onSelectionChange?: (...args: any[]) => void;
  disabled?: boolean;
}

// Filter configuration constants
export const FILTER_CONFIG = {
  HEADING: {
    MIN: 0,
    MAX: 345,
    STEP: 15,
    COMMON: [0, 45, 90, 135, 180, 225, 270, 315],
  },
  WATER_LEVELS: ['hwl', 'lwl'] as const,
  VOLUMES: ['125km3', '180km3'] as const,
  SIDES: ['pb', 'sb'] as const,
  UPDATE_DEBOUNCE_MS: 500,
} as const;

// Filter validation utilities
export const validateFilters = {
  heading: (heading: number): boolean => {
    return heading >= 0 && heading <= 345 && heading % 15 === 0;
  },
  
  loadingCondition: (condition: LoadingCondition): boolean => {
    return (
      FILTER_CONFIG.WATER_LEVELS.includes(condition.waterLevel) &&
      FILTER_CONFIG.VOLUMES.includes(condition.volume) &&
      FILTER_CONFIG.SIDES.includes(condition.side)
    );
  },
  
  analysisCase: (caseId: string): boolean => {
    return typeof caseId === 'string' && caseId.length > 0;
  },
};

// Filter utilities
export const filterUtils = {
  formatLoadingCondition: (condition: LoadingCondition): string => {
    return `${condition.waterLevel.toUpperCase()}, ${condition.volume}, ${condition.side.toUpperCase()}`;
  },
  
  formatHeading: (heading: number): string => {
    const cardinalDirections = [
      { heading: 0, label: 'N' },
      { heading: 45, label: 'NE' },
      { heading: 90, label: 'E' },
      { heading: 135, label: 'SE' },
      { heading: 180, label: 'S' },
      { heading: 225, label: 'SW' },
      { heading: 270, label: 'W' },
      { heading: 315, label: 'NW' },
    ];
    
    const cardinal = cardinalDirections.find(dir => dir.heading === heading);
    return cardinal ? `${heading}° (${cardinal.label})` : `${heading}°`;
  },
  
  serializeFiltersToUrl: (filters: Partial<FilterState>): string => {
    const params = new URLSearchParams();
    
    if (filters.analysisCase?.length) {
      params.set('analysisCase', filters.analysisCase.join(','));
    }
    
    if (filters.loadingCondition?.length) {
      params.set('loadingCondition', JSON.stringify(filters.loadingCondition));
    }
    
    if (filters.heading?.length) {
      params.set('heading', filters.heading.join(','));
    }
    
    return params.toString();
  },
  
  deserializeFiltersFromUrl: (searchParams: URLSearchParams): Partial<FilterState> => {
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
  },
};

// Hook for filter state management outside of context
export const useFilterState = () => {
  const context = useFilterContext();
  
  return {
    // State
    filters: {
      analysisCase: context.state.analysisCase,
      loadingCondition: context.state.loadingCondition,
      heading: context.state.heading,
    },
    isLoading: context.state.isLoading,
    error: context.state.error,
    lastUpdated: context.state.lastUpdated,
    
    // Actions
    setAnalysisCase: context.actions.setAnalysisCase,
    setLoadingCondition: context.actions.setLoadingCondition,
    setHeading: context.actions.setHeading,
    resetFilters: context.actions.resetFilters,
    validateFilters: context.actions.validateFilters,
    
    // Presets
    presets: context.presets,
    savePreset: context.actions.savePreset,
    applyPreset: context.actions.applyPreset,
    deletePreset: context.actions.deletePreset,
    
    // Computed values
    hasActiveFilters: (
      context.state.analysisCase.length > 0 ||
      context.state.loadingCondition.length > 0 ||
      context.state.heading.length > 0
    ),
    totalFilterCount: (
      context.state.analysisCase.length +
      context.state.loadingCondition.length +
      context.state.heading.length
    ),
    isValidCascade: context.state.analysisCase.length > 0,
  };
};

// Default export for convenience
export default {
  FilterPanel,
  FilterProvider,
  useFilterContext,
  useFilterState,
  filterUtils,
  validateFilters,
  FILTER_CONFIG,
};