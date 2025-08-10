// Core filter types
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
  tags?: string[];
  isShared?: boolean;
  createdBy?: string;
}

// Analysis case types
export interface AnalysisCase {
  id: string;
  name: string;
  description: string;
  category: string;
  tags: string[];
  isActive: boolean;
  createdDate: string;
  parameters?: {
    duration?: number;
    timestep?: number;
    environment?: string;
    vessel?: string;
  };
  metadata?: Record<string, any>;
}

// Dashboard data types
export interface DashboardData {
  analysisCases: AnalysisCase[];
  loadingConditions: Record<string, LoadingCondition[]>;
  headings: Record<string, Record<string, number[]>>;
  results: Record<string, any>;
  loading: boolean;
  error: string | null;
  lastUpdated: Date | null;
}

// Filter component props
export interface BaseFilterProps {
  onSelectionChange?: (...args: any[]) => void;
  disabled?: boolean;
  className?: string;
}

export interface AnalysisCaseFilterProps extends BaseFilterProps {
  maxHeight?: number;
  showCategories?: boolean;
  enableSearch?: boolean;
}

export interface LoadingConditionFilterProps extends BaseFilterProps {
  availableConditions?: LoadingCondition[];
  mode?: 'simple' | 'advanced';
  showPreview?: boolean;
}

export interface HeadingFilterProps extends BaseFilterProps {
  minHeading?: number;
  maxHeading?: number;
  step?: number;
  mode?: 'range' | 'individual' | 'visual';
  showCompass?: boolean;
}

export interface FilterPresetsProps extends BaseFilterProps {
  onPresetApply?: (preset: FilterPreset) => void;
  onPresetSave?: (preset: FilterPreset) => void;
  onPresetDelete?: (id: string) => void;
  allowImportExport?: boolean;
  maxPresets?: number;
}

export interface FilterPanelProps extends BaseFilterProps {
  onFilterChange?: () => void;
  autoApplyFilters?: boolean;
  showPresets?: boolean;
  compactMode?: boolean;
  allowCollapse?: boolean;
  defaultExpanded?: string[];
}

// Context types
export interface FilterContextActions {
  setAnalysisCase: (cases: string[]) => void;
  setLoadingCondition: (conditions: LoadingCondition[]) => void;
  setHeading: (headings: number[]) => void;
  setLoading: (loading: boolean) => void;
  setError: (error: string | null) => void;
  resetFilters: () => void;
  applyPreset: (preset: FilterPreset) => void;
  savePreset: (name: string, description: string, tags?: string[]) => FilterPreset;
  deletePreset: (id: string) => void;
  validateFilters: () => boolean;
  updatePreset: (id: string, updates: Partial<FilterPreset>) => void;
  exportPresets: () => string;
  importPresets: (data: string) => void;
}

export interface FilterContextType {
  state: FilterState;
  actions: FilterContextActions;
  presets: FilterPreset[];
  config: FilterConfig;
}

// Configuration types
export interface FilterConfig {
  heading: {
    min: number;
    max: number;
    step: number;
    common: number[];
  };
  waterLevels: readonly ('hwl' | 'lwl')[];
  volumes: readonly ('125km3' | '180km3')[];
  sides: readonly ('pb' | 'sb')[];
  updateDebounceMs: number;
  maxPresets: number;
  autoSaveToUrl: boolean;
  storageKey: string;
}

// Validation types
export interface FilterValidationResult {
  isValid: boolean;
  errors: FilterValidationError[];
  warnings: FilterValidationWarning[];
}

export interface FilterValidationError {
  field: keyof FilterState;
  message: string;
  value: any;
  code: string;
}

export interface FilterValidationWarning {
  field: keyof FilterState;
  message: string;
  suggestion?: string;
  code: string;
}

// Event types
export interface FilterChangeEvent {
  type: 'filter-change';
  field: keyof FilterState;
  oldValue: any;
  newValue: any;
  timestamp: Date;
  source: 'user' | 'preset' | 'url' | 'reset';
}

export interface FilterErrorEvent {
  type: 'filter-error';
  error: FilterValidationError;
  timestamp: Date;
}

export interface FilterPresetEvent {
  type: 'preset-saved' | 'preset-applied' | 'preset-deleted';
  preset: FilterPreset;
  timestamp: Date;
}

// Utility types
export type FilterFieldType = keyof Pick<FilterState, 'analysisCase' | 'loadingCondition' | 'heading'>;

export type FilterUpdateCallback = (field: FilterFieldType, value: any) => void;

export type FilterValidationCallback = (result: FilterValidationResult) => void;

export type PresetActionCallback = (action: string, preset?: FilterPreset) => void;

// Redux integration types
export interface FilterSliceState {
  currentFilters: FilterState;
  presets: FilterPreset[];
  config: FilterConfig;
  ui: {
    panelExpanded: boolean;
    activeTab: string;
    compactMode: boolean;
    autoApply: boolean;
  };
}

export interface FilterAction {
  type: string;
  payload?: any;
  meta?: {
    timestamp: Date;
    source: string;
    debounce?: number;
  };
}

// Chart integration types
export interface ChartFilterData {
  analysisCase: string[];
  loadingCondition: LoadingCondition[];
  heading: number[];
  dateRange?: [Date, Date];
  parameters?: Record<string, any>;
}

export interface FilteredChartResult {
  data: any[];
  metadata: {
    totalRecords: number;
    filteredRecords: number;
    filterCriteria: ChartFilterData;
    processingTime: number;
  };
}

// Export preset formats
export interface PresetExportFormat {
  version: string;
  timestamp: string;
  presets: FilterPreset[];
  metadata?: {
    exportedBy?: string;
    description?: string;
    tags?: string[];
  };
}

// Advanced filter types
export interface AdvancedFilterOptions {
  enableFuzzySearch: boolean;
  caseSensitive: boolean;
  useRegex: boolean;
  includeInactive: boolean;
  sortBy: 'name' | 'date' | 'category';
  sortOrder: 'asc' | 'desc';
  groupBy?: 'category' | 'tag' | 'none';
}

export interface FilterHistory {
  id: string;
  filters: FilterState;
  timestamp: Date;
  description?: string;
  autoSaved: boolean;
}

// Performance monitoring
export interface FilterPerformanceMetrics {
  filterUpdateTime: number;
  dataProcessingTime: number;
  renderTime: number;
  totalRecords: number;
  filteredRecords: number;
  cachehits: number;
  cacheMisses: number;
}

// Error handling
export enum FilterErrorCode {
  INVALID_ANALYSIS_CASE = 'INVALID_ANALYSIS_CASE',
  INVALID_LOADING_CONDITION = 'INVALID_LOADING_CONDITION',
  INVALID_HEADING_RANGE = 'INVALID_HEADING_RANGE',
  NO_DATA_AVAILABLE = 'NO_DATA_AVAILABLE',
  PRESET_SAVE_FAILED = 'PRESET_SAVE_FAILED',
  PRESET_LOAD_FAILED = 'PRESET_LOAD_FAILED',
  VALIDATION_FAILED = 'VALIDATION_FAILED',
  NETWORK_ERROR = 'NETWORK_ERROR',
  PERMISSION_DENIED = 'PERMISSION_DENIED',
}

export class FilterError extends Error {
  code: FilterErrorCode;
  field?: keyof FilterState;
  value?: any;
  context?: Record<string, any>;

  constructor(
    message: string,
    code: FilterErrorCode,
    field?: keyof FilterState,
    value?: any,
    context?: Record<string, any>
  ) {
    super(message);
    this.name = 'FilterError';
    this.code = code;
    this.field = field;
    this.value = value;
    this.context = context;
  }
}

// Type guards
export const isLoadingCondition = (value: any): value is LoadingCondition => {
  return (
    value &&
    typeof value === 'object' &&
    ['hwl', 'lwl'].includes(value.waterLevel) &&
    ['125km3', '180km3'].includes(value.volume) &&
    ['pb', 'sb'].includes(value.side)
  );
};

export const isValidHeading = (heading: number): boolean => {
  return Number.isInteger(heading) && heading >= 0 && heading <= 345 && heading % 15 === 0;
};

export const isFilterPreset = (value: any): value is FilterPreset => {
  return (
    value &&
    typeof value === 'object' &&
    typeof value.id === 'string' &&
    typeof value.name === 'string' &&
    value.filters &&
    value.createdAt instanceof Date
  );
};