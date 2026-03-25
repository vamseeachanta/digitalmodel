// Redux slice for components state management
// Manages OrcaFlex model components and their relationships

import { createSlice, createAsyncThunk, PayloadAction } from '@reduxjs/toolkit';
import {
  Component,
  ComponentType,
  LoadingState,
  ApiResponse
} from '../../types/api';
import { apiClient } from '../../services/api';
import type { RootState } from '../index';

// State interface
export interface ComponentsState {
  // Data - organized by analysis ID
  components: Record<string, Component[]>;
  componentMap: Record<string, Record<string, Component>>;
  
  // Loading states
  loading: LoadingState;
  
  // Error states
  error: string | null;
  
  // Cache management
  lastFetched: Record<string, string>;
  cacheValid: Record<string, boolean>;
  
  // Filter and sorting
  filters: Record<string, ComponentFilter>;
  sorting: Record<string, ComponentSorting>;
}

interface ComponentFilter {
  types: ComponentType[];
  search: string;
  hasConnections: boolean | null;
  customFilter?: (component: Component) => boolean;
}

interface ComponentSorting {
  field: 'name' | 'type' | 'position';
  order: 'asc' | 'desc';
}

// Initial state
const initialState: ComponentsState = {
  components: {},
  componentMap: {},
  
  loading: 'idle',
  error: null,
  
  lastFetched: {},
  cacheValid: {},
  
  filters: {},
  sorting: {}
};

// Async thunks
export const fetchComponents = createAsyncThunk<
  { analysisId: string; components: Component[] },
  string,
  { rejectValue: string }
>(
  'components/fetchComponents',
  async (analysisId, { rejectWithValue }) => {
    try {
      const response = await apiClient.getComponents(analysisId);
      return {
        analysisId,
        components: response.data
      };
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to fetch components');
    }
  }
);

export const fetchComponent = createAsyncThunk<
  { analysisId: string; component: Component },
  { analysisId: string; componentId: string },
  { rejectValue: string }
>(
  'components/fetchComponent',
  async ({ analysisId, componentId }, { rejectWithValue }) => {
    try {
      const response = await apiClient.getComponent(analysisId, componentId);
      return {
        analysisId,
        component: response.data
      };
    } catch (error: any) {
      return rejectWithValue(error.message || 'Failed to fetch component');
    }
  }
);

// Slice
const componentsSlice = createSlice({
  name: 'components',
  initialState,
  reducers: {
    // Cache management
    invalidateComponents: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      state.cacheValid[analysisId] = false;
      delete state.lastFetched[analysisId];
    },
    
    clearComponents: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      delete state.components[analysisId];
      delete state.componentMap[analysisId];
      delete state.lastFetched[analysisId];
      delete state.cacheValid[analysisId];
      delete state.filters[analysisId];
      delete state.sorting[analysisId];
    },
    
    clearAllComponents: (state) => {
      state.components = {};
      state.componentMap = {};
      state.lastFetched = {};
      state.cacheValid = {};
      state.filters = {};
      state.sorting = {};
    },
    
    // Filter management
    setComponentFilter: (state, action: PayloadAction<{ analysisId: string; filter: Partial<ComponentFilter> }>) => {
      const { analysisId, filter } = action.payload;
      
      if (!state.filters[analysisId]) {
        state.filters[analysisId] = {
          types: [],
          search: '',
          hasConnections: null
        };
      }
      
      state.filters[analysisId] = {
        ...state.filters[analysisId],
        ...filter
      };
    },
    
    clearComponentFilter: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      delete state.filters[analysisId];
    },
    
    // Sorting management
    setComponentSorting: (state, action: PayloadAction<{ analysisId: string; sorting: ComponentSorting }>) => {
      const { analysisId, sorting } = action.payload;
      state.sorting[analysisId] = sorting;
    },
    
    clearComponentSorting: (state, action: PayloadAction<string>) => {
      const analysisId = action.payload;
      delete state.sorting[analysisId];
    },
    
    // Component updates (for real-time updates)
    updateComponent: (state, action: PayloadAction<{ analysisId: string; component: Component }>) => {
      const { analysisId, component } = action.payload;
      
      // Update in components array
      if (state.components[analysisId]) {
        const index = state.components[analysisId].findIndex(c => c.id === component.id);
        if (index !== -1) {
          state.components[analysisId][index] = component;
        } else {
          state.components[analysisId].push(component);
        }
      }
      
      // Update in component map
      if (!state.componentMap[analysisId]) {
        state.componentMap[analysisId] = {};
      }
      state.componentMap[analysisId][component.id] = component;
    },
    
    removeComponent: (state, action: PayloadAction<{ analysisId: string; componentId: string }>) => {
      const { analysisId, componentId } = action.payload;
      
      // Remove from components array
      if (state.components[analysisId]) {
        state.components[analysisId] = state.components[analysisId].filter(c => c.id !== componentId);
      }
      
      // Remove from component map
      if (state.componentMap[analysisId]) {
        delete state.componentMap[analysisId][componentId];
      }
    },
    
    // Error management
    clearError: (state) => {
      state.error = null;
    }
  },
  extraReducers: (builder) => {
    // Fetch components
    builder
      .addCase(fetchComponents.pending, (state) => {
        state.loading = 'loading';
        state.error = null;
      })
      .addCase(fetchComponents.fulfilled, (state, action) => {
        state.loading = 'succeeded';
        const { analysisId, components } = action.payload;
        
        state.components[analysisId] = components;
        state.lastFetched[analysisId] = new Date().toISOString();
        state.cacheValid[analysisId] = true;
        
        // Update component map
        state.componentMap[analysisId] = {};
        components.forEach(component => {
          state.componentMap[analysisId][component.id] = component;
        });
      })
      .addCase(fetchComponents.rejected, (state, action) => {
        state.loading = 'failed';
        state.error = action.payload || 'Failed to fetch components';
      });

    // Fetch single component
    builder
      .addCase(fetchComponent.fulfilled, (state, action) => {
        const { analysisId, component } = action.payload;
        
        // Update in components array if it exists
        if (state.components[analysisId]) {
          const existingIndex = state.components[analysisId].findIndex(c => c.id === component.id);
          if (existingIndex !== -1) {
            state.components[analysisId][existingIndex] = component;
          }
        }
        
        // Always update in component map
        if (!state.componentMap[analysisId]) {
          state.componentMap[analysisId] = {};
        }
        state.componentMap[analysisId][component.id] = component;
      });
  }
});

// Selectors
export const selectComponents = (state: RootState, analysisId: string) =>
  state.components.components[analysisId] || [];

export const selectComponentMap = (state: RootState, analysisId: string) =>
  state.components.componentMap[analysisId] || {};

export const selectComponentById = (state: RootState, analysisId: string, componentId: string) =>
  state.components.componentMap[analysisId]?.[componentId] || null;

export const selectComponentsLoading = (state: RootState) => state.components.loading;
export const selectComponentsError = (state: RootState) => state.components.error;

export const selectComponentsCached = (state: RootState, analysisId: string) =>
  state.components.cacheValid[analysisId] || false;

export const selectComponentsLastFetched = (state: RootState, analysisId: string) =>
  state.components.lastFetched[analysisId] || null;

// Filter and sorting selectors
export const selectComponentFilter = (state: RootState, analysisId: string) =>
  state.components.filters[analysisId] || null;

export const selectComponentSorting = (state: RootState, analysisId: string) =>
  state.components.sorting[analysisId] || null;

// Derived selectors
export const selectComponentsByType = (state: RootState, analysisId: string, type: ComponentType) =>
  selectComponents(state, analysisId).filter(component => component.type === type);

export const selectComponentTypes = (state: RootState, analysisId: string) => {
  const components = selectComponents(state, analysisId);
  const types = new Set<ComponentType>();
  components.forEach(component => types.add(component.type));
  return Array.from(types);
};

export const selectComponentsCount = (state: RootState, analysisId: string) =>
  selectComponents(state, analysisId).length;

export const selectComponentsCountByType = (state: RootState, analysisId: string) => {
  const components = selectComponents(state, analysisId);
  const counts: Record<ComponentType, number> = {
    'line': 0,
    'vessel': 0,
    'winch': 0,
    'attachment': 0,
    'constraint': 0,
    'support': 0,
    'buoy': 0,
    '6dbuoy': 0,
    'shape': 0
  };
  
  components.forEach(component => {
    counts[component.type]++;
  });
  
  return counts;
};

// Connection analysis selectors
export const selectConnectedComponents = (state: RootState, analysisId: string, componentId: string) => {
  const component = selectComponentById(state, analysisId, componentId);
  if (!component) return [];
  
  const allComponents = selectComponents(state, analysisId);
  const connectedIds = component.connections.map(conn => conn.connected_to);
  
  return allComponents.filter(comp => connectedIds.includes(comp.id));
};

export const selectComponentConnections = (state: RootState, analysisId: string) => {
  const components = selectComponents(state, analysisId);
  const connections: Array<{ from: string; to: string; type: string }> = [];
  
  components.forEach(component => {
    component.connections.forEach(connection => {
      connections.push({
        from: component.id,
        to: connection.connected_to,
        type: connection.connection_type
      });
    });
  });
  
  return connections;
};

export const selectIsolatedComponents = (state: RootState, analysisId: string) =>
  selectComponents(state, analysisId).filter(component => component.connections.length === 0);

export const selectMostConnectedComponent = (state: RootState, analysisId: string) => {
  const components = selectComponents(state, analysisId);
  if (components.length === 0) return null;
  
  let mostConnected = components[0];
  let maxConnections = components[0].connections.length;
  
  components.forEach(component => {
    if (component.connections.length > maxConnections) {
      maxConnections = component.connections.length;
      mostConnected = component;
    }
  });
  
  return mostConnected;
};

// Filtered and sorted selectors
export const selectFilteredComponents = (state: RootState, analysisId: string) => {
  const components = selectComponents(state, analysisId);
  const filter = selectComponentFilter(state, analysisId);
  
  if (!filter) return components;
  
  return components.filter(component => {
    // Type filter
    if (filter.types.length > 0 && !filter.types.includes(component.type)) {
      return false;
    }
    
    // Search filter
    if (filter.search && !component.name.toLowerCase().includes(filter.search.toLowerCase())) {
      return false;
    }
    
    // Connection filter
    if (filter.hasConnections !== null) {
      const hasConnections = component.connections.length > 0;
      if (filter.hasConnections !== hasConnections) {
        return false;
      }
    }
    
    // Custom filter
    if (filter.customFilter && !filter.customFilter(component)) {
      return false;
    }
    
    return true;
  });
};

export const selectSortedFilteredComponents = (state: RootState, analysisId: string) => {
  const filteredComponents = selectFilteredComponents(state, analysisId);
  const sorting = selectComponentSorting(state, analysisId);
  
  if (!sorting) return filteredComponents;
  
  const sorted = [...filteredComponents];
  
  sorted.sort((a, b) => {
    let aValue: any;
    let bValue: any;
    
    switch (sorting.field) {
      case 'name':
        aValue = a.name.toLowerCase();
        bValue = b.name.toLowerCase();
        break;
      case 'type':
        aValue = a.type;
        bValue = b.type;
        break;
      case 'position':
        aValue = Math.sqrt(a.position.x ** 2 + a.position.y ** 2 + a.position.z ** 2);
        bValue = Math.sqrt(b.position.x ** 2 + b.position.y ** 2 + b.position.z ** 2);
        break;
      default:
        return 0;
    }
    
    if (aValue < bValue) return sorting.order === 'asc' ? -1 : 1;
    if (aValue > bValue) return sorting.order === 'asc' ? 1 : -1;
    return 0;
  });
  
  return sorted;
};

// Component statistics selectors
export const selectComponentStatistics = (state: RootState, analysisId: string) => {
  const components = selectComponents(state, analysisId);
  
  if (components.length === 0) {
    return {
      totalComponents: 0,
      byType: {},
      totalConnections: 0,
      averageConnections: 0,
      maxConnections: 0,
      minConnections: 0,
      isolatedComponents: 0
    };
  }
  
  const byType = selectComponentsCountByType(state, analysisId);
  const connections = components.map(c => c.connections.length);
  const totalConnections = connections.reduce((sum, count) => sum + count, 0);
  const isolatedCount = components.filter(c => c.connections.length === 0).length;
  
  return {
    totalComponents: components.length,
    byType,
    totalConnections,
    averageConnections: totalConnections / components.length,
    maxConnections: Math.max(...connections, 0),
    minConnections: Math.min(...connections, 0),
    isolatedComponents: isolatedCount
  };
};

// Model bounds selector for 3D visualization
export const selectModelBounds = (state: RootState, analysisId: string) => {
  const components = selectComponents(state, analysisId);
  
  if (components.length === 0) {
    return {
      min: { x: -1, y: -1, z: -1 },
      max: { x: 1, y: 1, z: 1 },
      center: { x: 0, y: 0, z: 0 },
      size: { x: 2, y: 2, z: 2 }
    };
  }
  
  let minX = Infinity, minY = Infinity, minZ = Infinity;
  let maxX = -Infinity, maxY = -Infinity, maxZ = -Infinity;
  
  components.forEach(component => {
    const { x, y, z } = component.position;
    minX = Math.min(minX, x);
    minY = Math.min(minY, y);
    minZ = Math.min(minZ, z);
    maxX = Math.max(maxX, x);
    maxY = Math.max(maxY, y);
    maxZ = Math.max(maxZ, z);
  });
  
  const center = {
    x: (minX + maxX) / 2,
    y: (minY + maxY) / 2,
    z: (minZ + maxZ) / 2
  };
  
  const size = {
    x: maxX - minX,
    y: maxY - minY,
    z: maxZ - minZ
  };
  
  return {
    min: { x: minX, y: minY, z: minZ },
    max: { x: maxX, y: maxY, z: maxZ },
    center,
    size
  };
};

// Search selector
export const selectSearchComponents = (state: RootState, analysisId: string, query: string) => {
  if (!query.trim()) return selectComponents(state, analysisId);
  
  const lowercaseQuery = query.toLowerCase();
  return selectComponents(state, analysisId).filter(component =>
    component.name.toLowerCase().includes(lowercaseQuery) ||
    component.type.toLowerCase().includes(lowercaseQuery) ||
    component.id.toLowerCase().includes(lowercaseQuery)
  );
};

// Export actions
export const {
  invalidateComponents,
  clearComponents,
  clearAllComponents,
  setComponentFilter,
  clearComponentFilter,
  setComponentSorting,
  clearComponentSorting,
  updateComponent,
  removeComponent,
  clearError
} = componentsSlice.actions;

// Export reducer
export default componentsSlice.reducer;