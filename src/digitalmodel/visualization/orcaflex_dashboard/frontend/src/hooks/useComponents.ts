// Custom hook for component operations
// Provides state management and operations for OrcaFlex model components

import { useState, useEffect, useCallback, useMemo } from 'react';
import { useSelector, useDispatch } from 'react-redux';
import {
  Component,
  ComponentType,
  LoadingState
} from '../types/api';
import { apiClient } from '../services/api';
import {
  fetchComponents,
  fetchComponent,
  selectComponents,
  selectComponentById,
  selectComponentsLoading,
  selectComponentsError
} from '../store/slices/componentsSlice';
import type { RootState, AppDispatch } from '../store';

export interface UseComponentsOptions {
  analysisId: string;
  autoLoad?: boolean;
  filterByType?: ComponentType[];
  sortBy?: 'name' | 'type' | 'position';
  sortOrder?: 'asc' | 'desc';
}

export interface UseComponentsReturn {
  // Data
  components: Component[];
  totalCount: number;
  componentsByType: Record<ComponentType, Component[]>;
  
  // Loading states
  loading: LoadingState;
  error: string | null;
  
  // Operations
  loadComponents: () => Promise<void>;
  loadComponent: (componentId: string) => Promise<Component | null>;
  refreshComponents: () => Promise<void>;
  
  // Filtering and utilities
  getComponentsByType: (type: ComponentType) => Component[];
  getComponentById: (id: string) => Component | null;
  getConnectedComponents: (componentId: string) => Component[];
  searchComponents: (query: string) => Component[];
  filterComponents: (predicate: (component: Component) => boolean) => Component[];
  
  // Statistics and analysis
  getComponentStats: () => ComponentStats;
  getTypeDistribution: () => Record<ComponentType, number>;
  getConnectionMatrix: () => ConnectionMatrix;
  
  // Sorting
  sortedComponents: Component[];
  setSortBy: (field: 'name' | 'type' | 'position') => void;
  setSortOrder: (order: 'asc' | 'desc') => void;
  
  // Type filtering
  typeFilter: ComponentType[];
  setTypeFilter: (types: ComponentType[]) => void;
  clearTypeFilter: () => void;
}

export interface ComponentStats {
  totalComponents: number;
  byType: Record<ComponentType, number>;
  totalConnections: number;
  averageConnectionsPerComponent: number;
  mostConnectedComponent: Component | null;
  isolatedComponents: Component[];
}

export interface ConnectionMatrix {
  components: string[];
  matrix: boolean[][];
  adjacencyList: Record<string, string[]>;
}

const DEFAULT_SORT_BY = 'name';
const DEFAULT_SORT_ORDER = 'asc';

export const useComponents = (options: UseComponentsOptions): UseComponentsReturn => {
  const {
    analysisId,
    autoLoad = true,
    filterByType = [],
    sortBy: initialSortBy = DEFAULT_SORT_BY,
    sortOrder: initialSortOrder = DEFAULT_SORT_ORDER
  } = options;

  const dispatch = useDispatch<AppDispatch>();
  
  // Redux selectors
  const components = useSelector((state: RootState) => selectComponents(state, analysisId));
  const loading = useSelector((state: RootState) => selectComponentsLoading(state));
  const error = useSelector((state: RootState) => selectComponentsError(state));

  // Local state
  const [sortBy, setSortBy] = useState<'name' | 'type' | 'position'>(initialSortBy);
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>(initialSortOrder);
  const [typeFilter, setTypeFilter] = useState<ComponentType[]>(filterByType);

  // Computed values
  const totalCount = components.length;

  // Filter components by type
  const filteredComponents = useMemo(() => {
    if (typeFilter.length === 0) return components;
    return components.filter(component => typeFilter.includes(component.type));
  }, [components, typeFilter]);

  // Sort components
  const sortedComponents = useMemo(() => {
    const sorted = [...filteredComponents];
    
    sorted.sort((a, b) => {
      let aValue: any;
      let bValue: any;
      
      switch (sortBy) {
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
      
      if (aValue < bValue) return sortOrder === 'asc' ? -1 : 1;
      if (aValue > bValue) return sortOrder === 'asc' ? 1 : -1;
      return 0;
    });
    
    return sorted;
  }, [filteredComponents, sortBy, sortOrder]);

  // Group components by type
  const componentsByType = useMemo(() => {
    const grouped: Record<ComponentType, Component[]> = {
      'line': [],
      'vessel': [],
      'winch': [],
      'attachment': [],
      'constraint': [],
      'support': [],
      'buoy': [],
      '6dbuoy': [],
      'shape': []
    };
    
    components.forEach(component => {
      if (grouped[component.type]) {
        grouped[component.type].push(component);
      }
    });
    
    return grouped;
  }, [components]);

  // Load components
  const loadComponents = useCallback(async () => {
    if (!analysisId) return;
    
    try {
      await dispatch(fetchComponents(analysisId)).unwrap();
    } catch (err) {
      console.error('Failed to load components:', err);
    }
  }, [dispatch, analysisId]);

  // Load single component
  const loadComponent = useCallback(async (componentId: string): Promise<Component | null> => {
    if (!analysisId) return null;
    
    try {
      const result = await dispatch(fetchComponent({ analysisId, componentId })).unwrap();
      return result;
    } catch (err) {
      console.error('Failed to load component:', err);
      return null;
    }
  }, [dispatch, analysisId]);

  // Refresh components
  const refreshComponents = useCallback(async () => {
    await loadComponents();
  }, [loadComponents]);

  // Filtering and utility functions
  const getComponentsByType = useCallback((type: ComponentType): Component[] => {
    return componentsByType[type] || [];
  }, [componentsByType]);

  const getComponentById = useCallback((id: string): Component | null => {
    return components.find(component => component.id === id) || null;
  }, [components]);

  const getConnectedComponents = useCallback((componentId: string): Component[] => {
    const component = getComponentById(componentId);
    if (!component) return [];
    
    const connectedIds = component.connections.map(conn => conn.connected_to);
    return components.filter(comp => connectedIds.includes(comp.id));
  }, [components, getComponentById]);

  const searchComponents = useCallback((query: string): Component[] => {
    const lowercaseQuery = query.toLowerCase();
    return components.filter(component =>
      component.name.toLowerCase().includes(lowercaseQuery) ||
      component.type.toLowerCase().includes(lowercaseQuery) ||
      component.id.toLowerCase().includes(lowercaseQuery)
    );
  }, [components]);

  const filterComponents = useCallback((predicate: (component: Component) => boolean): Component[] => {
    return components.filter(predicate);
  }, [components]);

  // Statistics and analysis
  const getComponentStats = useCallback((): ComponentStats => {
    const byType: Record<ComponentType, number> = {
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

    let totalConnections = 0;
    let maxConnections = 0;
    let mostConnectedComponent: Component | null = null;
    const isolatedComponents: Component[] = [];

    components.forEach(component => {
      byType[component.type]++;
      
      const connectionCount = component.connections.length;
      totalConnections += connectionCount;
      
      if (connectionCount > maxConnections) {
        maxConnections = connectionCount;
        mostConnectedComponent = component;
      }
      
      if (connectionCount === 0) {
        isolatedComponents.push(component);
      }
    });

    return {
      totalComponents: components.length,
      byType,
      totalConnections,
      averageConnectionsPerComponent: components.length > 0 ? totalConnections / components.length : 0,
      mostConnectedComponent,
      isolatedComponents
    };
  }, [components]);

  const getTypeDistribution = useCallback((): Record<ComponentType, number> => {
    const distribution: Record<ComponentType, number> = {
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
      distribution[component.type]++;
    });

    return distribution;
  }, [components]);

  const getConnectionMatrix = useCallback((): ConnectionMatrix => {
    const componentIds = components.map(c => c.id);
    const matrix: boolean[][] = Array(components.length).fill(null).map(() => Array(components.length).fill(false));
    const adjacencyList: Record<string, string[]> = {};

    components.forEach((component, i) => {
      adjacencyList[component.id] = [];
      
      component.connections.forEach(connection => {
        const connectedIndex = componentIds.indexOf(connection.connected_to);
        if (connectedIndex !== -1) {
          matrix[i][connectedIndex] = true;
          matrix[connectedIndex][i] = true; // Assuming bidirectional connections
          adjacencyList[component.id].push(connection.connected_to);
          
          if (!adjacencyList[connection.connected_to]) {
            adjacencyList[connection.connected_to] = [];
          }
          if (!adjacencyList[connection.connected_to].includes(component.id)) {
            adjacencyList[connection.connected_to].push(component.id);
          }
        }
      });
    });

    return {
      components: componentIds,
      matrix,
      adjacencyList
    };
  }, [components]);

  // Filter management
  const clearTypeFilter = useCallback(() => {
    setTypeFilter([]);
  }, []);

  // Auto-load effect
  useEffect(() => {
    if (autoLoad && analysisId) {
      loadComponents();
    }
  }, [autoLoad, analysisId, loadComponents]);

  return {
    // Data
    components: sortedComponents,
    totalCount,
    componentsByType,
    
    // Loading states
    loading,
    error,
    
    // Operations
    loadComponents,
    loadComponent,
    refreshComponents,
    
    // Filtering and utilities
    getComponentsByType,
    getComponentById,
    getConnectedComponents,
    searchComponents,
    filterComponents,
    
    // Statistics and analysis
    getComponentStats,
    getTypeDistribution,
    getConnectionMatrix,
    
    // Sorting
    sortedComponents,
    setSortBy,
    setSortOrder,
    
    // Type filtering
    typeFilter,
    setTypeFilter,
    clearTypeFilter
  };
};

// Specialized hook for single component
export interface UseSingleComponentOptions {
  analysisId: string;
  componentId: string;
  autoLoad?: boolean;
}

export const useSingleComponent = (options: UseSingleComponentOptions) => {
  const { analysisId, componentId, autoLoad = true } = options;
  
  const dispatch = useDispatch<AppDispatch>();
  const component = useSelector((state: RootState) => selectComponentById(state, analysisId, componentId));
  
  const [loading, setLoading] = useState<LoadingState>('idle');
  const [error, setError] = useState<string | null>(null);

  const loadComponent = useCallback(async () => {
    if (!analysisId || !componentId) return;
    
    setLoading('loading');
    setError(null);
    
    try {
      await dispatch(fetchComponent({ analysisId, componentId })).unwrap();
      setLoading('succeeded');
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load component');
      setLoading('failed');
    }
  }, [dispatch, analysisId, componentId]);

  // Auto-load effect
  useEffect(() => {
    if (autoLoad && analysisId && componentId) {
      loadComponent();
    }
  }, [autoLoad, analysisId, componentId, loadComponent]);

  return {
    component,
    loading,
    error,
    refresh: loadComponent,
    exists: component !== null
  };
};

// Hook for component visualization helpers
export interface UseComponentVisualizationOptions {
  components: Component[];
}

export const useComponentVisualization = (options: UseComponentVisualizationOptions) => {
  const { components } = options;

  // Get 3D positions for visualization
  const getPositions3D = useCallback(() => {
    return components.map(component => ({
      id: component.id,
      name: component.name,
      type: component.type,
      position: component.position,
      orientation: component.orientation
    }));
  }, [components]);

  // Get connection lines for 3D visualization
  const getConnectionLines = useCallback(() => {
    const lines: Array<{ from: string; to: string; fromPos: any; toPos: any }> = [];
    
    components.forEach(component => {
      component.connections.forEach(connection => {
        const connectedComponent = components.find(c => c.id === connection.connected_to);
        if (connectedComponent) {
          lines.push({
            from: component.id,
            to: connectedComponent.id,
            fromPos: component.position,
            toPos: connectedComponent.position
          });
        }
      });
    });
    
    return lines;
  }, [components]);

  // Get component bounds for camera positioning
  const getModelBounds = useCallback() => {
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
  }, [components]);

  // Get color mapping for component types
  const getTypeColorMap = useCallback() => {
    const colorMap: Record<ComponentType, string> = {
      'line': '#3498db',      // Blue
      'vessel': '#e74c3c',    // Red
      'winch': '#f39c12',     // Orange
      'attachment': '#2ecc71', // Green
      'constraint': '#9b59b6', // Purple
      'support': '#1abc9c',   // Teal
      'buoy': '#e67e22',      // Orange
      '6dbuoy': '#f1c40f',    // Yellow
      'shape': '#95a5a6'      // Gray
    };
    
    return colorMap;
  }, []);

  return {
    getPositions3D,
    getConnectionLines,
    getModelBounds,
    getTypeColorMap
  };
};