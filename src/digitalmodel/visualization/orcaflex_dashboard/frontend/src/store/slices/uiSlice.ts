/**
 * UI state management slice
 */

import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface UIState {
  // Navigation
  sidebarOpen: boolean;
  currentPage: string;
  
  // Modals and dialogs
  modals: {
    createAnalysis: boolean;
    uploadFile: boolean;
    exportData: boolean;
    deleteConfirmation: boolean;
  };
  
  // Notifications
  notifications: Array<{
    id: string;
    type: 'success' | 'error' | 'warning' | 'info';
    title: string;
    message: string;
    timestamp: number;
    autoHide?: boolean;
  }>;
  
  // Loading states
  globalLoading: boolean;
  loadingStates: Record<string, boolean>;
  
  // Theme and display
  theme: 'light' | 'dark' | 'auto';
  compactMode: boolean;
  
  // Data tables
  tableSettings: {
    pageSize: number;
    sortBy: string;
    sortOrder: 'asc' | 'desc';
  };
  
  // Charts and visualization
  chartSettings: {
    showGrid: boolean;
    showLegend: boolean;
    autoScale: boolean;
    colorScheme: 'default' | 'viridis' | 'plasma' | 'inferno';
  };
}

const initialState: UIState = {
  sidebarOpen: true,
  currentPage: '/',
  
  modals: {
    createAnalysis: false,
    uploadFile: false,
    exportData: false,
    deleteConfirmation: false,
  },
  
  notifications: [],
  
  globalLoading: false,
  loadingStates: {},
  
  theme: 'auto',
  compactMode: false,
  
  tableSettings: {
    pageSize: 25,
    sortBy: 'created_at',
    sortOrder: 'desc',
  },
  
  chartSettings: {
    showGrid: true,
    showLegend: true,
    autoScale: true,
    colorScheme: 'default',
  },
};

const uiSlice = createSlice({
  name: 'ui',
  initialState,
  reducers: {
    // Navigation
    setSidebarOpen: (state, action: PayloadAction<boolean>) => {
      state.sidebarOpen = action.payload;
    },

    toggleSidebar: (state) => {
      state.sidebarOpen = !state.sidebarOpen;
    },

    setCurrentPage: (state, action: PayloadAction<string>) => {
      state.currentPage = action.payload;
    },

    // Modals
    setModalOpen: (state, action: PayloadAction<{ modal: keyof UIState['modals']; open: boolean }>) => {
      state.modals[action.payload.modal] = action.payload.open;
    },

    openModal: (state, action: PayloadAction<keyof UIState['modals']>) => {
      state.modals[action.payload] = true;
    },

    closeModal: (state, action: PayloadAction<keyof UIState['modals']>) => {
      state.modals[action.payload] = false;
    },

    closeAllModals: (state) => {
      Object.keys(state.modals).forEach(key => {
        state.modals[key as keyof UIState['modals']] = false;
      });
    },

    // Notifications
    addNotification: (state, action: PayloadAction<{
      type: 'success' | 'error' | 'warning' | 'info';
      title: string;
      message: string;
      autoHide?: boolean;
    }>) => {
      const notification = {
        id: Date.now().toString(),
        timestamp: Date.now(),
        autoHide: true,
        ...action.payload,
      };
      
      state.notifications.push(notification);
      
      // Keep only last 10 notifications
      if (state.notifications.length > 10) {
        state.notifications = state.notifications.slice(-10);
      }
    },

    removeNotification: (state, action: PayloadAction<string>) => {
      state.notifications = state.notifications.filter(n => n.id !== action.payload);
    },

    clearNotifications: (state) => {
      state.notifications = [];
    },

    // Loading states
    setGlobalLoading: (state, action: PayloadAction<boolean>) => {
      state.globalLoading = action.payload;
    },

    setLoadingState: (state, action: PayloadAction<{ key: string; loading: boolean }>) => {
      state.loadingStates[action.payload.key] = action.payload.loading;
    },

    clearLoadingState: (state, action: PayloadAction<string>) => {
      delete state.loadingStates[action.payload];
    },

    // Theme and display
    setTheme: (state, action: PayloadAction<'light' | 'dark' | 'auto'>) => {
      state.theme = action.payload;
    },

    setCompactMode: (state, action: PayloadAction<boolean>) => {
      state.compactMode = action.payload;
    },

    toggleCompactMode: (state) => {
      state.compactMode = !state.compactMode;
    },

    // Table settings
    updateTableSettings: (state, action: PayloadAction<Partial<UIState['tableSettings']>>) => {
      state.tableSettings = { ...state.tableSettings, ...action.payload };
    },

    setTablePageSize: (state, action: PayloadAction<number>) => {
      state.tableSettings.pageSize = action.payload;
    },

    setTableSort: (state, action: PayloadAction<{ sortBy: string; sortOrder: 'asc' | 'desc' }>) => {
      state.tableSettings.sortBy = action.payload.sortBy;
      state.tableSettings.sortOrder = action.payload.sortOrder;
    },

    // Chart settings
    updateChartSettings: (state, action: PayloadAction<Partial<UIState['chartSettings']>>) => {
      state.chartSettings = { ...state.chartSettings, ...action.payload };
    },

    toggleChartGrid: (state) => {
      state.chartSettings.showGrid = !state.chartSettings.showGrid;
    },

    toggleChartLegend: (state) => {
      state.chartSettings.showLegend = !state.chartSettings.showLegend;
    },

    toggleChartAutoScale: (state) => {
      state.chartSettings.autoScale = !state.chartSettings.autoScale;
    },

    setColorScheme: (state, action: PayloadAction<'default' | 'viridis' | 'plasma' | 'inferno'>) => {
      state.chartSettings.colorScheme = action.payload;
    },

    // Reset state
    resetUIState: (state) => {
      return initialState;
    },
  },
});

export const {
  setSidebarOpen,
  toggleSidebar,
  setCurrentPage,
  setModalOpen,
  openModal,
  closeModal,
  closeAllModals,
  addNotification,
  removeNotification,
  clearNotifications,
  setGlobalLoading,
  setLoadingState,
  clearLoadingState,
  setTheme,
  setCompactMode,
  toggleCompactMode,
  updateTableSettings,
  setTablePageSize,
  setTableSort,
  updateChartSettings,
  toggleChartGrid,
  toggleChartLegend,
  toggleChartAutoScale,
  setColorScheme,
  resetUIState,
} = uiSlice.actions;

// Selectors
export const selectSidebarOpen = (state: { ui: UIState }) => state.ui.sidebarOpen;
export const selectCurrentPage = (state: { ui: UIState }) => state.ui.currentPage;
export const selectModals = (state: { ui: UIState }) => state.ui.modals;
export const selectNotifications = (state: { ui: UIState }) => state.ui.notifications;
export const selectGlobalLoading = (state: { ui: UIState }) => state.ui.globalLoading;
export const selectLoadingStates = (state: { ui: UIState }) => state.ui.loadingStates;
export const selectTheme = (state: { ui: UIState }) => state.ui.theme;
export const selectCompactMode = (state: { ui: UIState }) => state.ui.compactMode;
export const selectTableSettings = (state: { ui: UIState }) => state.ui.tableSettings;
export const selectChartSettings = (state: { ui: UIState }) => state.ui.chartSettings;

// Computed selectors
export const selectIsLoading = (state: { ui: UIState }, key: string) => 
  state.ui.loadingStates[key] || false;

export const selectAnyLoading = (state: { ui: UIState }) =>
  state.ui.globalLoading || Object.values(state.ui.loadingStates).some(loading => loading);

export const selectUnreadNotifications = (state: { ui: UIState }) =>
  state.ui.notifications.length;

export default uiSlice.reducer;