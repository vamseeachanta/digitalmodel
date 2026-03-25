/**
 * Redux store configuration with RTK Query
 */

import { configureStore } from '@reduxjs/toolkit';
import { setupListeners } from '@reduxjs/toolkit/query';

import { apiSlice } from './api/apiSlice';
import analysisReducer from './slices/analysisSlice';
import resultsReducer from './slices/resultsSlice';
import uiReducer from './slices/uiSlice';

export const store = configureStore({
  reducer: {
    // RTK Query
    [apiSlice.reducerPath]: apiSlice.reducer,
    
    // Feature slices
    analysis: analysisReducer,
    results: resultsReducer,
    ui: uiReducer,
  },
  middleware: (getDefaultMiddleware) =>
    getDefaultMiddleware({
      serializableCheck: {
        ignoredActions: [
          // Ignore these action types
          'persist/PERSIST',
          'persist/REHYDRATE',
          'persist/PAUSE',
          'persist/PURGE',
          'persist/REGISTER',
        ],
        ignoredActionsPaths: ['meta.arg', 'payload.timestamp'],
        ignoredPaths: ['items.dates'],
      },
    }).concat(apiSlice.middleware),
  
  devTools: process.env.NODE_ENV !== 'production',
});

// Setup listeners for RTK Query
setupListeners(store.dispatch);

export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;

// Re-export hooks
export { useAppDispatch, useAppSelector } from './hooks';
export { apiSlice } from './api/apiSlice';