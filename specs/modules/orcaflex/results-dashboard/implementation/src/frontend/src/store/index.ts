import { configureStore } from '@reduxjs/toolkit';
import dataSlice from './slices/dataSlice';
import filterSlice from './slices/filterSlice';
import uiSlice from './slices/uiSlice';

export const store = configureStore({
  reducer: {
    data: dataSlice,
    filter: filterSlice,
    ui: uiSlice,
  },
});

export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;