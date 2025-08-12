import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface FilterState {
  selectedCase: string | null;
  selectedComponent: string | null;
  selectedLoadingCondition: string | null;
  selectedHeading: number | null;
}

const initialState: FilterState = {
  selectedCase: null,
  selectedComponent: null,
  selectedLoadingCondition: null,
  selectedHeading: null,
};

const filterSlice = createSlice({
  name: 'filter',
  initialState,
  reducers: {
    setSelectedCase: (state, action: PayloadAction<string | null>) => {
      state.selectedCase = action.payload;
    },
    setSelectedComponent: (state, action: PayloadAction<string | null>) => {
      state.selectedComponent = action.payload;
    },
    setSelectedLoadingCondition: (state, action: PayloadAction<string | null>) => {
      state.selectedLoadingCondition = action.payload;
    },
    setSelectedHeading: (state, action: PayloadAction<number | null>) => {
      state.selectedHeading = action.payload;
    },
    resetFilters: (state) => {
      return initialState;
    },
  },
});

export const {
  setSelectedCase,
  setSelectedComponent,
  setSelectedLoadingCondition,
  setSelectedHeading,
  resetFilters,
} = filterSlice.actions;

export default filterSlice.reducer;