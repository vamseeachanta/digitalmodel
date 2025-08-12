import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface DataState {
  cases: string[];
  components: string[];
  currentData: any;
  loading: boolean;
  error: string | null;
}

const initialState: DataState = {
  cases: [],
  components: [],
  currentData: null,
  loading: false,
  error: null,
};

const dataSlice = createSlice({
  name: 'data',
  initialState,
  reducers: {
    setCases: (state, action: PayloadAction<string[]>) => {
      state.cases = action.payload;
    },
    setComponents: (state, action: PayloadAction<string[]>) => {
      state.components = action.payload;
    },
    setCurrentData: (state, action: PayloadAction<any>) => {
      state.currentData = action.payload;
    },
    setLoading: (state, action: PayloadAction<boolean>) => {
      state.loading = action.payload;
    },
    setError: (state, action: PayloadAction<string | null>) => {
      state.error = action.payload;
    },
  },
});

export const { setCases, setComponents, setCurrentData, setLoading, setError } = dataSlice.actions;
export default dataSlice.reducer;