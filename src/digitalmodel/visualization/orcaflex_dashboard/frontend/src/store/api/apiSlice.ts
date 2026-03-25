/**
 * RTK Query API slice for OrcaFlex Dashboard
 */

import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';
import type { RootState } from '../index';

// Base query with authentication
const baseQuery = fetchBaseQuery({
  baseUrl: process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000/api',
  prepareHeaders: (headers, { getState }) => {
    // Add auth token if available
    const token = typeof window !== 'undefined' 
      ? localStorage.getItem('auth_token') 
      : null;
    
    if (token) {
      headers.set('authorization', `Bearer ${token}`);
    }
    
    return headers;
  },
});

// Base query with retry logic
const baseQueryWithRetry = async (args: any, api: any, extraOptions: any) => {
  let result = await baseQuery(args, api, extraOptions);
  
  // Retry on network errors
  if (result.error && result.error.status === 'FETCH_ERROR') {
    // Wait 1 second and retry
    await new Promise(resolve => setTimeout(resolve, 1000));
    result = await baseQuery(args, api, extraOptions);
  }
  
  return result;
};

export const apiSlice = createApi({
  reducerPath: 'api',
  baseQuery: baseQueryWithRetry,
  tagTypes: ['Analysis', 'Result', 'Health'],
  endpoints: (builder) => ({
    // Health endpoints
    getHealth: builder.query<any, void>({
      query: () => '/health',
      providesTags: ['Health'],
    }),
    
    getDetailedHealth: builder.query<any, void>({
      query: () => '/health/detailed',
      providesTags: ['Health'],
    }),
    
    // Analysis endpoints - basic CRUD operations for caching
    getAnalyses: builder.query<any[], void>({
      query: () => '/analysis',
      providesTags: ['Analysis'],
    }),
    
    getAnalysis: builder.query<any, string>({
      query: (id) => `/analysis/${id}`,
      providesTags: (result, error, id) => [{ type: 'Analysis', id }],
    }),
    
    // Results endpoints - basic CRUD operations for caching
    getResults: builder.query<any[], void>({
      query: () => '/results',
      providesTags: ['Result'],
    }),
    
    getResult: builder.query<any, string>({
      query: (id) => `/results/${id}`,
      providesTags: (result, error, id) => [{ type: 'Result', id }],
    }),
  }),
});

export const {
  useGetHealthQuery,
  useGetDetailedHealthQuery,
  useGetAnalysesQuery,
  useGetAnalysisQuery,
  useGetResultsQuery,
  useGetResultQuery,
} = apiSlice;