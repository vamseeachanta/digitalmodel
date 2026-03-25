// API Client for OrcaFlex Dashboard
// Axios-based client with interceptors, error handling, and request/response transformations

import axios, { 
  AxiosInstance, 
  AxiosRequestConfig, 
  AxiosResponse, 
  InternalAxiosRequestConfig 
} from 'axios';
import {
  ApiResponse,
  PaginatedResponse,
  ErrorResponse,
  Analysis,
  AnalysisResults,
  Component,
  CreateAnalysisRequest,
  UpdateAnalysisRequest,
  RunAnalysisRequest,
  GetAnalysesRequest,
  GetResultsRequest,
  ExportResultsRequest,
  CacheOptions
} from '../types/api';

// Configuration
const API_BASE_URL = process.env.REACT_APP_API_BASE_URL || 'http://localhost:8000';
const API_VERSION = process.env.REACT_APP_API_VERSION || 'v1';
const REQUEST_TIMEOUT = parseInt(process.env.REACT_APP_REQUEST_TIMEOUT || '30000');

// Cache configuration
const DEFAULT_CACHE_TTL = 5 * 60 * 1000; // 5 minutes
const cache = new Map<string, { data: any; expires: number }>();

// Types for internal use
interface RequestConfig extends AxiosRequestConfig {
  skipAuth?: boolean;
  skipErrorHandler?: boolean;
  cache?: CacheOptions;
}

interface ApiError extends Error {
  status?: number;
  data?: ErrorResponse;
  isNetworkError?: boolean;
}

// Authentication token management
class TokenManager {
  private static instance: TokenManager;
  private token: string | null = null;
  private refreshPromise: Promise<string> | null = null;

  public static getInstance(): TokenManager {
    if (!TokenManager.instance) {
      TokenManager.instance = new TokenManager();
    }
    return TokenManager.instance;
  }

  setToken(token: string): void {
    this.token = token;
    localStorage.setItem('auth_token', token);
  }

  getToken(): string | null {
    if (!this.token) {
      this.token = localStorage.getItem('auth_token');
    }
    return this.token;
  }

  clearToken(): void {
    this.token = null;
    localStorage.removeItem('auth_token');
    localStorage.removeItem('refresh_token');
  }

  async refreshToken(): Promise<string> {
    if (this.refreshPromise) {
      return this.refreshPromise;
    }

    this.refreshPromise = this.performTokenRefresh();
    
    try {
      const newToken = await this.refreshPromise;
      this.refreshPromise = null;
      return newToken;
    } catch (error) {
      this.refreshPromise = null;
      throw error;
    }
  }

  private async performTokenRefresh(): Promise<string> {
    const refreshToken = localStorage.getItem('refresh_token');
    if (!refreshToken) {
      throw new Error('No refresh token available');
    }

    try {
      const response = await axios.post(`${API_BASE_URL}/auth/refresh`, {
        refresh_token: refreshToken
      });

      const { access_token, refresh_token: newRefreshToken } = response.data;
      
      this.setToken(access_token);
      localStorage.setItem('refresh_token', newRefreshToken);
      
      return access_token;
    } catch (error) {
      this.clearToken();
      throw error;
    }
  }
}

// Cache utilities
const getCacheKey = (url: string, params?: any): string => {
  const paramString = params ? JSON.stringify(params) : '';
  return `${url}:${paramString}`;
};

const getFromCache = <T>(key: string): T | null => {
  const cached = cache.get(key);
  if (cached && cached.expires > Date.now()) {
    return cached.data;
  }
  cache.delete(key);
  return null;
};

const setCache = <T>(key: string, data: T, ttl: number = DEFAULT_CACHE_TTL): void => {
  cache.set(key, {
    data,
    expires: Date.now() + ttl
  });
};

const clearCache = (pattern?: string): void => {
  if (pattern) {
    for (const [key] of cache) {
      if (key.includes(pattern)) {
        cache.delete(key);
      }
    }
  } else {
    cache.clear();
  }
};

// Error handling utilities
const createApiError = (error: any): ApiError => {
  const apiError = new Error() as ApiError;
  
  if (error.response) {
    // Server responded with error status
    apiError.message = error.response.data?.message || 'Server error occurred';
    apiError.status = error.response.status;
    apiError.data = error.response.data;
    apiError.isNetworkError = false;
  } else if (error.request) {
    // Network error
    apiError.message = 'Network error - please check your connection';
    apiError.isNetworkError = true;
  } else {
    // Other error
    apiError.message = error.message || 'An unexpected error occurred';
  }
  
  return apiError;
};

// Request/Response interceptors
const setupInterceptors = (instance: AxiosInstance) => {
  // Request interceptor
  instance.interceptors.request.use(
    (config: InternalAxiosRequestConfig) => {
      // Add authentication token
      const tokenManager = TokenManager.getInstance();
      const token = tokenManager.getToken();
      
      if (token && !config.skipAuth) {
        config.headers.Authorization = `Bearer ${token}`;
      }

      // Add request timestamp for monitoring
      config.metadata = {
        ...config.metadata,
        startTime: Date.now()
      };

      // Log request in development
      if (process.env.NODE_ENV === 'development') {
        console.log(`🚀 API Request: ${config.method?.toUpperCase()} ${config.url}`);
      }

      return config;
    },
    (error) => {
      console.error('Request interceptor error:', error);
      return Promise.reject(error);
    }
  );

  // Response interceptor
  instance.interceptors.response.use(
    (response: AxiosResponse) => {
      // Log response time in development
      if (process.env.NODE_ENV === 'development') {
        const duration = Date.now() - (response.config.metadata?.startTime || 0);
        console.log(`✅ API Response: ${response.status} ${response.config.url} (${duration}ms)`);
      }

      return response;
    },
    async (error) => {
      const originalRequest = error.config;

      // Handle 401 errors with token refresh
      if (error.response?.status === 401 && !originalRequest._retry && !originalRequest.skipAuth) {
        originalRequest._retry = true;

        try {
          const tokenManager = TokenManager.getInstance();
          const newToken = await tokenManager.refreshToken();
          originalRequest.headers.Authorization = `Bearer ${newToken}`;
          return instance(originalRequest);
        } catch (refreshError) {
          // Redirect to login or handle authentication failure
          window.dispatchEvent(new CustomEvent('auth:logout'));
          return Promise.reject(refreshError);
        }
      }

      // Log error in development
      if (process.env.NODE_ENV === 'development') {
        console.error(`❌ API Error: ${error.response?.status} ${error.config?.url}`, error.response?.data);
      }

      return Promise.reject(error);
    }
  );
};

// Create axios instance
const createApiClient = (): AxiosInstance => {
  const instance = axios.create({
    baseURL: `${API_BASE_URL}/api/${API_VERSION}`,
    timeout: REQUEST_TIMEOUT,
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json'
    }
  });

  setupInterceptors(instance);
  return instance;
};

// Main API client class
class ApiClient {
  private client: AxiosInstance;
  private tokenManager: TokenManager;

  constructor() {
    this.client = createApiClient();
    this.tokenManager = TokenManager.getInstance();
  }

  // Generic request method with caching support
  private async request<T>(
    config: RequestConfig
  ): Promise<T> {
    const { cache: cacheOptions, ...axiosConfig } = config;
    const cacheKey = cacheOptions ? getCacheKey(axiosConfig.url || '', axiosConfig.params) : null;

    // Check cache first
    if (cacheKey && axiosConfig.method?.toLowerCase() === 'get') {
      const cached = getFromCache<T>(cacheKey);
      if (cached) {
        return cached;
      }
    }

    try {
      const response = await this.client(axiosConfig);
      const data = response.data;

      // Cache successful GET requests
      if (cacheKey && axiosConfig.method?.toLowerCase() === 'get' && cacheOptions) {
        setCache(cacheKey, data, cacheOptions.ttl);
      }

      return data;
    } catch (error) {
      throw createApiError(error);
    }
  }

  // Authentication methods
  async login(email: string, password: string): Promise<{ access_token: string; refresh_token: string }> {
    const response = await this.request<ApiResponse<{ access_token: string; refresh_token: string }>>({
      method: 'POST',
      url: '/auth/login',
      data: { email, password },
      skipAuth: true
    });

    this.tokenManager.setToken(response.data.access_token);
    localStorage.setItem('refresh_token', response.data.refresh_token);

    return response.data;
  }

  async logout(): Promise<void> {
    try {
      await this.request({
        method: 'POST',
        url: '/auth/logout'
      });
    } finally {
      this.tokenManager.clearToken();
      clearCache();
    }
  }

  // Analysis methods
  async getAnalyses(params?: GetAnalysesRequest): Promise<PaginatedResponse<Analysis>> {
    return this.request<PaginatedResponse<Analysis>>({
      method: 'GET',
      url: '/analyses',
      params,
      cache: { ttl: DEFAULT_CACHE_TTL }
    });
  }

  async getAnalysis(id: string): Promise<ApiResponse<Analysis>> {
    return this.request<ApiResponse<Analysis>>({
      method: 'GET',
      url: `/analyses/${id}`,
      cache: { ttl: DEFAULT_CACHE_TTL }
    });
  }

  async createAnalysis(data: CreateAnalysisRequest): Promise<ApiResponse<Analysis>> {
    const response = await this.request<ApiResponse<Analysis>>({
      method: 'POST',
      url: '/analyses',
      data
    });

    // Invalidate analyses list cache
    clearCache('/analyses');

    return response;
  }

  async updateAnalysis(id: string, data: UpdateAnalysisRequest): Promise<ApiResponse<Analysis>> {
    const response = await this.request<ApiResponse<Analysis>>({
      method: 'PATCH',
      url: `/analyses/${id}`,
      data
    });

    // Invalidate related caches
    clearCache(`/analyses/${id}`);
    clearCache('/analyses');

    return response;
  }

  async deleteAnalysis(id: string): Promise<ApiResponse<void>> {
    const response = await this.request<ApiResponse<void>>({
      method: 'DELETE',
      url: `/analyses/${id}`
    });

    // Invalidate related caches
    clearCache(`/analyses/${id}`);
    clearCache('/analyses');

    return response;
  }

  async runAnalysis(id: string, options?: RunAnalysisRequest): Promise<ApiResponse<Analysis>> {
    const response = await this.request<ApiResponse<Analysis>>({
      method: 'POST',
      url: `/analyses/${id}/run`,
      data: options || {}
    });

    // Invalidate analysis cache to reflect new status
    clearCache(`/analyses/${id}`);

    return response;
  }

  async cancelAnalysis(id: string): Promise<ApiResponse<Analysis>> {
    const response = await this.request<ApiResponse<Analysis>>({
      method: 'POST',
      url: `/analyses/${id}/cancel`
    });

    // Invalidate analysis cache
    clearCache(`/analyses/${id}`);

    return response;
  }

  // Results methods
  async getResults(params: GetResultsRequest): Promise<ApiResponse<AnalysisResults>> {
    return this.request<ApiResponse<AnalysisResults>>({
      method: 'GET',
      url: `/analyses/${params.analysis_id}/results`,
      params: {
        result_types: params.result_types,
        component_ids: params.component_ids,
        start_time: params.start_time,
        end_time: params.end_time,
        include_time_series: params.include_time_series,
        include_statistics: params.include_statistics,
        include_extremes: params.include_extremes
      },
      cache: { ttl: DEFAULT_CACHE_TTL * 2 } // Cache results longer
    });
  }

  async exportResults(params: ExportResultsRequest): Promise<Blob> {
    const response = await this.client.request({
      method: 'POST',
      url: `/analyses/${params.analysis_id}/results/export`,
      data: params,
      responseType: 'blob',
      timeout: 300000 // 5 minutes for large exports
    });

    return response.data;
  }

  async getResultsSummary(analysisId: string): Promise<ApiResponse<any>> {
    return this.request<ApiResponse<any>>({
      method: 'GET',
      url: `/analyses/${analysisId}/results/summary`,
      cache: { ttl: DEFAULT_CACHE_TTL }
    });
  }

  // Component methods
  async getComponents(analysisId: string): Promise<ApiResponse<Component[]>> {
    return this.request<ApiResponse<Component[]>>({
      method: 'GET',
      url: `/analyses/${analysisId}/components`,
      cache: { ttl: DEFAULT_CACHE_TTL * 3 } // Cache components longer as they rarely change
    });
  }

  async getComponent(analysisId: string, componentId: string): Promise<ApiResponse<Component>> {
    return this.request<ApiResponse<Component>>({
      method: 'GET',
      url: `/analyses/${analysisId}/components/${componentId}`,
      cache: { ttl: DEFAULT_CACHE_TTL * 3 }
    });
  }

  // File upload methods
  async uploadModelFile(file: File, onUploadProgress?: (progress: number) => void): Promise<ApiResponse<{ file_path: string }>> {
    const formData = new FormData();
    formData.append('file', file);

    return this.request<ApiResponse<{ file_path: string }>>({
      method: 'POST',
      url: '/files/upload',
      data: formData,
      headers: {
        'Content-Type': 'multipart/form-data'
      },
      onUploadProgress: (progressEvent) => {
        if (onUploadProgress && progressEvent.total) {
          const progress = (progressEvent.loaded / progressEvent.total) * 100;
          onUploadProgress(Math.round(progress));
        }
      },
      timeout: 600000 // 10 minutes for large files
    });
  }

  // Health check method
  async healthCheck(): Promise<ApiResponse<{ status: string; timestamp: string }>> {
    return this.request<ApiResponse<{ status: string; timestamp: string }>>({
      method: 'GET',
      url: '/health',
      skipAuth: true,
      timeout: 5000
    });
  }

  // Cache management methods
  clearCache(pattern?: string): void {
    clearCache(pattern);
  }

  getCacheStats(): { size: number; keys: string[] } {
    return {
      size: cache.size,
      keys: Array.from(cache.keys())
    };
  }
}

// WebSocket client for real-time updates
export class WebSocketClient {
  private ws: WebSocket | null = null;
  private listeners: Map<string, Set<(data: any) => void>> = new Map();
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;
  private reconnectDelay = 1000;
  private tokenManager: TokenManager;

  constructor() {
    this.tokenManager = TokenManager.getInstance();
  }

  connect(): void {
    const token = this.tokenManager.getToken();
    const wsUrl = `${process.env.REACT_APP_WS_URL || 'ws://localhost:8000'}/ws?token=${token}`;

    this.ws = new WebSocket(wsUrl);

    this.ws.onopen = () => {
      console.log('WebSocket connected');
      this.reconnectAttempts = 0;
      this.emit('connection_status', { connected: true });
    };

    this.ws.onmessage = (event) => {
      try {
        const message = JSON.parse(event.data);
        this.emit(message.type, message.payload);
      } catch (error) {
        console.error('Failed to parse WebSocket message:', error);
      }
    };

    this.ws.onclose = () => {
      console.log('WebSocket disconnected');
      this.emit('connection_status', { connected: false });
      this.handleReconnect();
    };

    this.ws.onerror = (error) => {
      console.error('WebSocket error:', error);
      this.emit('error', { error: 'WebSocket connection error' });
    };
  }

  disconnect(): void {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
    this.listeners.clear();
  }

  subscribe(event: string, callback: (data: any) => void): () => void {
    if (!this.listeners.has(event)) {
      this.listeners.set(event, new Set());
    }
    this.listeners.get(event)!.add(callback);

    // Return unsubscribe function
    return () => {
      const eventListeners = this.listeners.get(event);
      if (eventListeners) {
        eventListeners.delete(callback);
        if (eventListeners.size === 0) {
          this.listeners.delete(event);
        }
      }
    };
  }

  private emit(event: string, data: any): void {
    const eventListeners = this.listeners.get(event);
    if (eventListeners) {
      eventListeners.forEach(callback => callback(data));
    }
  }

  private handleReconnect(): void {
    if (this.reconnectAttempts >= this.maxReconnectAttempts) {
      console.error('Max reconnection attempts reached');
      return;
    }

    const delay = this.reconnectDelay * Math.pow(2, this.reconnectAttempts);
    this.reconnectAttempts++;

    setTimeout(() => {
      console.log(`Attempting to reconnect (${this.reconnectAttempts}/${this.maxReconnectAttempts})`);
      this.connect();
    }, delay);
  }
}

// Create singleton instances
export const apiClient = new ApiClient();
export const wsClient = new WebSocketClient();

// Export utility functions
export { createApiError, TokenManager };
export type { ApiError };