/**
 * API client configuration and base service
 */

import axios, { AxiosInstance, AxiosRequestConfig, AxiosResponse } from 'axios';

// Create axios instance with default configuration
const createApiClient = (): AxiosInstance => {
  const baseURL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8000';
  
  const client = axios.create({
    baseURL,
    timeout: 30000, // 30 seconds
    headers: {
      'Content-Type': 'application/json',
    },
  });

  // Request interceptor
  client.interceptors.request.use(
    (config) => {
      // Add auth token if available
      const token = typeof window !== 'undefined' 
        ? localStorage.getItem('auth_token') 
        : null;
      
      if (token) {
        config.headers.Authorization = `Bearer ${token}`;
      }

      // Log requests in development
      if (process.env.NODE_ENV === 'development') {
        console.log(`[API Request] ${config.method?.toUpperCase()} ${config.url}`, {
          params: config.params,
          data: config.data,
        });
      }

      return config;
    },
    (error) => {
      console.error('[API Request Error]', error);
      return Promise.reject(error);
    }
  );

  // Response interceptor
  client.interceptors.response.use(
    (response: AxiosResponse) => {
      // Log responses in development
      if (process.env.NODE_ENV === 'development') {
        console.log(`[API Response] ${response.status} ${response.config.url}`, response.data);
      }
      return response;
    },
    (error) => {
      // Handle common error scenarios
      if (error.response) {
        // Server responded with error status
        const { status, data } = error.response;
        
        switch (status) {
          case 401:
            // Unauthorized - clear token and redirect to login
            if (typeof window !== 'undefined') {
              localStorage.removeItem('auth_token');
              window.location.href = '/login';
            }
            break;
            
          case 403:
            console.warn('[API] Forbidden - insufficient permissions');
            break;
            
          case 404:
            console.warn('[API] Resource not found');
            break;
            
          case 500:
            console.error('[API] Server error');
            break;
            
          default:
            console.error(`[API Error] ${status}:`, data);
        }
        
        // Return structured error
        return Promise.reject({
          status,
          message: data?.message || data?.detail || 'An error occurred',
          details: data,
        });
      } else if (error.request) {
        // Network error
        console.error('[API Network Error]', error.message);
        return Promise.reject({
          status: 0,
          message: 'Network error - please check your connection',
          details: error.message,
        });
      } else {
        // Other error
        console.error('[API Error]', error.message);
        return Promise.reject({
          status: -1,
          message: error.message || 'An unexpected error occurred',
          details: error,
        });
      }
    }
  );

  return client;
};

// Create the main API client
export const apiClient = createApiClient();

// Base API service class
export class BaseApiService {
  protected client: AxiosInstance;
  protected baseEndpoint: string;

  constructor(baseEndpoint: string, client: AxiosInstance = apiClient) {
    this.client = client;
    this.baseEndpoint = baseEndpoint;
  }

  protected async get<T>(
    endpoint: string = '',
    config?: AxiosRequestConfig
  ): Promise<T> {
    const response = await this.client.get(
      `${this.baseEndpoint}${endpoint}`,
      config
    );
    return response.data;
  }

  protected async post<T>(
    endpoint: string = '',
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<T> {
    const response = await this.client.post(
      `${this.baseEndpoint}${endpoint}`,
      data,
      config
    );
    return response.data;
  }

  protected async put<T>(
    endpoint: string = '',
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<T> {
    const response = await this.client.put(
      `${this.baseEndpoint}${endpoint}`,
      data,
      config
    );
    return response.data;
  }

  protected async patch<T>(
    endpoint: string = '',
    data?: any,
    config?: AxiosRequestConfig
  ): Promise<T> {
    const response = await this.client.patch(
      `${this.baseEndpoint}${endpoint}`,
      data,
      config
    );
    return response.data;
  }

  protected async delete<T>(
    endpoint: string = '',
    config?: AxiosRequestConfig
  ): Promise<T> {
    const response = await this.client.delete(
      `${this.baseEndpoint}${endpoint}`,
      config
    );
    return response.data;
  }

  protected buildQueryParams(params: Record<string, any>): string {
    const searchParams = new URLSearchParams();
    
    Object.entries(params).forEach(([key, value]) => {
      if (value !== undefined && value !== null && value !== '') {
        if (Array.isArray(value)) {
          value.forEach((item) => searchParams.append(key, String(item)));
        } else {
          searchParams.append(key, String(value));
        }
      }
    });

    return searchParams.toString();
  }
}

// Health check service
export class HealthService extends BaseApiService {
  constructor() {
    super('/api/health');
  }

  async getHealth() {
    return this.get('/');
  }

  async getDetailedHealth() {
    return this.get('/detailed');
  }

  async checkReadiness() {
    return this.get('/ready');
  }

  async checkLiveness() {
    return this.get('/live');
  }
}

// Export singleton instances
export const healthService = new HealthService();

// Utility functions
export const handleApiError = (error: any) => {
  if (error?.message) {
    return error.message;
  }
  return 'An unexpected error occurred';
};

export const isNetworkError = (error: any): boolean => {
  return error?.status === 0;
};

export const isServerError = (error: any): boolean => {
  return error?.status >= 500;
};

export const isClientError = (error: any): boolean => {
  return error?.status >= 400 && error?.status < 500;
};