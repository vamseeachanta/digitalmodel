import axios from 'axios';

const API_URL = import.meta.env.VITE_API_URL || 'http://localhost:8000';

const api = axios.create({
  baseURL: `${API_URL}/api`,
  headers: {
    'Content-Type': 'application/json',
  },
});

// Request interceptor
api.interceptors.request.use(
  (config) => {
    // Add auth token if available
    const token = localStorage.getItem('token');
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => Promise.reject(error)
);

// Response interceptor
api.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response?.status === 401) {
      // Handle unauthorized
      localStorage.removeItem('token');
      window.location.href = '/login';
    }
    return Promise.reject(error);
  }
);

// API methods
export const dataAPI = {
  getCases: () => api.get('/data/cases'),
  getComponents: (caseId?: string) => api.get('/data/components', { params: { case: caseId } }),
  getPolarData: (data: any) => api.post('/data/polar', data),
  getTimeTrace: (data: any) => api.post('/data/time-trace', data),
  getStatistics: (data: any) => api.post('/data/statistics', data),
};

export const analysisAPI = {
  correlation: (data: any) => api.post('/analysis/correlation', data),
  regression: (data: any) => api.post('/analysis/regression', data),
  compare: (data: any) => api.post('/analysis/compare', data),
  sensitivity: (data: any) => api.post('/analysis/sensitivity', data),
};

export const exportAPI = {
  chart: (data: any, format: string) => api.post(`/export/chart?format=${format}`, data),
  data: (data: any, format: string) => api.post(`/export/data?format=${format}`, data),
  report: (data: any) => api.post('/export/report', data),
};

export default api;