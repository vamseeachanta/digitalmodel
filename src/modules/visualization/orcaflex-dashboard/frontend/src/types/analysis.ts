/**
 * Type definitions for OrcaFlex analysis operations
 */

export enum AnalysisStatus {
  PENDING = 'pending',
  RUNNING = 'running',
  COMPLETED = 'completed',
  FAILED = 'failed',
  CANCELLED = 'cancelled',
}

export enum AnalysisType {
  STATIC = 'static',
  DYNAMIC = 'dynamic',
  FATIGUE = 'fatigue',
  FREQUENCY_DOMAIN = 'frequency_domain',
  MODAL = 'modal',
}

export interface Analysis {
  id: string;
  name: string;
  description?: string;
  analysis_type: AnalysisType;
  orcaflex_file: string;
  configuration: Record<string, any>;
  status: AnalysisStatus;
  created_at: string;
  started_at?: string;
  completed_at?: string;
  user_id?: string;
  tags: string[];
}

export interface AnalysisRequest {
  name: string;
  description?: string;
  analysis_type: AnalysisType;
  orcaflex_file: string;
  configuration?: Record<string, any>;
  tags?: string[];
}

export interface AnalysisResult {
  analysis_id: string;
  status: AnalysisStatus;
  results: Record<string, any>;
  metrics: Record<string, number>;
  output_files: string[];
  visualization_data?: Record<string, any>;
  error_message?: string;
  error_details?: Record<string, any>;
  execution_time?: number;
  memory_usage?: number;
}

export interface AnalysisProgress {
  analysis_id: string;
  progress: number; // 0-100
  current_step: string;
  total_steps: number;
  completed_steps: number;
  estimated_completion?: string;
}

export interface AnalysisSummary {
  id: string;
  name: string;
  analysis_type: AnalysisType;
  status: AnalysisStatus;
  created_at: string;
  completed_at?: string;
  execution_time?: number;
  has_results: boolean;
}

export interface AnalysisFilters {
  status?: AnalysisStatus;
  analysis_type?: AnalysisType;
  date_range?: {
    start: string;
    end: string;
  };
  tags?: string[];
  search?: string;
}

export interface AnalysisPagination {
  limit: number;
  offset: number;
}

export interface AnalysisListResponse {
  analyses: AnalysisSummary[];
  total: number;
  limit: number;
  offset: number;
  has_more: boolean;
}

export interface FileUploadResponse {
  message: string;
  file_path: string;
  filename: string;
  size: number;
}