/**
 * Analysis Service - Complete API client for analysis operations
 * Matches backend API endpoints exactly with comprehensive TypeScript support
 */

import { BaseApiService, EnhancedApiClient } from './api';
import {
  Analysis,
  AnalysisRequest,
  AnalysisSummary,
  AnalysisProgress,
  AnalysisFilter,
  AnalysisComparison,
  AnalysisComparisonResult,
  PaginatedResponse,
  FileUploadResponse,
  Component,
  ComponentSummary,
  ExportJob,
  ExportRequest,
} from '../types/api';

export class AnalysisService extends BaseApiService {
  constructor(client?: EnhancedApiClient) {
    super('/api/analyses', client);
  }

  // ====================
  // Analysis Management
  // ====================

  /**
   * Get list of analyses with comprehensive filtering and pagination
   * Supports all backend filter parameters with client-side caching
   */
  async getAnalyses(params: {
    status?: string;
    water_level?: string;
    volume_condition?: string;
    side_configuration?: string;
    is_baseline?: boolean;
    validation_score_min?: number;
    limit?: number;
    offset?: number;
    sort_by?: string;
    sort_desc?: boolean;
  } = {}): Promise<AnalysisSummary[]> {
    const queryParams = this.buildQueryParams(params);
    return this.get(`/?${queryParams}`);
  }

  /**
   * Create new analysis with comprehensive validation
   */
  async createAnalysis(analysisRequest: AnalysisRequest): Promise<Analysis> {
    return this.post('/', analysisRequest);
  }

  /**
   * Upload OrcaFlex file with progress tracking and validation
   */
  async uploadFile(
    file: File,
    options: {
      create_analysis?: boolean;
      analysis_name?: string;
    } = {},
    onProgress?: (progress: number) => void
  ): Promise<FileUploadResponse> {
    const formData = new FormData();
    formData.append('file', file);

    const queryParams = this.buildQueryParams(options);
    
    return this.uploadWithProgress(
      `/upload?${queryParams}`,
      formData,
      onProgress
    );
  }

  /**
   * Get specific analysis by ID with optional component and result data
   */
  async getAnalysis(
    analysisId: string,
    options: {
      include_components?: boolean;
      include_results?: boolean;
    } = {}
  ): Promise<Analysis> {
    const queryParams = this.buildQueryParams(options);
    return this.get(`/${analysisId}?${queryParams}`);
  }

  /**
   * Get components for a specific analysis with filtering
   */
  async getAnalysisComponents(
    analysisId: string,
    options: {
      component_type?: string;
      include_results?: boolean;
    } = {}
  ): Promise<{
    analysis_id: string;
    components: Component[];
    total_components: number;
    component_types: string[];
  }> {
    const queryParams = this.buildQueryParams(options);
    return this.get(`/${analysisId}/components?${queryParams}`);
  }

  /**
   * Get real-time progress information for running analysis
   */
  async getAnalysisProgress(analysisId: string): Promise<AnalysisProgress> {
    return this.get(`/${analysisId}/progress`);
  }

  /**
   * Start running an analysis with priority and options
   */
  async runAnalysis(
    analysisId: string,
    options: {
      priority?: 'low' | 'normal' | 'high';
      force_rerun?: boolean;
    } = {}
  ): Promise<{
    message: string;
    analysis_id: string;
    status: string;
    priority: string;
    estimated_duration: number;
  }> {
    const queryParams = this.buildQueryParams(options);
    return this.post(`/${analysisId}/run?${queryParams}`);
  }

  /**
   * Cancel a running analysis
   */
  async cancelAnalysis(
    analysisId: string,
    reason?: string
  ): Promise<{
    message: string;
    analysis_id: string;
    status: string;
    cancelled_by: string;
    reason?: string;
  }> {
    const queryParams = reason ? this.buildQueryParams({ reason }) : '';
    return this.post(`/${analysisId}/cancel?${queryParams}`);
  }

  /**
   * Compare multiple analyses with statistical analysis
   */
  async compareAnalyses(comparisonRequest: AnalysisComparison): Promise<AnalysisComparisonResult> {
    return this.post('/compare', comparisonRequest);
  }

  /**
   * Get comprehensive statistical analysis of analysis data
   */
  async getAnalysisStatistics(
    analysisId: string,
    options: {
      include_components?: boolean;
      include_quality_metrics?: boolean;
    } = {}
  ): Promise<any> {
    const queryParams = this.buildQueryParams(options);
    return this.get(`/${analysisId}/statistics?${queryParams}`);
  }

  /**
   * Delete analysis with comprehensive cleanup options
   */
  async deleteAnalysis(
    analysisId: string,
    options: {
      force?: boolean;
      delete_files?: boolean;
    } = {}
  ): Promise<{
    message: string;
    analysis_id: string;
    deleted_by: string;
    files_deleted: boolean;
  }> {
    const queryParams = this.buildQueryParams(options);
    return this.delete(`/${analysisId}?${queryParams}`);
  }

  // ====================
  // Analysis Validation and Quality
  // ====================

  /**
   * Validate analysis configuration before creation
   */
  async validateAnalysisConfig(config: Partial<AnalysisRequest>): Promise<{
    is_valid: boolean;
    errors: string[];
    warnings: string[];
    suggestions: string[];
  }> {
    return this.post('/validate', config);
  }

  /**
   * Get analysis quality metrics and validation results
   */
  async getAnalysisQuality(analysisId: string): Promise<{
    analysis_id: string;
    overall_quality: number;
    data_completeness: number;
    validation_score: number;
    issues: string[];
    recommendations: string[];
  }> {
    return this.get(`/${analysisId}/quality`);
  }

  // ====================
  // Batch Operations
  // ====================

  /**
   * Create multiple analyses from batch upload
   */
  async createBatchAnalyses(
    analyses: AnalysisRequest[]
  ): Promise<{
    created: Analysis[];
    failed: { request: AnalysisRequest; error: string }[];
    summary: {
      total: number;
      successful: number;
      failed: number;
    };
  }> {
    return this.post('/batch', { analyses });
  }

  /**
   * Delete multiple analyses with options
   */
  async deleteBatchAnalyses(
    analysisIds: string[],
    options: {
      force?: boolean;
      delete_files?: boolean;
    } = {}
  ): Promise<{
    deleted: string[];
    failed: { analysis_id: string; error: string }[];
    summary: {
      total: number;
      successful: number;
      failed: number;
    };
  }> {
    return this.post('/batch/delete', { 
      analysis_ids: analysisIds,
      ...options 
    });
  }

  // ====================
  // Export Operations
  // ====================

  /**
   * Export analysis data with comprehensive options
   */
  async exportAnalysis(
    analysisId: string,
    exportRequest: Partial<ExportRequest>
  ): Promise<ExportJob> {
    return this.post(`/${analysisId}/export`, exportRequest);
  }

  /**
   * Export multiple analyses
   */
  async exportMultipleAnalyses(
    analysisIds: string[],
    exportRequest: Partial<ExportRequest>
  ): Promise<ExportJob> {
    return this.post('/export', {
      analysis_ids: analysisIds,
      ...exportRequest
    });
  }

  /**
   * Get export job status and download link
   */
  async getExportJob(jobId: string): Promise<ExportJob> {
    return this.get(`/export/${jobId}`);
  }

  // ====================
  // Search and Discovery
  // ====================

  /**
   * Search analyses by text query with advanced filtering
   */
  async searchAnalyses(
    query: string,
    filters: AnalysisFilter = {},
    options: {
      limit?: number;
      offset?: number;
      sort_by?: string;
      sort_desc?: boolean;
    } = {}
  ): Promise<PaginatedResponse<AnalysisSummary>> {
    const params = {
      q: query,
      ...filters,
      ...options
    };
    const queryParams = this.buildQueryParams(params);
    return this.get(`/search?${queryParams}`);
  }

  /**
   * Get analysis suggestions based on current context
   */
  async getAnalysisSuggestions(
    contextAnalysisId?: string,
    limit: number = 5
  ): Promise<{
    suggestions: AnalysisSummary[];
    reasoning: string[];
  }> {
    const params = contextAnalysisId 
      ? { context: contextAnalysisId, limit }
      : { limit };
    const queryParams = this.buildQueryParams(params);
    return this.get(`/suggestions?${queryParams}`);
  }

  // ====================
  // Analysis Templates and Presets
  // ====================

  /**
   * Get available analysis templates
   */
  async getAnalysisTemplates(): Promise<{
    templates: AnalysisRequest[];
    categories: string[];
  }> {
    return this.get('/templates');
  }

  /**
   * Create analysis from template
   */
  async createFromTemplate(
    templateId: string,
    overrides: Partial<AnalysisRequest> = {}
  ): Promise<Analysis> {
    return this.post('/templates/create', {
      template_id: templateId,
      overrides
    });
  }

  // ====================
  // Real-time Updates
  // ====================

  /**
   * Subscribe to analysis status updates (if WebSocket is available)
   */
  subscribeToAnalysisUpdates(
    analysisId: string,
    onUpdate: (progress: AnalysisProgress) => void,
    onError?: (error: Error) => void
  ): () => void {
    // Implementation would depend on WebSocket setup
    // For now, return polling implementation
    const intervalId = setInterval(async () => {
      try {
        const progress = await this.getAnalysisProgress(analysisId);
        onUpdate(progress);
        
        // Stop polling if analysis is complete
        if (['completed', 'failed', 'cancelled'].includes(progress.status)) {
          clearInterval(intervalId);
        }
      } catch (error) {
        if (onError) {
          onError(error as Error);
        }
      }
    }, 2000); // Poll every 2 seconds

    // Return unsubscribe function
    return () => clearInterval(intervalId);
  }
}

// Create and export singleton instance
export const analysisService = new AnalysisService();