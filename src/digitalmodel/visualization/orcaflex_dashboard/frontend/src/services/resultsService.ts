/**
 * Results service for API operations
 */

import { BaseApiService } from './api';
import {
  SimulationResult,
  ResultsQuery,
  ResultsMetadata,
  TimeSeriesData,
  StatisticalSummary,
  ResultsFilters,
  ResultsPagination,
  ResultsListResponse,
  ExportConfiguration,
} from '@/types/results';

export class ResultsService extends BaseApiService {
  constructor() {
    super('/api/results');
  }

  async getResults(
    filters: ResultsFilters = {},
    pagination: ResultsPagination = { limit: 50, offset: 0 }
  ): Promise<SimulationResult[]> {
    const params = {
      ...filters,
      ...pagination,
      ...(filters.date_range && {
        start_date: filters.date_range.start,
        end_date: filters.date_range.end,
      }),
    };

    const queryString = this.buildQueryParams(params);
    return this.get(`?${queryString}`);
  }

  async getResult(id: string): Promise<SimulationResult> {
    return this.get(`/${id}`);
  }

  async getResultMetadata(id: string): Promise<ResultsMetadata> {
    return this.get(`/${id}/metadata`);
  }

  async queryResultData(id: string, query: ResultsQuery): Promise<SimulationResult> {
    return this.post(`/${id}/query`, query);
  }

  async getTimeSeriesData(
    id: string, 
    query: Partial<ResultsQuery> = {}
  ): Promise<TimeSeriesData[]> {
    const params = {
      ...(query.variable_names && { variable_names: query.variable_names.join(',') }),
      ...(query.object_names && { object_names: query.object_names.join(',') }),
      ...(query.time_range && { 
        time_start: query.time_range[0], 
        time_end: query.time_range[1] 
      }),
    };

    const queryString = this.buildQueryParams(params);
    return this.get(`/${id}/timeseries?${queryString}`);
  }

  async getStatistics(id: string, variables?: string[]): Promise<StatisticalSummary[]> {
    const params = variables ? { variable_names: variables.join(',') } : {};
    const queryString = this.buildQueryParams(params);
    return this.get(`/${id}/statistics?${queryString}`);
  }

  async exportResult(id: string, config: ExportConfiguration): Promise<Blob> {
    const params = {
      format: config.format,
      ...(config.variables && { variables: config.variables.join(',') }),
    };

    const queryString = this.buildQueryParams(params);
    const response = await this.client.get(
      `${this.baseEndpoint}/${id}/export?${queryString}`,
      { responseType: 'blob' }
    );

    return response.data;
  }

  async deleteResult(id: string): Promise<void> {
    return this.delete(`/${id}`);
  }
}

// Export singleton instance
export const resultsService = new ResultsService();