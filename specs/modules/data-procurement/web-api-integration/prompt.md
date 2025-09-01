# Prompt Documentation

> Created: 2025-01-09
> Spec: Web API Integration for Data Procurement
> Context: DigitalModel Repository

## Original Prompt

```
/create-spec module data procurement 

Research if web apis exist to avoid downloading the data on to the repo. If yes, write tests
```

## Prompt Analysis

### Intent
- Create a data procurement module that uses web APIs instead of local file storage
- Research available APIs for marine/offshore engineering data
- Ensure comprehensive testing of API integrations

### Key Requirements Identified
1. **Avoid Local Storage**: No data files downloaded to repository
2. **Web API Integration**: Use external APIs for real-time data access
3. **Testing Focus**: Comprehensive test coverage with mock responses
4. **Module Creation**: New module in the DigitalModel architecture

## Research Findings

### Available Marine Data APIs
1. **NOAA APIs**
   - CO-OPS Data Retrieval API for tides and currents
   - Weather Service API for marine forecasts
   - Free, government-provided data

2. **Open-Meteo Marine API**
   - Free marine weather forecasts
   - Wave height, period, direction
   - No API key required for basic use

3. **StormGlass.io**
   - Comprehensive marine data
   - Requires API key
   - Limited free tier (50 requests/day)

4. **Xweather Maritime API**
   - Professional marine weather data
   - Requires subscription
   - High-resolution forecasts

### Design Decisions
- **API Gateway Pattern**: Centralized management of multiple APIs
- **Intelligent Caching**: Reduce API calls while maintaining data freshness
- **Fallback Strategy**: Automatic failover between APIs
- **Mock Testing**: Comprehensive mocks to avoid API dependencies in tests

## Implementation Approach

### Architecture
```
data_procurement/
├── api_gateway.py       # Central coordinator
├── clients/            # Individual API clients
├── cache/             # Caching layer
├── models/            # Data models
└── utils/             # Helpers
```

### Key Features
1. **Multi-API Support**: NOAA, Open-Meteo, StormGlass, Xweather
2. **Rate Limiting**: Respect API quotas
3. **Caching**: LRU cache with TTL
4. **Retry Logic**: Exponential backoff
5. **Data Transformation**: Standardized output format

## Curated Reuse Prompt

For future enhancements or similar implementations, use this refined prompt:

```
Create a comprehensive web API integration module for [DOMAIN] data procurement that:

1. **Research Phase**
   - Identify available APIs for [SPECIFIC_DATA_TYPES]
   - Document free vs paid tiers
   - Note rate limits and authentication requirements

2. **Architecture Requirements**
   - Implement API Gateway pattern for multiple sources
   - Add intelligent caching with configurable TTL
   - Include fallback mechanisms for API failures
   - Support concurrent requests for performance

3. **Implementation Details**
   - Use async/await for concurrent API calls
   - Implement rate limiting per API provider
   - Add retry logic with exponential backoff
   - Transform all responses to standard format
   - Store credentials in environment variables

4. **Testing Strategy**
   - Create comprehensive mock responses for all APIs
   - Test failover scenarios
   - Verify rate limit compliance
   - Test cache coherence
   - Achieve 90%+ code coverage

5. **Integration Points**
   - Provide Python module interface
   - Add CLI for command-line usage
   - Support YAML configuration
   - Enable batch processing

6. **Documentation**
   - API endpoint documentation
   - Configuration examples
   - Integration guides for existing modules
   - Troubleshooting section

Specific for marine/offshore engineering:
- Wave data (height, period, direction)
- Wind data (speed, direction, gusts)
- Current data (speed, direction)
- Tide levels and predictions
- Water temperature
- Atmospheric pressure
```

## Lessons Learned

### What Worked Well
1. **API Gateway Pattern**: Provides excellent abstraction and flexibility
2. **Pydantic Models**: Strong typing and validation for API responses
3. **Mock-First Testing**: Enables development without API dependencies
4. **YAML Configuration**: User-friendly configuration management

### Challenges Addressed
1. **API Diversity**: Different response formats handled by transformers
2. **Rate Limits**: Token bucket algorithm prevents exceeding quotas
3. **Data Gaps**: Interpolation and fallback strategies
4. **Authentication**: Secure credential management via environment variables

### Future Enhancements
1. **Redis Cache**: For distributed deployments
2. **GraphQL Interface**: More flexible data queries
3. **ML Predictions**: Fill data gaps with predictions
4. **WebSocket Support**: Real-time data streaming
5. **API Cost Optimizer**: Automatic selection based on cost/quality

## Context for AI Agents

When implementing tasks from this spec:
1. **Always use `uv run`**: All Python commands must use the repository's uv environment
2. **No Mock Data in Repo**: Never create sample data files, use API mocks only
3. **Test First**: Write tests before implementation (TDD)
4. **Parallel Processing**: Use concurrent.futures for multi-API calls
5. **Security**: Never hardcode API keys, use environment variables

## Module Integration Notes

This module integrates with:
- **OrcaFlex Module**: Environmental conditions for simulations
- **AQWA Module**: Wave spectra for hydrodynamic analysis
- **Mooring Module**: Current data for mooring analysis
- **Pipeline Module**: Metocean data for pipeline design

## Success Metrics

- ✅ Zero data files in repository
- ✅ &lt;5 second response time for uncached requests
- ✅ 90%+ test coverage
- ✅ Successful failover between APIs
- ✅ Compliance with all API rate limits
- ✅ Seamless integration with existing modules