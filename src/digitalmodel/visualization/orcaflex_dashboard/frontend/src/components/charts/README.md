# Interactive Polar Plot Components for OrcaFlex Results Dashboard

This directory contains a comprehensive set of React components for displaying interactive polar plots in marine engineering applications. The components are specifically designed for visualizing OrcaFlex simulation results with response quantities plotted against environmental headings.

## Components Overview

### 1. PolarPlot.tsx
Main interactive polar plot component using Plotly.js for rendering.

**Key Features:**
- Interactive polar plots with 24-point datasets (0° to 345° in 15° increments)
- Display response quantities: R(θ) vs. θ ∈ [0°, 345°]
- Support for min/max values with different colors
- Hover tooltips with exact values and headings
- Color-coded severity levels (green/yellow/red zones)
- Multiple loading conditions overlay
- Zoom, pan, and selection capabilities

**Mathematical Implementation:**
- Polar response: R(θ) = |F(θ)| or R(θ) = √(x(θ)² + y(θ)²)
- Statistical overlays with confidence bands
- Engineering limits display: operational (R_op) and survival (R_surv)
- RAO support: RAO(ω,θ) = X(ω,θ)/A(ω)

### 2. PolarPlotControls.tsx
Comprehensive controls for customizing polar plot display and data processing.

**Features:**
- Dataset visibility toggles
- Data processing options (interpolation, smoothing, filtering)
- Visualization settings (grid, markers, limits)
- Interaction controls (zoom, pan, selection)
- Styling options (colors, line width, marker size)

### 3. MultiPolarGrid.tsx
Grid layout for displaying multiple polar plots simultaneously.

**Features:**
- Multiple layout options (1x1, 2x2, 3x3, auto)
- Synchronized view capabilities
- Comparison mode for multiple plots
- Batch operations and export
- Responsive design with fullscreen support

### 4. PolarDataProcessor.ts
Comprehensive data processing utilities for polar plot data.

**Core Functions:**
- `cartesianToPolar()` - Convert Cartesian to polar coordinates
- `calculateRAO()` - Calculate Response Amplitude Operators  
- `interpolatePolarData()` - Fill missing data points
- `calculateStatisticalOverlay()` - Generate confidence bands
- `applySmoothingFilter()` - Smooth response data
- `filterOutliers()` - Remove statistical outliers
- `processPolarData()` - Main processing pipeline

### 5. PolarLegend.tsx
Custom legend component with engineering metadata display.

**Features:**
- Dataset information with statistics
- Engineering limits display
- Severity level indicators
- Units and conventions display
- Interactive dataset toggles

### 6. PolarExport.tsx
Advanced export functionality supporting multiple formats.

**Supported Formats:**
- PNG: High-quality raster images
- SVG: Scalable vector graphics
- PDF: Professional documents with metadata
- CSV: Raw data export
- JSON: Structured data with metadata

**Export Options:**
- Custom dimensions and DPI
- Quality settings
- Metadata inclusion
- Batch export capabilities

## Installation and Dependencies

### Required Dependencies
```bash
npm install react plotly.js react-plotly.js @mui/material @mui/icons-material
npm install jspdf html2canvas file-saver lodash
```

### TypeScript Types
```bash
npm install -D @types/plotly.js @types/lodash
```

## Usage Examples

### Basic Polar Plot
```tsx
import React from 'react';
import { PolarPlot, createSamplePolarData, createSampleLimits } from './components/charts';

const ExampleComponent = () => {
  const dataset = createSamplePolarData('Heave', 2.5, 'm');
  const limits = createSampleLimits('Heave');

  return (
    <PolarPlot
      datasets={[dataset]}
      limits={limits}
      title="Heave Response vs. Heading"
      showGrid={true}
      showMarkers={true}
      enableZoom={true}
    />
  );
};
```

### Multi-Plot Grid with Controls
```tsx
import React, { useState } from 'react';
import { 
  MultiPolarGrid, 
  PolarPlotControls, 
  VisualizationOptions,
  ProcessingOptions 
} from './components/charts';

const DashboardExample = () => {
  const [visualizationOptions, setVisualizationOptions] = useState<VisualizationOptions>({
    showGrid: true,
    showMarkers: true,
    showLimits: true,
    enableZoom: true,
    plotSize: 'medium',
    colorScheme: 'default'
  });

  const [processingOptions, setProcessingOptions] = useState<ProcessingOptions>({
    headingIncrement: 15,
    interpolateGaps: true,
    smoothing: false,
    filterOutliers: false
  });

  return (
    <Box sx={{ display: 'flex', gap: 2 }}>
      <MultiPolarGrid
        datasets={datasets}
        limits={limits}
        visualizationOptions={visualizationOptions}
        gridLayout="2x2"
        onLayoutChange={setGridLayout}
        synchronized={true}
      />
      
      <PolarPlotControls
        datasets={datasets}
        processingOptions={processingOptions}
        visualizationOptions={visualizationOptions}
        onProcessingOptionsChange={setProcessingOptions}
        onVisualizationChange={setVisualizationOptions}
        onDatasetToggle={handleDatasetToggle}
      />
    </Box>
  );
};
```

### Data Processing Pipeline
```tsx
import { 
  processPolarData,
  calculateStatisticalOverlay,
  determineSeverity 
} from './components/charts';

// Process raw polar data
const processedData = processPolarData(rawData, {
  headingIncrement: 15,
  interpolateGaps: true,
  smoothing: true,
  filterOutliers: true
});

// Calculate statistical overlay for multiple datasets
const statisticalOverlay = calculateStatisticalOverlay(datasets, 0.95);

// Apply severity classification
const dataWithSeverity = processedData.map(point => ({
  ...point,
  severity: determineSeverity(point.response, point.loadCase, limits)
}));
```

### Export Functionality
```tsx
import React, { useRef } from 'react';
import { PolarPlot, PolarExport } from './components/charts';

const ExportExample = () => {
  const plotRef = useRef<HTMLDivElement>(null);

  return (
    <Box>
      <div ref={plotRef}>
        <PolarPlot datasets={datasets} />
      </div>
      
      <PolarExport
        plotRef={plotRef}
        datasets={datasets}
        plotTitle="Marine Response Analysis"
        onExportComplete={(success, error) => {
          if (success) {
            console.log('Export completed successfully');
          } else {
            console.error('Export failed:', error);
          }
        }}
      />
    </Box>
  );
};
```

## Data Format Specifications

### PolarDataPoint Interface
```typescript
interface PolarDataPoint {
  heading: number;       // θ in degrees [0°, 345°]
  response: number;      // R(θ) - response amplitude
  phase?: number;        // Phase angle in degrees
  frequency?: number;    // ω in rad/s or Hz
  loadCase?: string;     // Loading condition identifier
  severity?: 'safe' | 'caution' | 'critical';
  timestamp?: Date;
}
```

### PolarDataSet Interface
```typescript
interface PolarDataSet {
  id: string;
  label: string;
  data: PolarDataPoint[];
  color: string;
  lineWidth?: number;
  showMarkers?: boolean;
  visible?: boolean;
  type: 'response' | 'rao' | 'statistical';
  units: string;
  responseType: string;
}
```

## Marine Engineering Applications

### Typical Response Types
- **Heave**: Vertical motion response (m)
- **Pitch**: Rotational response about Y-axis (degrees)
- **Roll**: Rotational response about X-axis (degrees) 
- **Surge**: Longitudinal motion response (m)
- **Sway**: Lateral motion response (m)
- **Yaw**: Rotational response about Z-axis (degrees)

### Heading Conventions
- **0°**: Head seas (waves from ahead)
- **90°**: Beam seas (waves from port/starboard)
- **180°**: Following seas (waves from behind)
- **270°**: Beam seas (waves from starboard/port)

### Mathematical Foundations

#### Response Amplitude Operator (RAO)
```
RAO(ω,θ) = X(ω,θ) / A(ω)
```
Where:
- X(ω,θ): Response amplitude at frequency ω and heading θ
- A(ω): Wave amplitude at frequency ω

#### Polar Response Magnitude
```
R(θ) = √(x(θ)² + y(θ)²)
```
Where:
- x(θ), y(θ): Cartesian components of response

#### Statistical Analysis
- Mean response: μ = Σ R(θ) / n
- Standard deviation: σ = √(Σ(R(θ) - μ)² / n)
- Confidence bands: μ ± z_α/2 × σ

## Performance Considerations

### Optimization Features
- **Memoized calculations**: Heavy computations are cached
- **Lazy loading**: Components load data on demand
- **Virtual scrolling**: For large datasets in grids
- **WebGL rendering**: Hardware acceleration for complex plots
- **Data streaming**: Progressive loading for real-time updates

### Memory Management
- Automatic cleanup of plot instances
- Efficient data structures for large datasets
- Garbage collection of unused computations
- Optimized rendering for multiple simultaneous plots

## Customization and Theming

### Material-UI Integration
All components fully support Material-UI theming:

```tsx
import { createTheme, ThemeProvider } from '@mui/material/styles';

const theme = createTheme({
  palette: {
    primary: { main: '#1976d2' },
    secondary: { main: '#dc004e' }
  }
});

<ThemeProvider theme={theme}>
  <PolarPlot datasets={datasets} />
</ThemeProvider>
```

### Custom Styling
Components accept `sx` props for custom styling:

```tsx
<PolarPlot 
  datasets={datasets}
  sx={{ 
    border: '2px solid #ccc',
    borderRadius: 2,
    boxShadow: 3
  }}
/>
```

## Testing and Validation

### Unit Tests
- Component rendering tests
- Data processing validation
- Mathematical function accuracy
- Export functionality verification

### Integration Tests  
- Multi-component interaction
- State synchronization
- Performance benchmarking
- Cross-browser compatibility

### Marine Engineering Validation
- Comparison with reference solutions
- Industry standard compliance
- Physical constraint validation
- Numerical stability testing

## Browser Compatibility

**Supported Browsers:**
- Chrome 80+
- Firefox 75+
- Safari 13+
- Edge 80+

**Required Features:**
- ES2018 support
- WebGL 2.0 (optional, for performance)
- Canvas 2D API
- File API (for export functionality)

## Troubleshooting

### Common Issues

1. **Plotly.js Loading Errors**
   ```bash
   # Ensure correct Plotly.js installation
   npm install plotly.js@latest react-plotly.js@latest
   ```

2. **Performance Issues with Large Datasets**
   ```tsx
   // Enable data sampling for better performance
   <PolarPlot 
     datasets={datasets}
     maxDataPoints={1000}
     enableVirtualization={true}
   />
   ```

3. **Export Failures**
   ```tsx
   // Ensure proper plot ref assignment
   const plotRef = useRef<HTMLDivElement>(null);
   
   return (
     <div ref={plotRef}>
       <PolarPlot datasets={datasets} />
     </div>
   );
   ```

### Debug Mode
Enable debug mode for detailed logging:

```tsx
<PolarPlot 
  datasets={datasets}
  debug={true}
  onDebug={(message) => console.log('[PolarPlot]', message)}
/>
```

## Contributing

For contributing to these components:

1. Follow the established coding patterns
2. Maintain TypeScript strict mode compliance
3. Add comprehensive tests for new features
4. Update documentation for API changes
5. Validate against marine engineering requirements

## License

These components are part of the Digital Model platform and follow the project's licensing terms.