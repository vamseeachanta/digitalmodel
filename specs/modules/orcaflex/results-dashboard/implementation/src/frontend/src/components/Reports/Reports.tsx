import React, { useState } from 'react';
import {
  Box,
  Paper,
  Typography,
  Button,
  Grid,
  TextField,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  Checkbox,
  FormControlLabel,
  FormGroup,
  Stepper,
  Step,
  StepLabel,
  Card,
  CardContent,
  Alert,
  LinearProgress
} from '@mui/material';
import {
  Description,
  PictureAsPdf,
  TableChart,
  Assessment,
  Send,
  Preview
} from '@mui/icons-material';
import { exportAPI } from '@services/api';

const Reports: React.FC = () => {
  const [activeStep, setActiveStep] = useState(0);
  const [reportConfig, setReportConfig] = useState({
    title: '',
    author: '',
    template: 'standard',
    sections: {
      executiveSummary: true,
      polarAnalysis: true,
      timeSeriesAnalysis: true,
      statisticalSummary: true,
      comparisonAnalysis: false,
      recommendations: false
    },
    format: 'pdf',
    includeRawData: false
  });
  const [generating, setGenerating] = useState(false);
  const [progress, setProgress] = useState(0);

  const steps = ['Configure Report', 'Select Data', 'Generate'];

  const handleNext = () => {
    setActiveStep((prevStep) => prevStep + 1);
  };

  const handleBack = () => {
    setActiveStep((prevStep) => prevStep - 1);
  };

  const handleGenerateReport = async () => {
    setGenerating(true);
    setProgress(0);

    // Simulate progress
    const progressInterval = setInterval(() => {
      setProgress((prev) => {
        if (prev >= 90) {
          clearInterval(progressInterval);
          return 90;
        }
        return prev + 10;
      });
    }, 500);

    try {
      const response = await exportAPI.report({
        data_type: 'report',
        query: {
          case: 'example_case',
          include_statistics: true
        },
        include_metadata: true,
        include_statistics: reportConfig.includeRawData
      });

      setProgress(100);
      clearInterval(progressInterval);

      // Handle successful report generation
      console.log('Report generated:', response.data);
    } catch (error) {
      console.error('Failed to generate report:', error);
      clearInterval(progressInterval);
    } finally {
      setGenerating(false);
    }
  };

  const renderStepContent = (step: number) => {
    switch (step) {
      case 0:
        return (
          <Grid container spacing={3}>
            <Grid item xs={12} md={6}>
              <TextField
                fullWidth
                label="Report Title"
                value={reportConfig.title}
                onChange={(e) => setReportConfig({ ...reportConfig, title: e.target.value })}
                placeholder="OrcaFlex Analysis Report"
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <TextField
                fullWidth
                label="Author"
                value={reportConfig.author}
                onChange={(e) => setReportConfig({ ...reportConfig, author: e.target.value })}
                placeholder="Engineering Team"
              />
            </Grid>
            <Grid item xs={12} md={6}>
              <FormControl fullWidth>
                <InputLabel>Template</InputLabel>
                <Select
                  value={reportConfig.template}
                  onChange={(e) => setReportConfig({ ...reportConfig, template: e.target.value })}
                  label="Template"
                >
                  <MenuItem value="standard">Standard Report</MenuItem>
                  <MenuItem value="executive">Executive Summary</MenuItem>
                  <MenuItem value="technical">Technical Detail</MenuItem>
                  <MenuItem value="compliance">Compliance Report</MenuItem>
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12} md={6}>
              <FormControl fullWidth>
                <InputLabel>Format</InputLabel>
                <Select
                  value={reportConfig.format}
                  onChange={(e) => setReportConfig({ ...reportConfig, format: e.target.value })}
                  label="Format"
                >
                  <MenuItem value="pdf">PDF</MenuItem>
                  <MenuItem value="docx">Word Document</MenuItem>
                  <MenuItem value="html">HTML</MenuItem>
                </Select>
              </FormControl>
            </Grid>
            <Grid item xs={12}>
              <Typography variant="subtitle2" gutterBottom>
                Report Sections
              </Typography>
              <FormGroup>
                <Grid container>
                  <Grid item xs={6}>
                    <FormControlLabel
                      control={
                        <Checkbox
                          checked={reportConfig.sections.executiveSummary}
                          onChange={(e) => setReportConfig({
                            ...reportConfig,
                            sections: { ...reportConfig.sections, executiveSummary: e.target.checked }
                          })}
                        />
                      }
                      label="Executive Summary"
                    />
                  </Grid>
                  <Grid item xs={6}>
                    <FormControlLabel
                      control={
                        <Checkbox
                          checked={reportConfig.sections.polarAnalysis}
                          onChange={(e) => setReportConfig({
                            ...reportConfig,
                            sections: { ...reportConfig.sections, polarAnalysis: e.target.checked }
                          })}
                        />
                      }
                      label="Polar Analysis"
                    />
                  </Grid>
                  <Grid item xs={6}>
                    <FormControlLabel
                      control={
                        <Checkbox
                          checked={reportConfig.sections.timeSeriesAnalysis}
                          onChange={(e) => setReportConfig({
                            ...reportConfig,
                            sections: { ...reportConfig.sections, timeSeriesAnalysis: e.target.checked }
                          })}
                        />
                      }
                      label="Time Series Analysis"
                    />
                  </Grid>
                  <Grid item xs={6}>
                    <FormControlLabel
                      control={
                        <Checkbox
                          checked={reportConfig.sections.statisticalSummary}
                          onChange={(e) => setReportConfig({
                            ...reportConfig,
                            sections: { ...reportConfig.sections, statisticalSummary: e.target.checked }
                          })}
                        />
                      }
                      label="Statistical Summary"
                    />
                  </Grid>
                  <Grid item xs={6}>
                    <FormControlLabel
                      control={
                        <Checkbox
                          checked={reportConfig.sections.comparisonAnalysis}
                          onChange={(e) => setReportConfig({
                            ...reportConfig,
                            sections: { ...reportConfig.sections, comparisonAnalysis: e.target.checked }
                          })}
                        />
                      }
                      label="Comparison Analysis"
                    />
                  </Grid>
                  <Grid item xs={6}>
                    <FormControlLabel
                      control={
                        <Checkbox
                          checked={reportConfig.sections.recommendations}
                          onChange={(e) => setReportConfig({
                            ...reportConfig,
                            sections: { ...reportConfig.sections, recommendations: e.target.checked }
                          })}
                        />
                      }
                      label="Recommendations"
                    />
                  </Grid>
                </Grid>
              </FormGroup>
            </Grid>
          </Grid>
        );

      case 1:
        return (
          <Box>
            <Alert severity="info" sx={{ mb: 2 }}>
              Select the data to include in your report. You can choose specific cases, components, and time ranges.
            </Alert>
            <Grid container spacing={3}>
              <Grid item xs={12}>
                <Typography variant="subtitle2" gutterBottom>
                  Data Selection (Placeholder - connect to actual data)
                </Typography>
                <FormControlLabel
                  control={
                    <Checkbox
                      checked={reportConfig.includeRawData}
                      onChange={(e) => setReportConfig({ ...reportConfig, includeRawData: e.target.checked })}
                    />
                  }
                  label="Include raw data appendix"
                />
              </Grid>
            </Grid>
          </Box>
        );

      case 2:
        return (
          <Box>
            <Typography variant="h6" gutterBottom>
              Report Summary
            </Typography>
            <Grid container spacing={2}>
              <Grid item xs={12} md={6}>
                <Card variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2" color="textSecondary">
                      Title
                    </Typography>
                    <Typography variant="body1">
                      {reportConfig.title || 'Untitled Report'}
                    </Typography>
                  </CardContent>
                </Card>
              </Grid>
              <Grid item xs={12} md={6}>
                <Card variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2" color="textSecondary">
                      Author
                    </Typography>
                    <Typography variant="body1">
                      {reportConfig.author || 'Anonymous'}
                    </Typography>
                  </CardContent>
                </Card>
              </Grid>
              <Grid item xs={12} md={6}>
                <Card variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2" color="textSecondary">
                      Template
                    </Typography>
                    <Typography variant="body1">
                      {reportConfig.template}
                    </Typography>
                  </CardContent>
                </Card>
              </Grid>
              <Grid item xs={12} md={6}>
                <Card variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2" color="textSecondary">
                      Format
                    </Typography>
                    <Typography variant="body1">
                      {reportConfig.format.toUpperCase()}
                    </Typography>
                  </CardContent>
                </Card>
              </Grid>
              <Grid item xs={12}>
                <Card variant="outlined">
                  <CardContent>
                    <Typography variant="subtitle2" color="textSecondary">
                      Included Sections
                    </Typography>
                    <Typography variant="body2">
                      {Object.entries(reportConfig.sections)
                        .filter(([_, enabled]) => enabled)
                        .map(([section, _]) => section)
                        .join(', ')}
                    </Typography>
                  </CardContent>
                </Card>
              </Grid>
            </Grid>

            {generating && (
              <Box sx={{ mt: 3 }}>
                <Typography variant="body2" gutterBottom>
                  Generating report...
                </Typography>
                <LinearProgress variant="determinate" value={progress} />
              </Box>
            )}
          </Box>
        );

      default:
        return null;
    }
  };

  return (
    <Box>
      <Typography variant="h4" gutterBottom>
        Report Generation
      </Typography>

      <Paper sx={{ p: 3, mb: 3 }}>
        <Stepper activeStep={activeStep}>
          {steps.map((label) => (
            <Step key={label}>
              <StepLabel>{label}</StepLabel>
            </Step>
          ))}
        </Stepper>
      </Paper>

      <Paper sx={{ p: 3 }}>
        {renderStepContent(activeStep)}

        <Box sx={{ mt: 3, display: 'flex', justifyContent: 'space-between' }}>
          <Button
            disabled={activeStep === 0}
            onClick={handleBack}
          >
            Back
          </Button>
          <Box>
            {activeStep === steps.length - 1 ? (
              <>
                <Button
                  variant="outlined"
                  startIcon={<Preview />}
                  sx={{ mr: 1 }}
                  disabled={generating}
                >
                  Preview
                </Button>
                <Button
                  variant="contained"
                  startIcon={<PictureAsPdf />}
                  onClick={handleGenerateReport}
                  disabled={generating}
                >
                  Generate Report
                </Button>
              </>
            ) : (
              <Button
                variant="contained"
                onClick={handleNext}
              >
                Next
              </Button>
            )}
          </Box>
        </Box>
      </Paper>

      <Grid container spacing={3} sx={{ mt: 2 }}>
        <Grid item xs={12} md={4}>
          <Card>
            <CardContent>
              <Description sx={{ fontSize: 40, color: 'primary.main' }} />
              <Typography variant="h6">Templates</Typography>
              <Typography variant="body2" color="textSecondary">
                Pre-configured report templates for common use cases
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} md={4}>
          <Card>
            <CardContent>
              <TableChart sx={{ fontSize: 40, color: 'secondary.main' }} />
              <Typography variant="h6">Data Tables</Typography>
              <Typography variant="body2" color="textSecondary">
                Include comprehensive data tables and statistics
              </Typography>
            </CardContent>
          </Card>
        </Grid>
        <Grid item xs={12} md={4}>
          <Card>
            <CardContent>
              <Assessment sx={{ fontSize: 40, color: 'success.main' }} />
              <Typography variant="h6">Analytics</Typography>
              <Typography variant="body2" color="textSecondary">
                Advanced analysis and recommendations
              </Typography>
            </CardContent>
          </Card>
        </Grid>
      </Grid>
    </Box>
  );
};

export default Reports;