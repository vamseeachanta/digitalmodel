'use client';

import React from 'react';
import {
  Box,
  Paper,
  Typography,
  Button,
  Alert,
  AlertTitle,
  Stack,
  Divider,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import { ErrorOutline, Refresh, BugReport } from '@mui/icons-material';

const ErrorContainer = styled(Paper)(({ theme }) => ({
  padding: theme.spacing(4),
  margin: theme.spacing(2),
  textAlign: 'center',
  backgroundColor: theme.palette.background.paper,
  border: `1px solid ${theme.palette.error.light}`,
  borderRadius: theme.spacing(2),
  maxWidth: 600,
  marginLeft: 'auto',
  marginRight: 'auto',
}));

const ErrorIcon = styled(ErrorOutline)(({ theme }) => ({
  fontSize: '4rem',
  color: theme.palette.error.main,
  marginBottom: theme.spacing(2),
}));

interface ErrorBoundaryState {
  hasError: boolean;
  error?: Error;
  errorInfo?: React.ErrorInfo;
}

interface ErrorBoundaryProps {
  children: React.ReactNode;
  fallback?: React.ComponentType<{ error?: Error; resetError: () => void }>;
  onError?: (error: Error, errorInfo: React.ErrorInfo) => void;
}

class ErrorBoundary extends React.Component<ErrorBoundaryProps, ErrorBoundaryState> {
  constructor(props: ErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false };
  }

  static getDerivedStateFromError(error: Error): ErrorBoundaryState {
    return {
      hasError: true,
      error,
    };
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo) {
    console.error('OrcaFlex Dashboard Error:', error, errorInfo);
    
    this.setState({
      error,
      errorInfo,
    });

    // Call optional error handler
    if (this.props.onError) {
      this.props.onError(error, errorInfo);
    }
  }

  handleReset = () => {
    this.setState({ hasError: false, error: undefined, errorInfo: undefined });
  };

  render() {
    if (this.state.hasError) {
      if (this.props.fallback) {
        const FallbackComponent = this.props.fallback;
        return <FallbackComponent error={this.state.error} resetError={this.handleReset} />;
      }

      return (
        <Box sx={{ p: 2, minHeight: '400px', display: 'flex', alignItems: 'center' }}>
          <ErrorContainer elevation={3}>
            <ErrorIcon />
            
            <Typography variant="h5" component="h2" gutterBottom color="error">
              OrcaFlex Analysis Error
            </Typography>
            
            <Typography variant="body1" color="text.secondary" sx={{ mb: 3 }}>
              An unexpected error occurred while processing the marine engineering analysis.
              This may be due to invalid input parameters or system limitations.
            </Typography>

            <Alert severity="error" sx={{ mb: 3, textAlign: 'left' }}>
              <AlertTitle>Technical Details</AlertTitle>
              <Typography variant="body2" component="div">
                <strong>Error:</strong> {this.state.error?.message}
              </Typography>
              {process.env.NODE_ENV === 'development' && this.state.error?.stack && (
                <Box
                  component="pre"
                  sx={{
                    mt: 1,
                    p: 1,
                    backgroundColor: 'grey.100',
                    borderRadius: 1,
                    fontSize: '0.75rem',
                    overflow: 'auto',
                    maxHeight: 150,
                  }}
                >
                  {this.state.error.stack}
                </Box>
              )}
            </Alert>

            <Divider sx={{ mb: 3 }} />

            <Stack direction="row" spacing={2} justifyContent="center">
              <Button
                variant="contained"
                startIcon={<Refresh />}
                onClick={this.handleReset}
                color="primary"
              >
                Retry Analysis
              </Button>
              
              <Button
                variant="outlined"
                startIcon={<BugReport />}
                onClick={() => {
                  console.log('Error details:', {
                    error: this.state.error,
                    errorInfo: this.state.errorInfo,
                    timestamp: new Date().toISOString(),
                  });
                  alert('Error details logged to console for debugging');
                }}
              >
                Report Issue
              </Button>
            </Stack>

            <Typography variant="caption" color="text.secondary" sx={{ mt: 2, display: 'block' }}>
              If this error persists, please check your input parameters and system configuration.
            </Typography>
          </ErrorContainer>
        </Box>
      );
    }

    return this.props.children;
  }
}

export default ErrorBoundary;