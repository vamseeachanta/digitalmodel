'use client';

import React from 'react';
import { Box, CircularProgress, Typography, Fade } from '@mui/material';
import { styled } from '@mui/material/styles';

const StyledBox = styled(Box)(({ theme }) => ({
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
  justifyContent: 'center',
  padding: theme.spacing(3),
  minHeight: '200px',
  color: theme.palette.primary.main,
}));

const MarineBlueProgress = styled(CircularProgress)(({ theme }) => ({
  color: '#1565C0', // Marine engineering blue
  '& .MuiCircularProgress-circle': {
    strokeLinecap: 'round',
  },
}));

interface LoadingSpinnerProps {
  message?: string;
  size?: number;
  fullScreen?: boolean;
  overlay?: boolean;
}

const LoadingSpinner: React.FC<LoadingSpinnerProps> = ({
  message = 'Processing analysis data...',
  size = 40,
  fullScreen = false,
  overlay = false,
}) => {
  const content = (
    <Fade in timeout={300}>
      <StyledBox
        sx={{
          ...(fullScreen && {
            position: 'fixed',
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            backgroundColor: overlay ? 'rgba(255, 255, 255, 0.9)' : 'transparent',
            zIndex: 9999,
            minHeight: '100vh',
          }),
        }}
      >
        <MarineBlueProgress size={size} thickness={4} />
        {message && (
          <Typography
            variant="body2"
            sx={{
              mt: 2,
              color: 'text.secondary',
              fontWeight: 500,
              textAlign: 'center',
              maxWidth: 300,
            }}
          >
            {message}
          </Typography>
        )}
      </StyledBox>
    </Fade>
  );

  return content;
};

export default LoadingSpinner;