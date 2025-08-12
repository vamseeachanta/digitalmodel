import React, { useState } from 'react';
import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  FormControl,
  InputLabel,
  Select,
  MenuItem,
  FormControlLabel,
  Checkbox,
  TextField,
  Box,
  Typography,
  CircularProgress,
  Alert
} from '@mui/material';
import { Download, Close } from '@mui/icons-material';
import { exportAPI } from '@services/api';
import { saveAs } from 'file-saver';

interface ExportDialogProps {
  open: boolean;
  onClose: () => void;
  exportType: 'chart' | 'data' | 'report';
  data?: any;
}

const ExportDialog: React.FC<ExportDialogProps> = ({
  open,
  onClose,
  exportType,
  data
}) => {
  const [format, setFormat] = useState('');
  const [includeMetadata, setIncludeMetadata] = useState(true);
  const [includeStatistics, setIncludeStatistics] = useState(true);
  const [fileName, setFileName] = useState('orcaflex_export');
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  const getFormatOptions = () => {
    switch (exportType) {
      case 'chart':
        return ['png', 'svg', 'pdf'];
      case 'data':
        return ['csv', 'xlsx', 'json'];
      case 'report':
        return ['pdf', 'docx', 'html'];
      default:
        return [];
    }
  };

  const handleExport = async () => {
    setLoading(true);
    setError(null);

    try {
      const exportRequest = {
        data_type: exportType,
        query: data,
        include_metadata: includeMetadata,
        include_statistics: includeStatistics
      };

      let response;
      switch (exportType) {
        case 'chart':
          response = await exportAPI.chart(exportRequest, format);
          break;
        case 'data':
          response = await exportAPI.data(exportRequest, format);
          break;
        case 'report':
          response = await exportAPI.report(exportRequest);
          break;
      }

      // Handle file download
      if (response && response.data) {
        const blob = new Blob([response.data], {
          type: getContentType(format)
        });
        saveAs(blob, `${fileName}.${format}`);
        onClose();
      }
    } catch (err: any) {
      setError(err.message || 'Export failed');
    } finally {
      setLoading(false);
    }
  };

  const getContentType = (format: string) => {
    const types: { [key: string]: string } = {
      'png': 'image/png',
      'svg': 'image/svg+xml',
      'pdf': 'application/pdf',
      'csv': 'text/csv',
      'xlsx': 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
      'json': 'application/json',
      'docx': 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
      'html': 'text/html'
    };
    return types[format] || 'application/octet-stream';
  };

  return (
    <Dialog open={open} onClose={onClose} maxWidth="sm" fullWidth>
      <DialogTitle>
        Export {exportType.charAt(0).toUpperCase() + exportType.slice(1)}
      </DialogTitle>
      <DialogContent>
        <Box sx={{ pt: 2 }}>
          <FormControl fullWidth sx={{ mb: 2 }}>
            <InputLabel>Format</InputLabel>
            <Select
              value={format}
              onChange={(e) => setFormat(e.target.value)}
              label="Format"
            >
              {getFormatOptions().map((fmt) => (
                <MenuItem key={fmt} value={fmt}>
                  {fmt.toUpperCase()}
                </MenuItem>
              ))}
            </Select>
          </FormControl>

          <TextField
            fullWidth
            label="File Name"
            value={fileName}
            onChange={(e) => setFileName(e.target.value)}
            sx={{ mb: 2 }}
            helperText="Without extension"
          />

          {exportType === 'data' && (
            <>
              <FormControlLabel
                control={
                  <Checkbox
                    checked={includeMetadata}
                    onChange={(e) => setIncludeMetadata(e.target.checked)}
                  />
                }
                label="Include Metadata"
              />
              <FormControlLabel
                control={
                  <Checkbox
                    checked={includeStatistics}
                    onChange={(e) => setIncludeStatistics(e.target.checked)}
                  />
                }
                label="Include Statistics"
              />
            </>
          )}

          {format && (
            <Box sx={{ mt: 2, p: 2, bgcolor: 'grey.100', borderRadius: 1 }}>
              <Typography variant="body2" color="textSecondary">
                Export Information:
              </Typography>
              <Typography variant="body2">
                • Format: {format.toUpperCase()}
              </Typography>
              <Typography variant="body2">
                • File: {fileName}.{format}
              </Typography>
              {exportType === 'chart' && (
                <Typography variant="body2">
                  • Resolution: High quality (2x)
                </Typography>
              )}
            </Box>
          )}

          {error && (
            <Alert severity="error" sx={{ mt: 2 }}>
              {error}
            </Alert>
          )}
        </Box>
      </DialogContent>
      <DialogActions>
        <Button onClick={onClose} startIcon={<Close />}>
          Cancel
        </Button>
        <Button
          onClick={handleExport}
          variant="contained"
          disabled={!format || loading}
          startIcon={loading ? <CircularProgress size={20} /> : <Download />}
        >
          {loading ? 'Exporting...' : 'Export'}
        </Button>
      </DialogActions>
    </Dialog>
  );
};

export default ExportDialog;