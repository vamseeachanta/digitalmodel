'use client';

import React, { useState, useMemo } from 'react';
import {
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Paper,
  TableSortLabel,
  TextField,
  Toolbar,
  Typography,
  Box,
  TablePagination,
  Chip,
  IconButton,
  Tooltip,
} from '@mui/material';
import { styled } from '@mui/material/styles';
import { Search, FilterList, GetApp } from '@mui/icons-material';

const StyledTableContainer = styled(TableContainer)(({ theme }) => ({
  maxHeight: 600,
  '& .MuiTableHead-root': {
    backgroundColor: theme.palette.grey[50],
  },
  '& .MuiTableCell-head': {
    fontWeight: 600,
    color: theme.palette.primary.main,
    borderBottom: `2px solid ${theme.palette.primary.main}`,
  },
  '& .MuiTableRow-hover:hover': {
    backgroundColor: theme.palette.action.hover,
  },
}));

const SearchToolbar = styled(Toolbar)(({ theme }) => ({
  paddingLeft: theme.spacing(2),
  paddingRight: theme.spacing(2),
  minHeight: '64px !important',
}));

export interface DataTableColumn {
  id: string;
  label: string;
  minWidth?: number;
  align?: 'right' | 'left' | 'center';
  format?: (value: any) => string | React.ReactNode;
  sortable?: boolean;
  filterable?: boolean;
  type?: 'string' | 'number' | 'date' | 'status';
}

interface DataTableProps {
  title?: string;
  columns: DataTableColumn[];
  rows: Record<string, any>[];
  loading?: boolean;
  searchable?: boolean;
  exportable?: boolean;
  onRowClick?: (row: Record<string, any>) => void;
  onExport?: () => void;
  dense?: boolean;
  stickyHeader?: boolean;
}

type Order = 'asc' | 'desc';

const DataTable: React.FC<DataTableProps> = ({
  title,
  columns,
  rows,
  loading = false,
  searchable = true,
  exportable = false,
  onRowClick,
  onExport,
  dense = false,
  stickyHeader = true,
}) => {
  const [order, setOrder] = useState<Order>('asc');
  const [orderBy, setOrderBy] = useState<string>('');
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(10);
  const [searchText, setSearchText] = useState('');

  const handleRequestSort = (property: string) => {
    const isAsc = orderBy === property && order === 'asc';
    setOrder(isAsc ? 'desc' : 'asc');
    setOrderBy(property);
  };

  const handleChangePage = (_event: unknown, newPage: number) => {
    setPage(newPage);
  };

  const handleChangeRowsPerPage = (event: React.ChangeEvent<HTMLInputElement>) => {
    setRowsPerPage(parseInt(event.target.value, 10));
    setPage(0);
  };

  const filteredAndSortedRows = useMemo(() => {
    let filtered = rows;

    // Apply search filter
    if (searchText) {
      filtered = rows.filter((row) =>
        columns.some((column) => {
          const value = row[column.id];
          if (value == null) return false;
          return value.toString().toLowerCase().includes(searchText.toLowerCase());
        })
      );
    }

    // Apply sorting
    if (orderBy) {
      filtered = filtered.slice().sort((a, b) => {
        const aValue = a[orderBy];
        const bValue = b[orderBy];

        if (aValue == null && bValue == null) return 0;
        if (aValue == null) return 1;
        if (bValue == null) return -1;

        if (typeof aValue === 'number' && typeof bValue === 'number') {
          return order === 'asc' ? aValue - bValue : bValue - aValue;
        }

        const aString = aValue.toString();
        const bString = bValue.toString();
        return order === 'asc'
          ? aString.localeCompare(bString)
          : bString.localeCompare(aString);
      });
    }

    return filtered;
  }, [rows, searchText, order, orderBy, columns]);

  const paginatedRows = filteredAndSortedRows.slice(
    page * rowsPerPage,
    page * rowsPerPage + rowsPerPage
  );

  const formatCellValue = (column: DataTableColumn, value: any) => {
    if (value == null) return '-';
    
    if (column.format) {
      return column.format(value);
    }

    if (column.type === 'status') {
      return (
        <Chip
          label={value}
          size="small"
          color={
            value.toLowerCase() === 'completed' ? 'success' :
            value.toLowerCase() === 'running' ? 'primary' :
            value.toLowerCase() === 'failed' ? 'error' : 'default'
          }
          variant="outlined"
        />
      );
    }

    if (column.type === 'number' && typeof value === 'number') {
      return value.toLocaleString(undefined, { maximumFractionDigits: 3 });
    }

    return value.toString();
  };

  return (
    <Paper elevation={2}>
      {(title || searchable || exportable) && (
        <SearchToolbar>
          <Box sx={{ flex: 1 }}>
            {title && (
              <Typography variant="h6" component="div" color="primary">
                {title}
              </Typography>
            )}
          </Box>

          {searchable && (
            <TextField
              size="small"
              placeholder="Search data..."
              value={searchText}
              onChange={(e) => setSearchText(e.target.value)}
              InputProps={{
                startAdornment: <Search sx={{ mr: 1, color: 'text.secondary' }} />,
              }}
              sx={{ mr: 2, minWidth: 250 }}
            />
          )}

          {exportable && (
            <Tooltip title="Export Data">
              <IconButton onClick={onExport} color="primary">
                <GetApp />
              </IconButton>
            </Tooltip>
          )}
        </SearchToolbar>
      )}

      <StyledTableContainer>
        <Table stickyHeader={stickyHeader} size={dense ? 'small' : 'medium'}>
          <TableHead>
            <TableRow>
              {columns.map((column) => (
                <TableCell
                  key={column.id}
                  align={column.align}
                  style={{ minWidth: column.minWidth }}
                  sortDirection={orderBy === column.id ? order : false}
                >
                  {column.sortable !== false ? (
                    <TableSortLabel
                      active={orderBy === column.id}
                      direction={orderBy === column.id ? order : 'asc'}
                      onClick={() => handleRequestSort(column.id)}
                    >
                      {column.label}
                    </TableSortLabel>
                  ) : (
                    column.label
                  )}
                </TableCell>
              ))}
            </TableRow>
          </TableHead>
          <TableBody>
            {loading ? (
              <TableRow>
                <TableCell colSpan={columns.length} align="center">
                  <Typography color="text.secondary">Loading analysis data...</Typography>
                </TableCell>
              </TableRow>
            ) : paginatedRows.length === 0 ? (
              <TableRow>
                <TableCell colSpan={columns.length} align="center">
                  <Typography color="text.secondary">
                    {searchText ? 'No matching results found' : 'No data available'}
                  </Typography>
                </TableCell>
              </TableRow>
            ) : (
              paginatedRows.map((row, index) => (
                <TableRow
                  hover
                  key={row.id || index}
                  onClick={() => onRowClick && onRowClick(row)}
                  sx={{ cursor: onRowClick ? 'pointer' : 'default' }}
                >
                  {columns.map((column) => (
                    <TableCell key={column.id} align={column.align}>
                      {formatCellValue(column, row[column.id])}
                    </TableCell>
                  ))}
                </TableRow>
              ))
            )}
          </TableBody>
        </Table>
      </StyledTableContainer>

      <TablePagination
        rowsPerPageOptions={[5, 10, 25, 50, 100]}
        component="div"
        count={filteredAndSortedRows.length}
        rowsPerPage={rowsPerPage}
        page={page}
        onPageChange={handleChangePage}
        onRowsPerPageChange={handleChangeRowsPerPage}
        labelDisplayedRows={({ from, to, count }) =>
          `${from}–${to} of ${count !== -1 ? count : `more than ${to}`} analyses`
        }
      />
    </Paper>
  );
};

export default DataTable;