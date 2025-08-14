import React, { useState, useMemo } from 'react';
import {
  Box,
  VStack,
  HStack,
  Text,
  Badge,
  Button,
  IconButton,
  Table,
  Thead,
  Tbody,
  Tr,
  Th,
  Td,
  Input,
  Select,
  Checkbox,
  Tooltip,
  Tag,
  TagLabel,
  TagCloseButton,
  Wrap,
  WrapItem,
  useColorModeValue,
  Divider,
  Progress,
  Stat,
  StatLabel,
  StatNumber,
  StatHelpText,
  SimpleGrid,
} from '@chakra-ui/react';
import {
  SearchIcon,
  DownloadIcon,
  ViewIcon,
  InfoIcon,
  FilterIcon,
} from '@chakra-ui/icons';

interface FileInfo {
  path: string;
  name: string;
  category: string;
  component?: string;
  size: number;
  modified: string;
  parameters: any;
  match_score: number;
}

interface FileSearchPanelProps {
  searchResults: {
    total_files: number;
    files: FileInfo[];
    files_by_category: Record<string, FileInfo[]>;
    available: boolean;
    search_time?: number;
  };
  onFileSelect?: (file: FileInfo) => void;
  onBulkSelect?: (files: FileInfo[]) => void;
  currentPattern: string;
}

const FileSearchPanel: React.FC<FileSearchPanelProps> = ({
  searchResults,
  onFileSelect,
  onBulkSelect,
  currentPattern
}) => {
  const [selectedFiles, setSelectedFiles] = useState<Set<string>>(new Set());
  const [filterCategory, setFilterCategory] = useState<string>('all');
  const [filterComponent, setFilterComponent] = useState<string>('');
  const [sortBy, setSortBy] = useState<string>('match_score');
  const [searchFilter, setSearchFilter] = useState<string>('');

  const bgColor = useColorModeValue('gray.50', 'gray.800');
  const borderColor = useColorModeValue('gray.200', 'gray.600');

  // Get unique categories and components
  const categories = useMemo(() => {
    const cats = new Set<string>();
    Object.keys(searchResults.files_by_category || {}).forEach(cat => cats.add(cat));
    return Array.from(cats);
  }, [searchResults]);

  const components = useMemo(() => {
    const comps = new Set<string>();
    searchResults.files.forEach(file => {
      if (file.component) comps.add(file.component);
    });
    return Array.from(comps).sort();
  }, [searchResults.files]);

  // Filter and sort files
  const displayFiles = useMemo(() => {
    let files = [...searchResults.files];

    // Apply category filter
    if (filterCategory !== 'all') {
      files = files.filter(f => f.category === filterCategory);
    }

    // Apply component filter
    if (filterComponent) {
      files = files.filter(f => f.component === filterComponent);
    }

    // Apply search filter
    if (searchFilter) {
      const search = searchFilter.toLowerCase();
      files = files.filter(f => 
        f.name.toLowerCase().includes(search) ||
        f.path.toLowerCase().includes(search)
      );
    }

    // Sort files
    files.sort((a, b) => {
      switch (sortBy) {
        case 'match_score':
          return b.match_score - a.match_score;
        case 'name':
          return a.name.localeCompare(b.name);
        case 'size':
          return b.size - a.size;
        case 'modified':
          return new Date(b.modified).getTime() - new Date(a.modified).getTime();
        default:
          return 0;
      }
    });

    return files;
  }, [searchResults.files, filterCategory, filterComponent, searchFilter, sortBy]);

  // Handle file selection
  const handleFileSelect = (file: FileInfo) => {
    const newSelected = new Set(selectedFiles);
    if (newSelected.has(file.path)) {
      newSelected.delete(file.path);
    } else {
      newSelected.add(file.path);
    }
    setSelectedFiles(newSelected);
  };

  // Handle select all
  const handleSelectAll = () => {
    if (selectedFiles.size === displayFiles.length) {
      setSelectedFiles(new Set());
    } else {
      setSelectedFiles(new Set(displayFiles.map(f => f.path)));
    }
  };

  // Handle bulk actions
  const handleBulkAction = () => {
    if (onBulkSelect) {
      const selected = displayFiles.filter(f => selectedFiles.has(f.path));
      onBulkSelect(selected);
    }
  };

  // Format file size
  const formatSize = (bytes: number): string => {
    if (bytes < 1024) return `${bytes} B`;
    if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
    return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
  };

  // Get category color
  const getCategoryColor = (category: string): string => {
    const colors: Record<string, string> = {
      summary: 'blue',
      time_series: 'green',
      strut: 'orange',
      jacket: 'purple',
      mooring: 'cyan',
      fst_motion: 'pink',
      configuration: 'gray',
      unknown: 'gray'
    };
    return colors[category] || 'gray';
  };

  return (
    <VStack spacing={4} align="stretch">
      {/* Search Statistics */}
      <SimpleGrid columns={{ base: 2, md: 4 }} spacing={4}>
        <Stat size="sm">
          <StatLabel>Total Files</StatLabel>
          <StatNumber>{searchResults.total_files}</StatNumber>
          <StatHelpText>
            {searchResults.search_time && `${searchResults.search_time.toFixed(2)}s`}
          </StatHelpText>
        </Stat>
        <Stat size="sm">
          <StatLabel>Categories</StatLabel>
          <StatNumber>{categories.length}</StatNumber>
          <StatHelpText>Types found</StatHelpText>
        </Stat>
        <Stat size="sm">
          <StatLabel>Components</StatLabel>
          <StatNumber>{components.length}</StatNumber>
          <StatHelpText>Unique</StatHelpText>
        </Stat>
        <Stat size="sm">
          <StatLabel>Selected</StatLabel>
          <StatNumber>{selectedFiles.size}</StatNumber>
          <StatHelpText>Files</StatHelpText>
        </Stat>
      </SimpleGrid>

      <Divider />

      {/* Current Pattern Display */}
      <Box p={3} bg={bgColor} borderRadius="md" borderWidth="1px" borderColor={borderColor}>
        <HStack justify="space-between">
          <VStack align="start" spacing={0}>
            <Text fontSize="xs" color="gray.500">Search Pattern</Text>
            <Text fontFamily="monospace" fontWeight="bold">{currentPattern}</Text>
          </VStack>
          <Badge colorScheme={searchResults.available ? 'green' : 'red'} fontSize="sm">
            {searchResults.available ? 'Files Found' : 'No Matches'}
          </Badge>
        </HStack>
      </Box>

      {/* Filters */}
      <HStack spacing={3}>
        <Input
          placeholder="Filter files..."
          value={searchFilter}
          onChange={(e) => setSearchFilter(e.target.value)}
          size="sm"
          leftElement={<SearchIcon />}
        />
        <Select
          value={filterCategory}
          onChange={(e) => setFilterCategory(e.target.value)}
          size="sm"
          minW="150px"
        >
          <option value="all">All Categories</option>
          {categories.map(cat => (
            <option key={cat} value={cat}>{cat}</option>
          ))}
        </Select>
        <Select
          value={filterComponent}
          onChange={(e) => setFilterComponent(e.target.value)}
          size="sm"
          minW="150px"
          placeholder="All Components"
        >
          <option value="">All Components</option>
          {components.map(comp => (
            <option key={comp} value={comp}>{comp}</option>
          ))}
        </Select>
        <Select
          value={sortBy}
          onChange={(e) => setSortBy(e.target.value)}
          size="sm"
          minW="120px"
        >
          <option value="match_score">Relevance</option>
          <option value="name">Name</option>
          <option value="size">Size</option>
          <option value="modified">Modified</option>
        </Select>
      </HStack>

      {/* Category Tags */}
      {categories.length > 0 && (
        <Wrap spacing={2}>
          {Object.entries(searchResults.files_by_category || {}).map(([cat, files]) => (
            <WrapItem key={cat}>
              <Tag
                size="md"
                colorScheme={getCategoryColor(cat)}
                cursor="pointer"
                onClick={() => setFilterCategory(cat)}
                variant={filterCategory === cat ? 'solid' : 'outline'}
              >
                <TagLabel>{cat}</TagLabel>
                <Badge ml={2} variant="solid" fontSize="xs">
                  {files.length}
                </Badge>
              </Tag>
            </WrapItem>
          ))}
        </Wrap>
      )}

      {/* File Table */}
      <Box overflowX="auto" borderWidth="1px" borderRadius="md" borderColor={borderColor}>
        <Table size="sm">
          <Thead bg={bgColor}>
            <Tr>
              <Th width="40px">
                <Checkbox
                  isChecked={selectedFiles.size === displayFiles.length && displayFiles.length > 0}
                  isIndeterminate={selectedFiles.size > 0 && selectedFiles.size < displayFiles.length}
                  onChange={handleSelectAll}
                />
              </Th>
              <Th>File Name</Th>
              <Th>Category</Th>
              <Th>Component</Th>
              <Th isNumeric>Size</Th>
              <Th>Modified</Th>
              <Th isNumeric>Score</Th>
              <Th width="100px">Actions</Th>
            </Tr>
          </Thead>
          <Tbody>
            {displayFiles.map((file) => (
              <Tr key={file.path} _hover={{ bg: bgColor }}>
                <Td>
                  <Checkbox
                    isChecked={selectedFiles.has(file.path)}
                    onChange={() => handleFileSelect(file)}
                  />
                </Td>
                <Td>
                  <Tooltip label={file.path} placement="top">
                    <Text fontSize="sm" isTruncated maxW="200px">
                      {file.name}
                    </Text>
                  </Tooltip>
                </Td>
                <Td>
                  <Badge colorScheme={getCategoryColor(file.category)} size="sm">
                    {file.category}
                  </Badge>
                </Td>
                <Td>
                  <Text fontSize="sm">{file.component || '-'}</Text>
                </Td>
                <Td isNumeric>
                  <Text fontSize="sm">{formatSize(file.size)}</Text>
                </Td>
                <Td>
                  <Text fontSize="xs" color="gray.500">
                    {new Date(file.modified).toLocaleDateString()}
                  </Text>
                </Td>
                <Td isNumeric>
                  <Progress
                    value={file.match_score * 50}
                    size="xs"
                    colorScheme="green"
                    w="50px"
                  />
                </Td>
                <Td>
                  <HStack spacing={1}>
                    <Tooltip label="View file">
                      <IconButton
                        aria-label="View"
                        icon={<ViewIcon />}
                        size="xs"
                        onClick={() => onFileSelect && onFileSelect(file)}
                      />
                    </Tooltip>
                    <Tooltip label="Download">
                      <IconButton
                        aria-label="Download"
                        icon={<DownloadIcon />}
                        size="xs"
                        onClick={() => {
                          // Implement download logic
                          console.log('Download:', file.path);
                        }}
                      />
                    </Tooltip>
                  </HStack>
                </Td>
              </Tr>
            ))}
          </Tbody>
        </Table>

        {displayFiles.length === 0 && (
          <Box p={8} textAlign="center">
            <Text color="gray.500">No files match the current filters</Text>
          </Box>
        )}
      </Box>

      {/* Bulk Actions */}
      {selectedFiles.size > 0 && (
        <HStack justify="space-between" p={3} bg={bgColor} borderRadius="md">
          <Text fontSize="sm">
            {selectedFiles.size} file{selectedFiles.size !== 1 ? 's' : ''} selected
          </Text>
          <HStack>
            <Button
              size="sm"
              variant="outline"
              onClick={() => setSelectedFiles(new Set())}
            >
              Clear Selection
            </Button>
            {onBulkSelect && (
              <Button
                size="sm"
                colorScheme="blue"
                leftIcon={<DownloadIcon />}
                onClick={handleBulkAction}
              >
                Process Selected
              </Button>
            )}
          </HStack>
        </HStack>
      )}
    </VStack>
  );
};

export default FileSearchPanel;