import React, { useState, useEffect, useCallback, useMemo } from 'react';
import {
  Box,
  Container,
  Grid,
  GridItem,
  VStack,
  HStack,
  Text,
  Button,
  IconButton,
  Drawer,
  DrawerBody,
  DrawerFooter,
  DrawerHeader,
  DrawerOverlay,
  DrawerContent,
  DrawerCloseButton,
  useDisclosure,
  useToast,
  Tabs,
  TabList,
  TabPanels,
  Tab,
  TabPanel,
  Badge,
  Divider,
  Tooltip,
  Collapse,
  useBreakpointValue,
  Spinner,
  Alert,
  AlertIcon,
  Menu,
  MenuButton,
  MenuList,
  MenuItem,
  MenuDivider,
  useColorMode,
  useColorModeValue,
} from '@chakra-ui/react';
import {
  SettingsIcon,
  InfoOutlineIcon,
  DownloadIcon,
  UploadIcon,
  StarIcon,
  RepeatIcon,
  ChevronDownIcon,
  QuestionIcon,
  MoonIcon,
  SunIcon,
  ViewIcon,
  SearchIcon,
  SaveIcon,
} from '@chakra-ui/icons';

// Import custom components
import ParameterOverride from './ParameterOverride';
import FileSearchPanel from './FileSearchPanel';
import ConfigurationPanel from './ConfigurationPanel';
import HelpPanel from './HelpPanel';

// Import services
import { ConfigurationService } from '../services/configurationService';
import { FileSearchService } from '../services/fileSearchService';
import { ValidationService } from '../services/validationService';

interface BrowserDashboardProps {
  onDataLoad?: (data: any) => void;
  onConfigurationChange?: (config: any) => void;
  userId?: string;
  baseApiUrl?: string;
}

const BrowserDashboard: React.FC<BrowserDashboardProps> = ({
  onDataLoad,
  onConfigurationChange,
  userId = 'default_user',
  baseApiUrl = '/api'
}) => {
  // Services
  const configService = useMemo(() => new ConfigurationService(baseApiUrl), [baseApiUrl]);
  const searchService = useMemo(() => new FileSearchService(baseApiUrl), [baseApiUrl]);
  const validationService = useMemo(() => new ValidationService(), []);

  // State management
  const [currentConfig, setCurrentConfig] = useState<any>({
    vessel_type: 'fsts',
    loading_condition: 'l015',
    tide_level: 'hwl',
    return_period: '0100yr',
    wave_direction: '000deg',
    analysis_type: '03c',
    auto_max: true
  });

  const [searchResults, setSearchResults] = useState<any>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [savedConfigs, setSavedConfigs] = useState<any[]>([]);
  const [templates, setTemplates] = useState<any[]>([]);
  const [maxConfiguration, setMaxConfiguration] = useState<any>(null);
  const [validationReport, setValidationReport] = useState<any>(null);
  const [userPreferences, setUserPreferences] = useState<any>(null);

  // UI state
  const { isOpen: isConfigOpen, onOpen: onConfigOpen, onClose: onConfigClose } = useDisclosure();
  const { isOpen: isHelpOpen, onOpen: onHelpOpen, onClose: onHelpClose } = useDisclosure();
  const [isParametersExpanded, setIsParametersExpanded] = useState(true);
  const [isSearchExpanded, setIsSearchExpanded] = useState(true);
  const [activeTab, setActiveTab] = useState(0);

  // Responsive
  const isMobile = useBreakpointValue({ base: true, md: false });
  const gridColumns = useBreakpointValue({ base: 1, md: 2, lg: 3 });
  
  // Theme
  const { colorMode, toggleColorMode } = useColorMode();
  const bgColor = useColorModeValue('gray.50', 'gray.900');
  const cardBg = useColorModeValue('white', 'gray.800');
  const borderColor = useColorModeValue('gray.200', 'gray.700');

  // Toast notifications
  const toast = useToast();

  // Load initial data
  useEffect(() => {
    loadUserPreferences();
    loadSavedConfigurations();
    loadTemplates();
    findMaxConfiguration();
  }, [userId]);

  // Auto-save effect
  useEffect(() => {
    if (userPreferences?.auto_save_enabled) {
      const timer = setTimeout(() => {
        autoSaveConfiguration();
      }, userPreferences.auto_save_interval * 1000);
      return () => clearTimeout(timer);
    }
  }, [currentConfig, userPreferences]);

  // Load user preferences
  const loadUserPreferences = async () => {
    try {
      const prefs = await configService.getUserPreferences(userId);
      setUserPreferences(prefs);
      
      // Apply default preferences
      if (prefs && !currentConfig.auto_max) {
        setCurrentConfig(prev => ({
          ...prev,
          vessel_type: prefs.default_vessel_type || prev.vessel_type,
          loading_condition: prefs.default_loading_condition || prev.loading_condition,
          tide_level: prefs.default_tide_level || prev.tide_level,
          return_period: prefs.default_return_period || prev.return_period
        }));
      }
    } catch (error) {
      console.error('Error loading preferences:', error);
    }
  };

  // Load saved configurations
  const loadSavedConfigurations = async () => {
    try {
      const configs = await configService.listConfigurations(userId);
      setSavedConfigs(configs);
    } catch (error) {
      console.error('Error loading configurations:', error);
    }
  };

  // Load templates
  const loadTemplates = async () => {
    try {
      const templates = await configService.getTemplates();
      setTemplates(templates);
    } catch (error) {
      console.error('Error loading templates:', error);
    }
  };

  // Find maximum configuration
  const findMaxConfiguration = async () => {
    try {
      setIsLoading(true);
      const maxConfig = await searchService.findMaximumConfiguration();
      setMaxConfiguration(maxConfig);
      
      if (currentConfig.auto_max && maxConfig) {
        setCurrentConfig(prev => ({
          ...prev,
          ...maxConfig.configuration
        }));
      }
    } catch (error) {
      console.error('Error finding max configuration:', error);
    } finally {
      setIsLoading(false);
    }
  };

  // Handle parameter change
  const handleParameterChange = useCallback(async (newConfig: any) => {
    // Validate configuration
    const validation = validationService.validate(newConfig);
    setValidationReport(validation);
    
    if (validation.is_valid) {
      setCurrentConfig(newConfig);
      
      // Trigger file search
      const pattern = searchService.generatePattern(newConfig);
      performFileSearch(pattern);
      
      // Notify parent
      if (onConfigurationChange) {
        onConfigurationChange(newConfig);
      }
    } else {
      toast({
        title: 'Validation Error',
        description: validation.errors[0]?.message || 'Invalid configuration',
        status: 'error',
        duration: 3000,
        isClosable: true
      });
    }
  }, [validationService, searchService, onConfigurationChange, toast]);

  // Perform file search
  const performFileSearch = async (pattern: string) => {
    try {
      setIsLoading(true);
      const results = await searchService.searchFiles(pattern);
      setSearchResults(results);
      
      // Load data if callback provided
      if (onDataLoad && results.files.length > 0) {
        onDataLoad(results.files);
      }
    } catch (error) {
      console.error('Error searching files:', error);
      toast({
        title: 'Search Error',
        description: 'Failed to search files',
        status: 'error',
        duration: 3000,
        isClosable: true
      });
    } finally {
      setIsLoading(false);
    }
  };

  // Save configuration
  const saveConfiguration = async (name: string, description?: string) => {
    try {
      const saved = await configService.saveConfiguration({
        name,
        description,
        parameters: currentConfig,
        user_id: userId
      });
      
      toast({
        title: 'Configuration Saved',
        description: `Saved as "${name}"`,
        status: 'success',
        duration: 2000,
        isClosable: true
      });
      
      loadSavedConfigurations();
      return saved;
    } catch (error) {
      console.error('Error saving configuration:', error);
      toast({
        title: 'Save Error',
        description: 'Failed to save configuration',
        status: 'error',
        duration: 3000,
        isClosable: true
      });
    }
  };

  // Auto-save configuration
  const autoSaveConfiguration = async () => {
    try {
      await configService.autoSave(currentConfig, userId);
      console.log('Auto-saved configuration');
    } catch (error) {
      console.error('Error auto-saving:', error);
    }
  };

  // Load configuration
  const loadConfiguration = async (configId: string) => {
    try {
      const config = await configService.loadConfiguration(configId);
      if (config) {
        setCurrentConfig(config.parameters);
        toast({
          title: 'Configuration Loaded',
          description: `Loaded "${config.name}"`,
          status: 'success',
          duration: 2000,
          isClosable: true
        });
      }
    } catch (error) {
      console.error('Error loading configuration:', error);
      toast({
        title: 'Load Error',
        description: 'Failed to load configuration',
        status: 'error',
        duration: 3000,
        isClosable: true
      });
    }
  };

  // Export configuration
  const exportConfiguration = async () => {
    try {
      const exported = await configService.exportConfiguration(currentConfig, 'json');
      // Create download
      const blob = new Blob([exported], { type: 'application/json' });
      const url = URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `orcaflex_config_${Date.now()}.json`;
      a.click();
      URL.revokeObjectURL(url);
      
      toast({
        title: 'Configuration Exported',
        status: 'success',
        duration: 2000,
        isClosable: true
      });
    } catch (error) {
      console.error('Error exporting configuration:', error);
    }
  };

  // Import configuration
  const importConfiguration = async (file: File) => {
    try {
      const text = await file.text();
      const imported = await configService.importConfiguration(text, 'json', userId);
      if (imported) {
        setCurrentConfig(imported.parameters);
        toast({
          title: 'Configuration Imported',
          status: 'success',
          duration: 2000,
          isClosable: true
        });
      }
    } catch (error) {
      console.error('Error importing configuration:', error);
      toast({
        title: 'Import Error',
        description: 'Failed to import configuration',
        status: 'error',
        duration: 3000,
        isClosable: true
      });
    }
  };

  // Apply template
  const applyTemplate = async (templateId: string) => {
    try {
      const template = templates.find(t => t.id === templateId);
      if (template) {
        setCurrentConfig(template.parameters);
        toast({
          title: 'Template Applied',
          description: `Applied "${template.name}"`,
          status: 'info',
          duration: 2000,
          isClosable: true
        });
      }
    } catch (error) {
      console.error('Error applying template:', error);
    }
  };

  return (
    <Box minH="100vh" bg={bgColor}>
      <Container maxW="container.xl" py={4}>
        {/* Header */}
        <HStack justify="space-between" mb={6}>
          <VStack align="start" spacing={0}>
            <Text fontSize="2xl" fontWeight="bold">
              OrcaFlex Browser Interface
            </Text>
            <Text fontSize="sm" color="gray.500">
              Manual parameter override and file browsing
            </Text>
          </VStack>
          
          <HStack spacing={2}>
            {/* Configuration Menu */}
            <Menu>
              <MenuButton as={Button} rightIcon={<ChevronDownIcon />} size="sm">
                Configurations
              </MenuButton>
              <MenuList>
                <MenuItem icon={<SaveIcon />} onClick={onConfigOpen}>
                  Save Current
                </MenuItem>
                <MenuItem icon={<UploadIcon />} onClick={() => document.getElementById('file-import')?.click()}>
                  Import
                </MenuItem>
                <MenuItem icon={<DownloadIcon />} onClick={exportConfiguration}>
                  Export
                </MenuItem>
                <MenuDivider />
                <MenuItem fontSize="sm" fontWeight="bold">Recent</MenuItem>
                {savedConfigs.slice(0, 5).map(config => (
                  <MenuItem key={config.id} onClick={() => loadConfiguration(config.id)}>
                    {config.name}
                  </MenuItem>
                ))}
              </MenuList>
            </Menu>
            
            {/* Hidden file input for import */}
            <input
              id="file-import"
              type="file"
              accept=".json"
              style={{ display: 'none' }}
              onChange={(e) => e.target.files?.[0] && importConfiguration(e.target.files[0])}
            />
            
            {/* Help Button */}
            <Tooltip label="Help & Documentation">
              <IconButton
                aria-label="Help"
                icon={<QuestionIcon />}
                size="sm"
                onClick={onHelpOpen}
              />
            </Tooltip>
            
            {/* Theme Toggle */}
            <Tooltip label={`Switch to ${colorMode === 'light' ? 'dark' : 'light'} mode`}>
              <IconButton
                aria-label="Toggle theme"
                icon={colorMode === 'light' ? <MoonIcon /> : <SunIcon />}
                size="sm"
                onClick={toggleColorMode}
              />
            </Tooltip>
          </HStack>
        </HStack>

        {/* Main Content Grid */}
        <Grid templateColumns={`repeat(${gridColumns}, 1fr)`} gap={4}>
          {/* Parameter Override Panel */}
          <GridItem colSpan={isMobile ? 1 : 1}>
            <Box bg={cardBg} borderRadius="lg" borderWidth="1px" borderColor={borderColor}>
              <HStack justify="space-between" p={3}>
                <Text fontWeight="bold">Parameters</Text>
                <IconButton
                  aria-label="Toggle parameters"
                  icon={isParametersExpanded ? <ChevronDownIcon /> : <ViewIcon />}
                  size="xs"
                  variant="ghost"
                  onClick={() => setIsParametersExpanded(!isParametersExpanded)}
                />
              </HStack>
              <Collapse in={isParametersExpanded}>
                <Box p={3} pt={0}>
                  <ParameterOverride
                    onParametersChange={handleParameterChange}
                    onPatternUpdate={(pattern) => performFileSearch(pattern)}
                    availableFiles={searchResults?.files || []}
                    maxConfiguration={maxConfiguration?.configuration}
                  />
                </Box>
              </Collapse>
            </Box>
          </GridItem>

          {/* File Search Results */}
          <GridItem colSpan={isMobile ? 1 : gridColumns - 1}>
            <Box bg={cardBg} borderRadius="lg" borderWidth="1px" borderColor={borderColor}>
              <HStack justify="space-between" p={3}>
                <HStack>
                  <Text fontWeight="bold">File Search</Text>
                  {searchResults && (
                    <Badge colorScheme={searchResults.available ? 'green' : 'red'}>
                      {searchResults.total_files} files
                    </Badge>
                  )}
                </HStack>
                <HStack>
                  <IconButton
                    aria-label="Refresh search"
                    icon={<RepeatIcon />}
                    size="xs"
                    variant="ghost"
                    onClick={() => performFileSearch(searchService.generatePattern(currentConfig))}
                    isLoading={isLoading}
                  />
                  <IconButton
                    aria-label="Toggle search"
                    icon={isSearchExpanded ? <ChevronDownIcon /> : <SearchIcon />}
                    size="xs"
                    variant="ghost"
                    onClick={() => setIsSearchExpanded(!isSearchExpanded)}
                  />
                </HStack>
              </HStack>
              <Collapse in={isSearchExpanded}>
                <Box p={3} pt={0}>
                  {isLoading ? (
                    <VStack py={8}>
                      <Spinner size="lg" />
                      <Text fontSize="sm" color="gray.500">Searching files...</Text>
                    </VStack>
                  ) : searchResults ? (
                    <FileSearchPanel
                      searchResults={searchResults}
                      onFileSelect={(file) => onDataLoad && onDataLoad([file])}
                      currentPattern={searchService.generatePattern(currentConfig)}
                    />
                  ) : (
                    <Alert status="info">
                      <AlertIcon />
                      Configure parameters to search for files
                    </Alert>
                  )}
                </Box>
              </Collapse>
            </Box>
          </GridItem>
        </Grid>

        {/* Templates Section */}
        {templates.length > 0 && (
          <Box mt={4} bg={cardBg} borderRadius="lg" borderWidth="1px" borderColor={borderColor} p={4}>
            <Text fontWeight="bold" mb={3}>Quick Templates</Text>
            <HStack spacing={2} wrap="wrap">
              {templates.map(template => (
                <Button
                  key={template.id}
                  size="sm"
                  variant="outline"
                  onClick={() => applyTemplate(template.id)}
                  leftIcon={<StarIcon />}
                >
                  {template.name}
                </Button>
              ))}
            </HStack>
          </Box>
        )}

        {/* Validation Messages */}
        {validationReport && !validationReport.is_valid && (
          <Alert status="warning" mt={4} borderRadius="md">
            <AlertIcon />
            <VStack align="start" spacing={0}>
              <Text fontWeight="bold">Validation Issues</Text>
              {validationReport.errors.map((error, idx) => (
                <Text key={idx} fontSize="sm">{error.message}</Text>
              ))}
            </VStack>
          </Alert>
        )}
      </Container>

      {/* Configuration Drawer */}
      <Drawer isOpen={isConfigOpen} placement="right" onClose={onConfigClose} size="md">
        <DrawerOverlay />
        <DrawerContent>
          <DrawerCloseButton />
          <DrawerHeader>Manage Configurations</DrawerHeader>
          <DrawerBody>
            <ConfigurationPanel
              currentConfig={currentConfig}
              savedConfigs={savedConfigs}
              onSave={saveConfiguration}
              onLoad={loadConfiguration}
              onDelete={async (id) => {
                await configService.deleteConfiguration(id);
                loadSavedConfigurations();
              }}
            />
          </DrawerBody>
        </DrawerContent>
      </Drawer>

      {/* Help Drawer */}
      <Drawer isOpen={isHelpOpen} placement="right" onClose={onHelpClose} size="lg">
        <DrawerOverlay />
        <DrawerContent>
          <DrawerCloseButton />
          <DrawerHeader>Help & Documentation</DrawerHeader>
          <DrawerBody>
            <HelpPanel />
          </DrawerBody>
        </DrawerContent>
      </Drawer>
    </Box>
  );
};

export default BrowserDashboard;