import React, { useState, useEffect, useCallback } from 'react';
import { 
  Box, 
  FormControl, 
  FormLabel, 
  Input, 
  Select, 
  Switch, 
  VStack, 
  HStack,
  Button,
  Alert,
  AlertIcon,
  Tooltip,
  IconButton,
  Text,
  Badge,
  Divider,
  useToast
} from '@chakra-ui/react';
import { InfoIcon, RefreshIcon, SaveIcon } from '@chakra-ui/icons';

interface ParameterConfig {
  vesselType: string;
  loadingCondition: string;
  tideLevel: string;
  returnPeriod: string;
  waveDirection: string;
  analysisType: string;
  autoMax: boolean;
  customBasename?: string;
}

interface ValidationError {
  field: string;
  message: string;
}

interface ParameterOverrideProps {
  onParametersChange: (config: ParameterConfig) => void;
  onPatternUpdate: (pattern: string) => void;
  availableFiles?: string[];
  maxConfiguration?: ParameterConfig;
}

const ParameterOverride: React.FC<ParameterOverrideProps> = ({
  onParametersChange,
  onPatternUpdate,
  availableFiles = [],
  maxConfiguration
}) => {
  const [config, setConfig] = useState<ParameterConfig>({
    vesselType: 'fsts',
    loadingCondition: 'l015',
    tideLevel: 'hwl',
    returnPeriod: '0100yr',
    waveDirection: '000deg',
    analysisType: '03c',
    autoMax: true,
    customBasename: ''
  });

  const [validationErrors, setValidationErrors] = useState<ValidationError[]>([]);
  const [isModified, setIsModified] = useState(false);
  const toast = useToast();

  // Vessel type options
  const vesselTypes = [
    { value: 'fsts', label: 'FSTS' },
    { value: 'flng', label: 'FLNG' },
    { value: 'lngc', label: 'LNGC' }
  ];

  // Loading condition options
  const loadingConditions = [
    { value: 'l015', label: '15% LNG' },
    { value: 'l050', label: '50% LNG' },
    { value: 'l095', label: '95% LNG' }
  ];

  // Tide level options
  const tideLevels = [
    { value: 'hwl', label: 'High Water Level' },
    { value: 'mwl', label: 'Mean Water Level' },
    { value: 'lwl', label: 'Low Water Level' }
  ];

  // Return period options
  const returnPeriods = [
    { value: '0001yr', label: '1 Year' },
    { value: '0010yr', label: '10 Year' },
    { value: '0100yr', label: '100 Year' }
  ];

  // Wave direction options
  const waveDirections = [
    { value: '000deg', label: '0°' },
    { value: '045deg', label: '45°' },
    { value: '090deg', label: '90°' },
    { value: '135deg', label: '135°' },
    { value: '180deg', label: '180°' },
    { value: '225deg', label: '225°' },
    { value: '270deg', label: '270°' },
    { value: '315deg', label: '315°' }
  ];

  // Analysis type options
  const analysisTypes = [
    { value: '03c', label: 'Configuration 3C' },
    { value: '04a', label: 'Configuration 4A' },
    { value: '05b', label: 'Configuration 5B' }
  ];

  // Generate file pattern based on configuration
  const generatePattern = useCallback((cfg: ParameterConfig): string => {
    if (cfg.customBasename) {
      return cfg.customBasename;
    }
    
    const parts = [
      cfg.vesselType,
      cfg.analysisType,
      cfg.returnPeriod,
      cfg.loadingCondition,
      cfg.tideLevel
    ].filter(Boolean);
    
    return parts.join('_');
  }, []);

  // Validate configuration
  const validateConfig = useCallback((cfg: ParameterConfig): ValidationError[] => {
    const errors: ValidationError[] = [];
    
    if (!cfg.vesselType) {
      errors.push({ field: 'vesselType', message: 'Vessel type is required' });
    }
    
    if (!cfg.autoMax && !cfg.customBasename) {
      if (!cfg.loadingCondition) {
        errors.push({ field: 'loadingCondition', message: 'Loading condition is required' });
      }
      if (!cfg.tideLevel) {
        errors.push({ field: 'tideLevel', message: 'Tide level is required' });
      }
    }
    
    if (cfg.customBasename && cfg.customBasename.length < 3) {
      errors.push({ field: 'customBasename', message: 'Custom basename must be at least 3 characters' });
    }
    
    return errors;
  }, []);

  // Handle parameter change
  const handleParameterChange = useCallback((field: keyof ParameterConfig, value: any) => {
    const newConfig = { ...config, [field]: value };
    
    // If switching to manual mode, populate with max configuration if available
    if (field === 'autoMax' && !value && maxConfiguration) {
      newConfig.vesselType = maxConfiguration.vesselType;
      newConfig.loadingCondition = maxConfiguration.loadingCondition;
      newConfig.tideLevel = maxConfiguration.tideLevel;
      newConfig.returnPeriod = maxConfiguration.returnPeriod;
      newConfig.waveDirection = maxConfiguration.waveDirection;
      newConfig.analysisType = maxConfiguration.analysisType;
    }
    
    setConfig(newConfig);
    setIsModified(true);
    
    // Validate
    const errors = validateConfig(newConfig);
    setValidationErrors(errors);
    
    // Update pattern if valid
    if (errors.length === 0) {
      const pattern = generatePattern(newConfig);
      onPatternUpdate(pattern);
      onParametersChange(newConfig);
    }
  }, [config, maxConfiguration, validateConfig, generatePattern, onPatternUpdate, onParametersChange]);

  // Apply configuration
  const applyConfiguration = useCallback(() => {
    const errors = validateConfig(config);
    if (errors.length > 0) {
      toast({
        title: 'Validation Error',
        description: 'Please fix the errors before applying',
        status: 'error',
        duration: 3000,
        isClosable: true
      });
      return;
    }
    
    onParametersChange(config);
    setIsModified(false);
    
    toast({
      title: 'Configuration Applied',
      description: 'Parameters have been updated successfully',
      status: 'success',
      duration: 2000,
      isClosable: true
    });
  }, [config, validateConfig, onParametersChange, toast]);

  // Reset to defaults
  const resetToDefaults = useCallback(() => {
    const defaultConfig: ParameterConfig = {
      vesselType: 'fsts',
      loadingCondition: 'l015',
      tideLevel: 'hwl',
      returnPeriod: '0100yr',
      waveDirection: '000deg',
      analysisType: '03c',
      autoMax: true,
      customBasename: ''
    };
    
    setConfig(defaultConfig);
    setValidationErrors([]);
    setIsModified(false);
    onParametersChange(defaultConfig);
    
    toast({
      title: 'Reset Complete',
      description: 'Parameters reset to defaults',
      status: 'info',
      duration: 2000,
      isClosable: true
    });
  }, [onParametersChange, toast]);

  // Update when max configuration changes
  useEffect(() => {
    if (config.autoMax && maxConfiguration) {
      setConfig(prev => ({
        ...prev,
        ...maxConfiguration,
        autoMax: true
      }));
    }
  }, [maxConfiguration, config.autoMax]);

  // Get validation error for field
  const getFieldError = (field: string): string | undefined => {
    const error = validationErrors.find(e => e.field === field);
    return error?.message;
  };

  return (
    <Box p={4} borderWidth="1px" borderRadius="lg" bg="white">
      <VStack spacing={4} align="stretch">
        <HStack justify="space-between">
          <Text fontSize="lg" fontWeight="bold">Parameter Override</Text>
          <HStack>
            <Badge colorScheme={config.autoMax ? 'green' : 'blue'}>
              {config.autoMax ? 'Auto-Max Mode' : 'Manual Mode'}
            </Badge>
            {availableFiles.length > 0 && (
              <Badge colorScheme="purple">{availableFiles.length} files</Badge>
            )}
          </HStack>
        </HStack>

        <Divider />

        {/* Auto-Max Toggle */}
        <FormControl display="flex" alignItems="center">
          <FormLabel htmlFor="auto-max" mb="0">
            Use Maximum Configuration
          </FormLabel>
          <Switch
            id="auto-max"
            isChecked={config.autoMax}
            onChange={(e) => handleParameterChange('autoMax', e.target.checked)}
          />
          <Tooltip label="Automatically use configuration with maximum strut forces">
            <IconButton
              aria-label="Info"
              icon={<InfoIcon />}
              size="sm"
              variant="ghost"
              ml={2}
            />
          </Tooltip>
        </FormControl>

        {/* Show current max configuration when in auto mode */}
        {config.autoMax && maxConfiguration && (
          <Alert status="info" borderRadius="md">
            <AlertIcon />
            <Box>
              <Text fontWeight="bold">Current Maximum Configuration:</Text>
              <Text fontSize="sm">
                {generatePattern(maxConfiguration)} (Auto-detected)
              </Text>
            </Box>
          </Alert>
        )}

        {/* Manual parameter controls */}
        {!config.autoMax && (
          <>
            <VStack spacing={3} align="stretch">
              <HStack spacing={3}>
                <FormControl isInvalid={!!getFieldError('vesselType')}>
                  <FormLabel>Vessel Type</FormLabel>
                  <Select
                    value={config.vesselType}
                    onChange={(e) => handleParameterChange('vesselType', e.target.value)}
                  >
                    {vesselTypes.map(opt => (
                      <option key={opt.value} value={opt.value}>{opt.label}</option>
                    ))}
                  </Select>
                </FormControl>

                <FormControl isInvalid={!!getFieldError('analysisType')}>
                  <FormLabel>Analysis Type</FormLabel>
                  <Select
                    value={config.analysisType}
                    onChange={(e) => handleParameterChange('analysisType', e.target.value)}
                  >
                    {analysisTypes.map(opt => (
                      <option key={opt.value} value={opt.value}>{opt.label}</option>
                    ))}
                  </Select>
                </FormControl>
              </HStack>

              <HStack spacing={3}>
                <FormControl isInvalid={!!getFieldError('loadingCondition')}>
                  <FormLabel>Loading Condition</FormLabel>
                  <Select
                    value={config.loadingCondition}
                    onChange={(e) => handleParameterChange('loadingCondition', e.target.value)}
                  >
                    {loadingConditions.map(opt => (
                      <option key={opt.value} value={opt.value}>{opt.label}</option>
                    ))}
                  </Select>
                </FormControl>

                <FormControl isInvalid={!!getFieldError('tideLevel')}>
                  <FormLabel>Tide Level</FormLabel>
                  <Select
                    value={config.tideLevel}
                    onChange={(e) => handleParameterChange('tideLevel', e.target.value)}
                  >
                    {tideLevels.map(opt => (
                      <option key={opt.value} value={opt.value}>{opt.label}</option>
                    ))}
                  </Select>
                </FormControl>
              </HStack>

              <HStack spacing={3}>
                <FormControl isInvalid={!!getFieldError('returnPeriod')}>
                  <FormLabel>Return Period</FormLabel>
                  <Select
                    value={config.returnPeriod}
                    onChange={(e) => handleParameterChange('returnPeriod', e.target.value)}
                  >
                    {returnPeriods.map(opt => (
                      <option key={opt.value} value={opt.value}>{opt.label}</option>
                    ))}
                  </Select>
                </FormControl>

                <FormControl isInvalid={!!getFieldError('waveDirection')}>
                  <FormLabel>Wave Direction</FormLabel>
                  <Select
                    value={config.waveDirection}
                    onChange={(e) => handleParameterChange('waveDirection', e.target.value)}
                  >
                    {waveDirections.map(opt => (
                      <option key={opt.value} value={opt.value}>{opt.label}</option>
                    ))}
                  </Select>
                </FormControl>
              </HStack>

              <FormControl isInvalid={!!getFieldError('customBasename')}>
                <FormLabel>Custom Basename (Optional)</FormLabel>
                <Input
                  placeholder="Enter custom file pattern..."
                  value={config.customBasename}
                  onChange={(e) => handleParameterChange('customBasename', e.target.value)}
                />
                <Text fontSize="xs" color="gray.500" mt={1}>
                  Override automatic pattern generation with custom basename
                </Text>
              </FormControl>
            </VStack>

            {/* Validation errors */}
            {validationErrors.length > 0 && (
              <Alert status="error" borderRadius="md">
                <AlertIcon />
                <VStack align="start" spacing={1}>
                  {validationErrors.map((error, idx) => (
                    <Text key={idx} fontSize="sm">{error.message}</Text>
                  ))}
                </VStack>
              </Alert>
            )}

            {/* Generated pattern preview */}
            <Box p={3} bg="gray.50" borderRadius="md">
              <Text fontSize="sm" fontWeight="bold">Generated Pattern:</Text>
              <Text fontSize="md" fontFamily="monospace">
                {generatePattern(config)}
              </Text>
            </Box>

            {/* Action buttons */}
            <HStack justify="flex-end" spacing={2}>
              <Button
                leftIcon={<RefreshIcon />}
                variant="outline"
                onClick={resetToDefaults}
              >
                Reset
              </Button>
              <Button
                leftIcon={<SaveIcon />}
                colorScheme="blue"
                onClick={applyConfiguration}
                isDisabled={validationErrors.length > 0 || !isModified}
              >
                Apply
              </Button>
            </HStack>
          </>
        )}
      </VStack>
    </Box>
  );
};

export default ParameterOverride;