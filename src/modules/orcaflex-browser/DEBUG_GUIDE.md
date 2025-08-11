# OrcaFlex Dashboard Debugging Guide

## Quick Access URLs
- Main Dashboard: http://localhost:5000/
- Debug Dashboard: http://localhost:5000/debug
- Previous Version: http://localhost:5000/v5

## 1. Browser Console Debugging

### Enable Verbose Logging
Add this to browser console to enable detailed logging:
```javascript
// Enable verbose logging
window.DEBUG_MODE = true;

// Test API connectivity
fetch('http://localhost:5000/api/test')
  .then(r => r.json())
  .then(console.log);

// Check current configuration
console.log({
  vessleType: currentVesselType,
  isLoading: isLoading,
  subfolder: document.getElementById('subfolderSelect').value
});

// Manually trigger max strut force scan
fetch('http://localhost:5000/api/max_strut_force?subfolder=02c_005yr')
  .then(r => r.json())
  .then(console.log);
```

### Monitor Data Flow
```javascript
// Add breakpoints in these functions:
loadData()  // Main data loading
loadDataManual()  // Manual configuration
plotData()  // Chart creation
setBusyState()  // Loading state management
```

## 2. Server-Side Debugging

### Enable Flask Debug Mode
```python
# In orcaflex_data_server.py, change:
app.run(debug=True, port=5000)
# This enables auto-reload and detailed error messages
```

### Add Debug Logging
```python
# Add to orcaflex_data_server.py:
import logging
logging.basicConfig(level=logging.DEBUG)

# In any function:
app.logger.debug(f"Processing {len(strut_files)} files")
app.logger.info(f"Max force found: {max_force}")
app.logger.error(f"Error: {e}")
```

### Monitor Server Output
```bash
# Watch server console for:
- "Processing X files in parallel..."
- "Using X parallel workers"
- "Parallel processing completed in X seconds"
- HTTP request logs with status codes
```

## 3. Common Issues and Solutions

### Issue: No Data Displayed
```javascript
// Check in browser console:
1. Network tab → Look for red (failed) API calls
2. Console tab → Look for JavaScript errors
3. Check if CSV files exist in folder
4. Verify file naming conventions (must contain 'strut' or 'jacket')
```

### Issue: Slow Performance
```python
# Check parallel processing:
1. Server console should show "Using 20 parallel workers"
2. Processing time should be displayed
3. Check CPU usage during processing
4. Verify files aren't too large (>100MB each)
```

### Issue: Button Not Working
```javascript
// In browser console:
isLoading  // Should be false
document.querySelectorAll('button:disabled')  // Should be empty array
setBusyState(false)  // Force reset busy state
```

### Issue: Charts Not Appearing
```javascript
// Debug chart creation:
1. Check if data has correct structure:
   console.log(data.time)  // Should be array
   console.log(data.jacket)  // Should be object with arrays
   console.log(data.struts)  // Should be object with arrays

2. Check Plotly is loaded:
   typeof Plotly  // Should be "object"

3. Manually create test chart:
   Plotly.newPlot('charts', [{x: [1,2,3], y: [1,2,3]}])
```

## 4. Test Different Scenarios

### Test Minimal Data
```bash
# Create test folder with single small CSV:
mkdir D:\1522\ctr7\orcaflex\rev_a08\output\csv\test_folder
# Add one strut CSV file with simple data
```

### Test Maximum Load
```bash
# Test with folder containing many files:
# The parallel processing should handle it efficiently
```

### Test Error Conditions
```javascript
// Test with invalid folder:
fetch('http://localhost:5000/api/max_strut_force?subfolder=invalid_folder')
  .then(r => r.json())
  .then(console.log);  // Should return error

// Test with empty folder:
fetch('http://localhost:5000/api/max_strut_force?subfolder=empty_folder')
  .then(r => r.json())
  .then(console.log);  // Should return "No strut files found"
```

## 5. Performance Monitoring

### Client-Side Performance
```javascript
// In browser console:
performance.mark('loadStart');
await loadData();
performance.mark('loadEnd');
performance.measure('loadTime', 'loadStart', 'loadEnd');
console.log(performance.getEntriesByName('loadTime'));
```

### Server-Side Performance
```python
# Already implemented - check response for:
"processing_time": "X.XX seconds"
```

## 6. Advanced Debugging

### Enable Chrome DevTools Protocol
```bash
# Start Chrome with debugging:
chrome.exe --remote-debugging-port=9222

# Connect to debug session
# Navigate to chrome://inspect
```

### Use Python Debugger
```python
# Add breakpoints in server code:
import pdb
pdb.set_trace()  # Execution will pause here

# Or use VS Code debugger with launch.json:
{
    "type": "python",
    "request": "launch",
    "name": "Debug Flask",
    "module": "flask",
    "env": {"FLASK_APP": "orcaflex_data_server.py"},
    "args": ["run", "--debug"]
}
```

### Network Analysis
```bash
# Monitor network traffic:
# Windows: Use Fiddler or Wireshark
# Browser: Network tab in DevTools
# Look for:
- Request/response times
- Payload sizes
- Error responses
- CORS issues
```

## 7. Logging to File

### Add File Logging
```python
# In orcaflex_data_server.py:
import logging
from logging.handlers import RotatingFileHandler

file_handler = RotatingFileHandler('orcaflex_debug.log', maxBytes=10240, backupCount=10)
file_handler.setFormatter(logging.Formatter(
    '%(asctime)s %(levelname)s: %(message)s'
))
app.logger.addHandler(file_handler)
app.logger.setLevel(logging.INFO)
```

### View Logs
```bash
# Watch log file in real-time:
tail -f orcaflex_debug.log

# Search for errors:
grep ERROR orcaflex_debug.log

# View processing times:
grep "processing completed" orcaflex_debug.log
```

## 8. Quick Debug Checklist

1. ✅ Server running? Check http://localhost:5000/api/test
2. ✅ CSV files exist? Check folder path
3. ✅ File naming correct? Must contain 'strut' or 'jacket'
4. ✅ Browser console errors? Press F12
5. ✅ Network requests failing? Check Network tab
6. ✅ Busy state stuck? Run `setBusyState(false)` in console
7. ✅ Charts not showing? Check data structure in console
8. ✅ Parallel processing working? Check server console for worker count

## 9. Emergency Recovery

If the dashboard becomes unresponsive:

```javascript
// In browser console:
setBusyState(false);  // Clear busy state
isLoading = false;    // Reset loading flag
location.reload();    // Refresh page

// Or just press Ctrl+F5 for hard refresh
```

## 10. Contact for Help

If issues persist:
1. Check server console for Python errors
2. Check browser console for JavaScript errors
3. Save both error messages
4. Check the CSV file format matches expected structure
5. Verify file permissions allow reading

---
*Debug dashboard available at: http://localhost:5000/debug*