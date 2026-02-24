# 🧪 Testing Guide for Step-by-Step OrcaFlex Browser

## 🌐 Access the Interface

Open your browser and go to:
### **http://localhost:5002**

## 📋 Step-by-Step Testing Process

### **Step 1: Select Folder** 📁
1. The interface opens with Step 1 highlighted
2. You'll see example paths in a yellow box
3. **Options:**
   - Enter your own OrcaFlex folder path
   - Use the default path: `D:\1522\ctr7\orcaflex\rev_a08\output\csv`
   - Click **"Create Sample Data"** button to generate test files
4. Click **"Select Folder"** button
5. ✅ You should see a green success message
6. The **"Next"** button appears at the bottom

### **Step 2: Scan Files** 🔍
1. Click **"Next"** or click on Step 2 in the progress bar
2. Click **"Start Scanning"** button
3. ✅ The system will:
   - Show how many files were found
   - Display the first 20 files with categories
   - Show statistics cards with file counts by type
4. The **"Next"** button appears when scanning is complete

### **Step 3: Find Maximum** ⚡
1. Click **"Next"** to go to Step 3
2. Click **"Find Maximum"** button
3. ✅ The system will:
   - Analyze summary files for maximum forces
   - Display the configuration with highest forces
   - Show the configuration in JSON format
4. The **"Next"** button appears when complete

### **Step 4: Search & Filter** 🎯
1. Click **"Next"** to go to Step 4
2. **Search Options:**
   - Type a search term (e.g., "fsts", "l095", "strut")
   - Or use quick search buttons:
     - **Search Struts** - finds all strut files
     - **Search Jackets** - finds all jacket files
     - **Search Moorings** - finds all mooring files
     - **Search Summaries** - finds all summary files
3. ✅ Results show matching files with categories

## 🎨 Interface Features

### **Visual Indicators:**
- **Gray circles** = Steps not reached yet
- **Purple circle** = Current active step
- **Green circles** = Completed steps

### **Status Messages:**
- 🟦 **Blue** = Information
- 🟩 **Green** = Success
- 🟥 **Red** = Error
- 🟨 **Yellow** = Tips/Examples

### **Navigation:**
- Click **"Previous"** to go back
- Click **"Next"** to proceed (only when step is complete)
- Click directly on step numbers to jump (if already completed)

## 🔧 Troubleshooting

### **If no files are found:**
1. Click **"Previous"** to go back to Step 1
2. Click **"Create Sample Data"**
3. This creates test files automatically
4. Continue with Step 2

### **If folder doesn't exist:**
- The system will tell you the folder wasn't found
- Try the sample data option instead

### **If maximum force shows 0 or N/A:**
- This means no summary files were found
- The system uses default configuration
- Try creating sample data for better results

## 📊 What Each Step Does

1. **Step 1**: Validates the folder exists and contains the right structure
2. **Step 2**: Scans for CSV files and categorizes them (summary, strut, jacket, etc.)
3. **Step 3**: Reads summary files to find maximum strut forces
4. **Step 4**: Allows pattern-based searching through all found files

## 🚀 Quick Test

For a quick test without real OrcaFlex data:
1. Open http://localhost:5002
2. Click **"Create Sample Data"** in Step 1
3. Click through each step using the **"Next"** button
4. Try different search patterns in Step 4

The interface guides you through each step with clear instructions and visual feedback!