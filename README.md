# My_Sap_Codes
Sap Codes of my Personal Programs

# My_Sap_Codes
Sap Codes of my Personal Programs

## ZSD_SHDB: Dynamic Batch Input Tool via Excel

### Overview
This ABAP utility simplifies mass data uploads by integrating directly with Transaction SHDB (Transaction Recorder). It automates the gap between recording a transaction and executing a Batch Input, eliminating the need for custom coding for every different upload.

### Key Features
* **Template Generation:** Automatically generates and downloads an Excel template based on the data structure of a specific SHDB recording.
* **Data Upload:** Reads the populated Excel file and maps the data back to the recording fields.
* **Dynamic Execution:** Executes the Batch Input (BDC) session for any transaction recorded in SHDB.
* **Portability:** Designed as a standalone report, making it easy to deploy across different SAP landscapes.

### How it Works
1. **Record:** Create a recording in SHDB for the target transaction.
2. **Download:** Run this program to export the required structure to Excel.
3. **Fill:** Populate the Excel file with your data.
4. **Upload & Process:** Use the program to upload the file; it will trigger the Batch Input.
