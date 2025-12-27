# Description
This is a simple project I created to better understand my expenses. Currently it only analyzes grocery expenses but I am working to add others, hence the blank sections at the end. My data is pulled from a Google Sheet using my locally-stored authentication; other users will instead receive randomly-generated sample data.

# File Structure
```
.
├── tools                             # Tools and utilities
    ├── generate_sample_data.R        # Creates sample data for non-authenticated users
    └── get_data.R                    # Retrieves data, either from the Google Sheet or by generating sample data
├── .gitignore
├── README.md
└── expenses.Rmd                      # Main script for generating report
```

## Local Files
The files below are not included in the repository. They are stored locally on my personal device, and so you may see references to them in the code. The scripts should run successfully (with simulated data) without these files.  
- _.Renviron_: An environment file containing global variables for the local gargle key path (GARGLE_KEY_PATH) and the Google account email address (MY_EMAIL). GARGLE_KEY_PATH is used if utilizing a Google Cloud Console service account, otherwise use a Google account (MY_EMAIL) with access to the sheet.
- _expenses.Rproj_: Preserves the file structure within my computer; useful for retrieving the cached authentication token.
