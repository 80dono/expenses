# Description
This is a simple project I created to better understand my expenses. Currently it only analyzes grocery expenses but I am working to add others, hence the blank sections at the end. My data is pulled from a Google Sheet using my locally-stored authentication; other users will instead receive randomly-generated sample data.

# File Structure
.
├── tools                             # Tools and utilities
    ├── generate_sample_data.R        # Creates sample data for non-authenticated users
    ├── get_data.R                    # Retrieves data, either from the Google Sheet or by generating sample data
├── .gitignore
├── expenses.Rmd                      # Main script for generating report
└── README.md

## Local Files
_email.txt_: A text file containing my personal email address for authentication
_expenses.Rproj_: Preserves the file structure within my computer; useful for retrieving authentication token
