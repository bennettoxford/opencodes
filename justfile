alias help := list

# List available commands
list:
    @just --list --unsorted

# Render and publish a manuscript to Google Drive
publish file:
    Rscript -e "pubthis::publish('{{file}}')"

# Open a published manuscript in the browser
open file:
    Rscript -e "pubthis::open_published('{{file}}')"

# Show instructions for Google Drive authentication
auth-gdrive:
    @echo "Run this once in an interactive R session (e.g. Positron/RStudio) to cache credentials:"
    @echo "googledrive::drive_auth()"
    @echo ""
    @echo "Then verify it worked:"
    @echo "googledrive::drive_user()"
    @echo ""
    @echo "To deauthorise:"
    @echo "googledrive::drive_deauth()"
