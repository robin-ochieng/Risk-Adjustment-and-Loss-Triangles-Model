# Posit Connect Deployment Guide

## Overview
This guide explains how to generate a `manifest.json` file for deploying your Shiny app to Posit Connect using Git-based deployments.

## Prerequisites
- R and RStudio installed
- All project dependencies installed locally
- Git repository initialized
- Access to Posit Connect server

## Quick Start

### 1. Generate manifest.json

Run the generator script in R:

```r
source("generate_manifest.R")
```

This will:
- Analyze your `app.R` file for dependencies
- Capture exact versions of all installed packages
- Generate `manifest.json` with your R version and package information

### 2. Manual Generation (Alternative)

If you prefer to generate manually:

```r
library(rsconnect)

# Generate manifest for Shiny app
rsconnect::writeManifest(
  appDir = getwd(),
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)
```

### 3. Commit to Git

```bash
git add manifest.json
git commit -m "Add manifest.json for Posit Connect deployment"
git push origin main
```

### 4. Deploy to Posit Connect

1. Log into your Posit Connect dashboard
2. Create a new deployment
3. Select "Git-Based Deployment"
4. Connect your Git repository
5. Configure deployment settings
6. Deploy!

## What's in manifest.json?

The manifest file contains:
- **R version** - Exact R version used
- **Package list** - All dependencies with specific versions
- **App metadata** - Application mode and configuration
- **File checksums** - Content validation

## Required Packages

Based on your `app.R`, these packages must be installed:

- shiny
- bs4Dash
- tidyverse (includes ggplot2, dplyr, tidyr, etc.)
- bslib
- DT
- scales
- lubridate
- zoo
- ChainLadder
- shinycssloaders
- plotly
- rsconnect (for manifest generation)

## Installing Missing Packages

```r
# Install all required packages
required_packages <- c(
  "shiny", "bs4Dash", "tidyverse", "bslib", "DT", 
  "scales", "lubridate", "zoo", "ChainLadder", 
  "shinycssloaders", "plotly", "rsconnect"
)

# Check and install missing packages
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}
```

## Troubleshooting

### Error: "Package not found"
- Ensure all packages are installed locally
- Run `install.packages("package_name")` for missing packages

### Error: "rsconnect package required"
- Install rsconnect: `install.packages("rsconnect")`

### Manifest doesn't include all dependencies
- The script auto-detects dependencies from library() calls
- Manually verify packages in manifest.json
- Ensure all module files are in the project directory

### Git deployment fails
- Check that manifest.json is committed to repository
- Verify R version compatibility on Posit Connect
- Review Posit Connect deployment logs

## Best Practices

1. **Version Control**: Always commit manifest.json to your repository
2. **Update Regularly**: Regenerate manifest.json when packages are updated
3. **Test Locally**: Ensure app runs locally before deployment
4. **Document Dependencies**: Keep track of system dependencies (if any)
5. **Check R Version**: Ensure Posit Connect supports your R version

## Additional Resources

- [Posit Connect User Guide](https://docs.posit.co/connect/user/)
- [rsconnect Package Documentation](https://rstudio.github.io/rsconnect/)
- [Git-Based Deployment Guide](https://docs.posit.co/connect/user/git-backed/)

## Support

For issues with:
- **Manifest generation**: Check R console output and package installations
- **Deployment**: Review Posit Connect deployment logs
- **App functionality**: Test locally first, then check server logs
