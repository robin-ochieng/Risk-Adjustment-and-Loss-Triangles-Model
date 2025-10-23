# Generate manifest.json for Posit Connect deployment
# This script creates a manifest file with exact package versions installed on your machine

# Function to generate manifest
generate_manifest <- function(app_file = "app.R", 
                              output_file = "manifest.json",
                              app_mode = "shiny") {
  
  # Check if rsconnect is installed
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("rsconnect package is required. Install it with: install.packages('rsconnect')")
  }
  
  # Load rsconnect
  library(rsconnect)
  
  # Get the directory of the app
  app_dir <- getwd()
  
  cat("Generating manifest.json for Posit Connect deployment...\n")
  cat("App directory:", app_dir, "\n")
  cat("App file:", app_file, "\n\n")
  
  # Generate the manifest
  tryCatch({
    rsconnect::writeManifest(
      appDir = app_dir,
      appPrimaryDoc = app_file,
      appMode = app_mode,
      contentCategory = NULL,
      python = NULL,
      quarto = NULL
    )
    
    cat("✓ manifest.json successfully created!\n\n")
    
    # Read and display the manifest
    manifest_content <- jsonlite::read_json(output_file)
    
    cat("Manifest summary:\n")
    cat("- R version:", manifest_content$metadata$rVersion$Version, "\n")
    cat("- App mode:", manifest_content$metadata$appmode, "\n")
    cat("- Number of packages:", length(manifest_content$packages), "\n\n")
    
    cat("Key packages included:\n")
    for (pkg in manifest_content$packages) {
      cat(sprintf("  - %s (version %s)\n", pkg$Package, pkg$Version))
    }
    
    cat("\nYou can now commit manifest.json to your Git repository.\n")
    cat("Posit Connect will use this file to recreate your exact environment.\n")
    
    return(invisible(TRUE))
    
  }, error = function(e) {
    cat("Error generating manifest:", conditionMessage(e), "\n")
    cat("\nTroubleshooting tips:\n")
    cat("1. Make sure all required packages are installed\n")
    cat("2. Ensure app.R is in the current directory\n")
    cat("3. Try running the app locally first to verify dependencies\n")
    return(invisible(FALSE))
  })
}

# Alternative function using a more explicit approach
generate_manifest_explicit <- function(app_file = "app.R") {
  
  cat("Analyzing dependencies from", app_file, "...\n\n")
  
  # Read the app file
  app_code <- readLines(app_file)
  
  # Extract library calls
  library_pattern <- "library\\(([^)]+)\\)"
  require_pattern <- "require\\(([^)]+)\\)"
  
  libraries <- c()
  for (line in app_code) {
    lib_match <- regmatches(line, regexec(library_pattern, line))
    if (length(lib_match[[1]]) > 1) {
      pkg <- gsub("[\"\']", "", lib_match[[1]][2])
      libraries <- c(libraries, pkg)
    }
    
    req_match <- regmatches(line, regexec(require_pattern, line))
    if (length(req_match[[1]]) > 1) {
      pkg <- gsub("[\"\']", "", req_match[[1]][2])
      libraries <- c(libraries, pkg)
    }
  }
  
  libraries <- unique(libraries)
  
  cat("Detected packages:\n")
  for (pkg in libraries) {
    version <- tryCatch(
      as.character(packageVersion(pkg)),
      error = function(e) "NOT INSTALLED"
    )
    cat(sprintf("  - %s: %s\n", pkg, version))
  }
  
  cat("\nNow generating manifest with rsconnect...\n")
  generate_manifest(app_file = app_file)
}

# Main execution
cat("===========================================\n")
cat("  Manifest Generator for Posit Connect\n")
cat("===========================================\n\n")

# Check if rsconnect is available
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  cat("Installing rsconnect package...\n")
  install.packages("rsconnect")
}

# Generate the manifest
generate_manifest_explicit()

cat("\n===========================================\n")
cat("Next steps:\n")
cat("1. Review the generated manifest.json file\n")
cat("2. Commit manifest.json to your Git repository:\n")
cat("   git add manifest.json\n")
cat("   git commit -m 'Add manifest.json for Posit Connect deployment'\n")
cat("   git push\n")
cat("3. Configure your Posit Connect deployment\n")
cat("===========================================\n")
