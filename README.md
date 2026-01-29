# sntmethods

Analytical Methods for Sub-National Tailoring (SNT) Workflows

## Overview

**sntmethods** provides core analytical functions for Sub-National Tailoring (SNT) processes, including:

- Incidence estimation methods
- Imputation of routine health data
- Campaign performance metrics
- Small-area statistical methods (MBG)
- DHS survey analysis

Designed as a companion to 'sntutils', this package focuses on epidemiologically grounded methods that produce reproducible, standardized outputs for malaria and related programmatic analyses.

## Installation

### Basic Installation

```r
# Install from GitHub
# install.packages("remotes")
remotes::install_github("ahadi-analytics/sntmethods")
```

### Full Installation (with spatial features)

Some features (MBG outputs, spatial processing) require additional system libraries:

#### macOS
```bash
# Install spatial libraries via Homebrew
brew install gdal geos proj

# Then install the package
R -e 'remotes::install_github("ahadi-analytics/sntmethods", dependencies = TRUE)'
```

#### Ubuntu/Debian
```bash
# Install spatial libraries
sudo apt-get update
sudo apt-get install -y \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libudunits2-dev

# Then install the package
R -e 'remotes::install_github("ahadi-analytics/sntmethods", dependencies = TRUE)'
```

#### Windows
Windows users typically don't need additional system setup. Spatial packages come with pre-compiled binaries.

```r
remotes::install_github("ahadi-analytics/sntmethods", dependencies = TRUE)
```

### Minimal Installation (without spatial features)

If you don't need MBG or spatial processing:

```r
remotes::install_github(
  "ahadi-analytics/sntmethods",
  dependencies = NA,  # Skip suggested packages
  upgrade = "never"
)
```

## Usage

```r
library(sntmethods)

# Example: Calculate incidence
# See vignettes for detailed examples
```

## Troubleshooting

### PROJ/GDAL Issues

If you encounter errors like "Cannot find proj.db" or "proj_create failed":

**macOS:**
```bash
# Reinstall PROJ
brew reinstall proj

# Set environment variable (add to ~/.zshrc or ~/.bash_profile)
export PROJ_LIB=$(brew --prefix proj)/share/proj
```

**Linux:**
```bash
sudo apt-get install --reinstall proj-bin proj-data
```

**Still having issues?** The core package functions work without spatial dependencies. Only MBG-related functions require PROJ/GDAL.

## Development

See [CLAUDE.md](CLAUDE.md) for development guidelines and commands.

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Citation

```r
citation("sntmethods")
```

## Issues

Report bugs at: https://github.com/ahadi-analytics/sntmethods/issues
