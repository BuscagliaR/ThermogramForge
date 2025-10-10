# ThermogramForge

<!-- badges: start -->
[![R-CMD-check](https://github.com/BuscagliaR/ThermogramForge/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BuscagliaR/ThermogramForge/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

**ThermogramForge** is an R Shiny web application for analyzing thermal liquid biopsy (TLB) thermogram data. It provides an interactive interface for baseline endpoint detection and adjustment, automated signal quality assessment, comprehensive metric calculation, and professional report generation.

This application is a complete rewrite of the original Python/Dash ThermogramForge implementation, leveraging native R packages ([ThermogramBaseline](https://github.com/BuscagliaR/ThermogramBaseline) and [tlbparam](https://github.com/BuscagliaR/tlbparam)) for all computational analysis.

## Features

- **Interactive Data Upload**: Support for CSV and Excel files with automatic format detection
- **Automated Baseline Detection**: Intelligent spline-based endpoint detection using ThermogramBaseline
- **Signal Quality Assessment**: Automated detection of low-quality or noise-only thermograms
- **Visual Review Interface**: Interactive plotting with manual endpoint adjustment
- **Undo/Redo Capability**: Full history tracking for all manual adjustments
- **Comprehensive Metrics**: Calculate standard TLB metrics (TPeak, Area, TFM, FWHM, etc.)
- **Professional Reports**: Export results to CSV or Excel with customizable metrics
- **Modern UI**: Clean, responsive interface mimicking the original Python application

## Installation

### Prerequisites

- R (≥ 4.1.0)
- RStudio (recommended, ≥ 2022.07.0)

### Install from GitHub

```r
# Install remotes if you don't have it
install.packages("remotes")

# Install ThermogramForge
remotes::install_github("BuscagliaR/ThermogramForge")
```

## Quick Start

```r
# Load the package
library(ThermogramForge)

# Launch the application
run_app()
```

The application will open in your default web browser.

## Workflow

1. **Upload Data**: Load thermogram data from CSV or Excel files
2. **Process**: Automatically detect baselines and assess signal quality
3. **Review**: Visually inspect and manually adjust endpoints as needed
4. **Report**: Generate comprehensive reports with calculated metrics

## Development Status

**Current Version**: 0.1.0 (Development)

This project is under active development. See [NEWS.md](NEWS.md) for version history and the [project roadmap](https://github.com/BuscagliaR/ThermogramForge/issues) for planned features.

## Documentation

- [User Guide](vignettes/user-guide.Rmd) (Coming soon)
- [Developer Guide](vignettes/developer-guide.Rmd) (Coming soon)
- [API Documentation](https://buscagliar.github.io/ThermogramForge/) (Coming soon)

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details on:

- Reporting bugs
- Suggesting features
- Submitting pull requests
- Development workflow

## Related Packages

- [ThermogramBaseline](https://github.com/BuscagliaR/ThermogramBaseline): R package for thermogram baseline detection
- [tlbparam](https://github.com/BuscagliaR/tlbparam): R package for TLB metric calculation

## Citation

If you use ThermogramForge in your research, please cite:

```
Reger, C. (2025). ThermogramForge: Interactive Thermal Liquid Biopsy Analysis.
R package version 0.1.0. https://github.com/BuscagliaR/ThermogramForge
```

## License

MIT License - see [LICENSE](LICENSE) file for details.

## Contact

- **Author**: Chris Reger
- **Email**: kcr28@nau.edu
- **Issues**: https://github.com/BuscagliaR/ThermogramForge/issues

## Acknowledgments

This project builds upon the Python ThermogramForge implementation and incorporates algorithms from the ThermogramBaseline and tlbparam R packages.