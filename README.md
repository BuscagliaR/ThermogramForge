# ThermogramForge

<!-- badges: start -->
[![R-CMD-check](https://github.com/BuscagliaR/ThermogramForge/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BuscagliaR/ThermogramForge/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Project Status: Active](https://img.shields.io/badge/status-active%20development-brightgreen.svg)](https://github.com/BuscagliaR/ThermogramForge)
<!-- badges: end -->

> **Interactive R Shiny Application for Thermal Liquid Biopsy Thermogram Analysis**

## 📋 Overview

**ThermogramForge** is a comprehensive R Shiny web application for analyzing thermal liquid biopsy (TLB) thermogram data. It provides an intuitive interface for:

- 📊 **Data Upload & Validation** - Support for CSV and Excel with automatic format detection
- 🔬 **Automated Baseline Detection** - Intelligent spline-based endpoint detection
- 🔍 **Signal Quality Assessment** - Automated detection of low-quality thermograms
- ✏️ **Interactive Review** - Manual endpoint adjustment with real-time visualization
- 💾 **Multi-Dataset Management** - Work with multiple datasets simultaneously
- 📈 **Comprehensive Reports** - Professional metric calculation and export

This application is a complete rewrite of the [Python/Dash ThermogramForge](https://github.com/Naalu/ThermogramForge), leveraging native R packages for all computational analysis while maintaining visual and functional consistency with the original.

## ✨ Key Features

### Data Management
- ✅ Upload multiple raw thermogram datasets (CSV/Excel)
- ✅ Process datasets independently with individual controls
- ✅ Save processed data to RDS (full reload), CSV, or Excel (export)
- ✅ Load saved datasets from disk
- ✅ Switch between datasets for review or report generation
- ✅ Automatic format detection (single/multi-sample, long/wide)

### Baseline Detection & Processing
- ✅ Automated baseline endpoint detection via [ThermogramBaseline](https://github.com/BuscagliaR/ThermogramBaseline)
- ✅ Signal quality assessment using ARIMA-based stationarity testing
- ✅ Batch processing with progress indicators
- ✅ Support for variable-length samples

### Interactive Review
- ✅ Interactive sample grid with sorting and filtering
- ✅ Dual-view plotting (Raw Thermogram / Baseline Subtracted)
- ✅ Click-to-adjust manual endpoint modification
- ✅ Real-time baseline recalculation
- ✅ Full undo/redo history system
- ✅ Sample navigation (Previous/Next)
- ✅ Review status tracking (Reviewed/Exclude checkboxes)

### Professional Workflow
- ✅ Session-based multi-dataset management
- ✅ Clear status indicators (Unprocessed/Processed/Loaded)
- ✅ Smart navigation based on data type
- ✅ Comprehensive file operations (save/load/delete)
- ✅ Intuitive UI matching Python application design

## 🚀 Installation

### Prerequisites

- **R** (≥ 4.1.0)
- **RStudio** (recommended, ≥ 2022.07.0)

### Install from GitHub

```r
# Install remotes if you don't have it
install.packages("remotes")

# Install ThermogramForge
remotes::install_github("BuscagliaR/ThermogramForge")
```

## 💻 Quick Start

```r
# Load the package
library(ThermogramForge)

# Launch the application
run_app()
```

The application will open in your default web browser at `http://127.0.0.1:####`

## 📖 Usage Workflow

### 1. Upload Data
- Click **"Upload New Raw Thermogram Data"** on the Data Overview tab
- Select CSV or Excel file containing thermogram data
- Preview and confirm upload

### 2. Process Data
- Click **"Process Data"** next to your uploaded dataset
- Automatic baseline detection and signal quality assessment runs
- View processing summary (samples processed, signal quality)

### 3. Review Endpoints
- Click **"Review Endpoints"** on processed dataset
- Visual inspection of each sample's baseline
- Manually adjust endpoints by clicking plots if needed
- Use undo/redo for any mistakes
- Mark samples as reviewed or excluded

### 4. Save & Generate Reports
- Click **"Save Processed Data"** to save your work
- Choose format: RDS (reloadable), CSV, or Excel (export)
- Use **"Create Reports"** to generate comprehensive metric reports *(Phase 8)*

## 📊 Supported Data Formats

ThermogramForge automatically detects and handles:

- **Single-sample format**: `Temperature`, `dCp` columns
- **Multi-sample long format**: `Sample_ID`, `Temperature`, `dCp` columns
- **Multi-sample wide format**: `T1a`, `1a`, `T1b`, `1b`, ... pattern

Files up to **150MB** are supported.

## 🔧 Development Status

**Current Version**: 0.1.0 (Active Development)

| Phase | Status | Description |
|-------|--------|-------------|
| Phase 1 | ✅ Complete | Project setup and infrastructure |
| Phase 2 | ✅ Complete | Data loading and validation |
| Phase 3 | ✅ Complete | Baseline detection and signal quality |
| Phase 4-6 | ✅ Complete | Interactive review interface |
| **Phase 7** | ✅ **Complete** | **Multi-dataset management & save/load** |
| Phase 8 | 🚧 Next | Report builder with tlbparam metrics |
| Phase 9 | 📋 Planned | Polish and optimization |
| Phase 10 | 📋 Planned | Documentation and release |

See [NEWS.md](NEWS.md) for detailed version history and [GitHub Issues](https://github.com/BuscagliaR/ThermogramForge/issues) for the development roadmap.

## 📚 Documentation

- **[NEWS.md](NEWS.md)** - Version history and changelog
- **[CONTRIBUTING.md](CONTRIBUTING.md)** - Development guide
- User Guide - Coming soon
- Developer Guide - Coming soon
- API Documentation - Coming soon

## 🤝 Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details on:

- 🐛 Reporting bugs
- 💡 Suggesting features
- 🔀 Submitting pull requests
- 🧪 Testing guidelines
- 📝 Documentation standards

## 🔗 Related Packages

- [**ThermogramBaseline**](https://github.com/BuscagliaR/ThermogramBaseline) - R package for automated baseline detection
- [**tlbparam**](https://github.com/BuscagliaR/tlbparam) - R package for TLB metric calculation
- [**ThermogramForge (Python)**](https://github.com/Naalu/ThermogramForge) - Original Python/Dash implementation

## 📄 Citation

If you use ThermogramForge in your research, please cite:

```bibtex
@software{thermogramforge_r,
  author = {Reger, Karl},
  title = {ThermogramForge: Interactive Thermal Liquid Biopsy Analysis},
  year = {2025},
  url = {https://github.com/BuscagliaR/ThermogramForge},
  version = {0.1.0}
}
```

## 📜 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

Copyright (c) 2025 Chris Reger and Robert Buscaglia

## 👥 Authors & Acknowledgments

- **Chris Reger** - Lead Developer (kcr28@nau.edu)
- **Dr. Robert Buscaglia** - Project Advisor

**Institutional Support:**
- Northern Arizona University
- Buscaglia Lab

**Technical Foundation:**
This project builds upon:
- The Python ThermogramForge implementation
- ThermogramBaseline R package algorithms
- tlbparam R package for metric calculations

## 📞 Contact & Support

- **Issues**: [GitHub Issues](https://github.com/BuscagliaR/ThermogramForge/issues)
- **Email**: kcr28@nau.edu
- **Repository**: https://github.com/BuscagliaR/ThermogramForge

---

**Built with ❤️ using R, Shiny, and the R scientific computing ecosystem**