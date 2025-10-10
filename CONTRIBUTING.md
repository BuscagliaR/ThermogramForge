# Contributing to ThermogramForge

Thank you for your interest in contributing to ThermogramForge! This document provides guidelines and instructions for contributing.

## Development Workflow

We use a feature-branch workflow with the following branches:

- **`main`**: Stable releases only
- **`develop`**: Integration branch for features (not yet created)
- **`feature/*`**: Feature development branches
- **`bugfix/*`**: Bug fix branches
- **`hotfix/*`**: Critical fixes for production

## Getting Started

### Prerequisites

- R (≥ 4.1.0)
- RStudio (recommended)
- Git
- GitHub account

### Setup Development Environment

```bash
# Clone the repository
git clone https://github.com/BuscagliaR/ThermogramForge.git
cd ThermogramForge

# Create a feature branch
git checkout -b feature/your-feature-name

# Install development dependencies
R -e "install.packages('devtools')"
R -e "devtools::install_dev_deps()"
```

## Making Changes

### 1. Create a Feature Branch

```bash
git checkout -b feature/descriptive-name
```

Branch naming conventions:
- `feature/add-data-validation`
- `bugfix/fix-plot-rendering`
- `docs/update-readme`

### 2. Make Your Changes

- Write clean, documented code
- Follow the [tidyverse style guide](https://style.tidyverse.org/)
- Add roxygen2 documentation for all functions
- Include unit tests for new functionality

### 3. Test Your Changes

```r
# Load the package
devtools::load_all()

# Run tests
devtools::test()

# Check package
devtools::check()

# Test the Shiny app
run_app()
```

### 4. Commit Your Changes

Write clear, descriptive commit messages following this format:

```
type: brief description (50 chars or less)

Longer explanation if needed (wrap at 72 characters).
Include context about what changed and why.

Fixes #123
```

Commit types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (formatting, etc.)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

Example:
```bash
git add .
git commit -m "feat: add CSV upload validation

Implements file format detection and validation for uploaded CSV files.
Validates required columns (Temperature, dCp) and provides clear error
messages for malformed files.

Closes #42"
```

### 5. Push and Create Pull Request

```bash
git push origin feature/your-feature-name
```

Then create a Pull Request on GitHub with:
- Clear title and description
- Reference to related issues
- Screenshots for UI changes
- Test results

## Code Standards

### R Code Style

- Use `<-` for assignment (not `=`)
- Use snake_case for function and variable names
- Maximum line length: 80 characters
- Use spaces around operators
- Indent with 2 spaces

### Documentation

All exported functions must have roxygen2 documentation:

```r
#' Brief function description
#'
#' @description
#' Longer description with details about what the function does.
#'
#' @param param_name Description of parameter
#' @param another_param Description of another parameter
#'
#' @return Description of return value
#'
#' @examples
#' \dontrun{
#' example_function(param = "value")
#' }
#'
#' @export
example_function <- function(param_name, another_param) {
  # Function body
}
```

### Testing

- Write tests for all new functions
- Use `testthat` framework
- Aim for >70% code coverage
- Include edge cases and error conditions

```r
test_that("function handles valid input correctly", {
  result <- example_function(valid_input)
  expect_equal(result, expected_output)
})

test_that("function errors on invalid input", {
  expect_error(example_function(invalid_input))
})
```

## Shiny Module Guidelines

When creating or modifying Shiny modules:

1. Use the namespace pattern with `NS(id)`
2. Separate UI and server functions
3. Use descriptive module names: `mod_feature_name`
4. Document module parameters and return values
5. Keep modules focused on a single responsibility

## Pull Request Process

1. Update documentation if you've changed functionality
2. Update NEWS.md with your changes
3. Ensure all tests pass (`devtools::check()`)
4. Request review from maintainers
5. Address review feedback
6. Wait for approval before merging

## Reporting Bugs

Use GitHub Issues to report bugs. Include:

- Brief descriptive title
- Steps to reproduce
- Expected behavior
- Actual behavior
- Screenshots if applicable
- System information (OS, R version)

## Suggesting Features

Feature requests are welcome! Please:

- Check existing issues first
- Explain the use case
- Describe the proposed solution
- Consider alternatives

## Questions?

- Open a GitHub Discussion for general questions
- Use GitHub Issues for specific bugs or features
- Contact: kcr28@nau.edu

## Code of Conduct

Be respectful and professional in all interactions. We aim to foster an open and welcoming environment.

Thank you for contributing to ThermogramForge!