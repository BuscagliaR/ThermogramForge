# Known Issues - ThermogramForge

This document tracks known issues, limitations, and workarounds for the ThermogramForge application.

## Active Issues

### Plot Zoom Persistence (Review Endpoints Module)

**Status:** Known Issue - Requires Further Investigation  
**Severity:** Medium (UX annoyance, not data integrity)  
**Date Identified:** December 5, 2024

**Description:**  
When using the Review Endpoints module, plot zoom state resets in two scenarios:

1. When clicking "Manually Adjust Lower/Upper Endpoint" buttons
2. After clicking on the plot to set a new endpoint

**Expected Behavior:**  
Zoom level should persist throughout the endpoint adjustment workflow, only resetting when navigating to a different sample.

**Current Behavior:**  
Plot zoom resets to full view (showing entire temperature range) whenever:

- User clicks either "Adjust" button to enter adjustment mode
- User clicks on plot to select new endpoint position

**Attempted Fixes:**

- Added `uirevision` parameter to plotly layout ❌
- Wrapped `adjustment_mode()` in `isolate()` to break reactive dependency ❌
- Both changes combined ❌

**Root Cause (Hypothesis):**  
The plot re-renders due to multiple reactive dependencies. Likely candidates:

- `app_data$processed_data` changes when endpoint is updated
- Complex interaction between Shiny observers and Plotly's event system
- Possible timing issue with state updates

**Workaround:**  
Users can re-zoom after each adjustment. While not ideal, data integrity is not affected.

**Next Steps for Investigation:**

- Test with `plotlyProxy()` to update plot data without full re-render
- Investigate use of `uirevision` with hash of data state rather than sample ID
- Consider implementing custom JavaScript solution for zoom preservation
- Review Plotly documentation for zoom persistence in reactive contexts

**Impact:**  

- Does not affect data quality or analysis results
- Minor inconvenience for users making many manual adjustments
- More significant impact for users working with zoomed views frequently

---

## Resolved Issues

_(None yet - issues will be moved here when resolved)_

---

## Future Considerations

### Performance with Large Datasets

Processing 100+ samples can be slow. Consider implementing:

- Async processing with `promises` and `future` packages
- Progress indicators with sample-by-sample updates
- Option to process subsets of samples

### Session Persistence

Currently no cross-session persistence for:

- Undo/redo history
- Unsaved work in Review Endpoints

Consider implementing browser-based storage or session save/restore functionality.

---

## Reporting New Issues

If you encounter a bug or unexpected behavior:

1. Check this document first to see if it's a known issue
2. Search [GitHub Issues](https://github.com/BuscagliaR/ThermogramForge/issues) for similar reports
3. If not found, create a new issue with:
   - Clear description of the problem
   - Steps to reproduce
   - Expected vs. actual behavior
   - Screenshots if applicable
   - Browser and R version information

---

**Last Updated:** December 9, 2025
