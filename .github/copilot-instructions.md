# GitHub Copilot Instructions for iphRa Repository

## General Guidelines

### Documentation and Reporting

**DO NOT** create implementation reports, completion reports, or delivery summaries in the main repository folder unless explicitly requested by the user. These include but are not limited to:

- Implementation summaries (e.g., `IMPLEMENTATION_SUMMARY.md`)
- Completion reports (e.g., `COMPLETION_REPORT.md`, `TASK_COMPLETION_REPORT.md`)
- Code review reports (e.g., `CODE_REVIEW_REPORT.md`)
- Delivery summaries (e.g., `DELIVERY_SUMMARY.md`)
- Fix documentation (e.g., `FIX_DOCUMENTATION.md`, `FIX_SUMMARY_*.md`)
- Agent review summaries (e.g., `AGENT_REVIEW_SUMMARY.txt`)
- PR summaries in the root folder (e.g., `PR_SUMMARY_FINAL.md`)

**Instead:**
- Use commit messages and PR descriptions to document changes
- Add meaningful documentation to the `docs/` folder only when it describes core functionality, architecture, or usage patterns
- If a user explicitly requests a specific report by name, create it in `/tmp` or another temporary location

### Code Organization

- Follow the existing R6 class hierarchy and naming conventions
- Keep utility functions organized by their primary usage context
- Maintain separation between validators, error handlers, and language utilities
- Add roxygen2 documentation to all exported functions
- Do not alter existing functionality unless explicitly requested

### Testing

- Organize tests to match the structure of class and utility files
- Remove duplicate tests when found
- Ensure adequate test coverage for new functionality
