# ShinyCardinal Refactoring Plan

## Purpose

This document records the initial architectural review of ShinyCardinal v0.3.5 and provides a practical roadmap for future refactoring.

The goal is **not** to rewrite the application from scratch. The goal is to improve clarity, testability, performance, modularity, dependency control, and long-term maintainability while preserving existing scientific functionality.

The refactoring should follow the Yonghui Style principles:

- Base R first where reasonable
- minimal dependencies
- function-first design
- clear module boundaries
- one function, one responsibility
- readable and predictable code
- gradual rather than disruptive change
- no unnecessary abstraction

---

## Current strengths

ShinyCardinal already has a strong foundation:

- it is organized as an R package;
- it uses golem and Shiny modules;
- `app_ui()` and `app_server()` are relatively small;
- major workflows are separated into modules;
- large functional areas such as preprocessing, visualization, segmentation, network analysis, identification, and export are already distinguished;
- the application is suitable for incremental refactoring rather than a full rewrite.

The main architectural issue is not the absence of modules. The main issue is that many modules still mix UI coordination, file operations, validation, scientific computation, state mutation, plotting, and output rendering in the same function.

---

## Main refactoring goals

### 1. Separate Shiny coordination from core R logic

Any code that does not require `input`, `output`, or `session` should be extracted from module servers into ordinary R functions.

Target structure:

```text
UI layer
    mod_xxx_ui()

Shiny coordination layer
    mod_xxx_server()

Core layer
    read_xxx()
    validate_xxx()
    calculate_xxx()
    plot_xxx()
```

Core functions should be callable directly from the R console and testable without launching Shiny.

---

### 2. Keep one function responsible for one task

Avoid server functions that simultaneously:

- inspect user input;
- search files;
- rename files;
- validate pairs;
- build parameters;
- run scientific computation;
- modify global state;
- render text or plots.

For example, the current imzML workflow should gradually be decomposed into functions such as:

```r
find_local_msi_files()
prepare_uploaded_msi_files()
match_msi_file_pairs()
validate_msi_file_pairs()
build_msi_read_options()
read_msi_data()
format_msi_summary()
```

The Shiny module should coordinate these functions rather than contain their full implementation.

---

### 3. Do not perform heavy work inside `render*()`

Heavy computation, file-system changes, data loading, and shared-state mutation should not be hidden inside `renderPrint()`, `renderPlot()`, or other renderers.

Preferred pattern:

```r
loaded_data <- eventReactive(input$load_data, {
  files <- resolve_msi_files(...)
  validate_msi_file_pairs(files)
  read_msi_data(files, ...)
})

observeEvent(loaded_data(), {
  state$msi_data <- loaded_data()
})

output$msi_info <- renderPrint({
  loaded_data()
})
```

Renderers should primarily render already-computed results.

---

### 4. Replace open global state with controlled interfaces

The current application passes a shared `reactiveValues()` object to many modules:

```r
global <- reactiveValues(
  msiData = NULL,
  processedMSIData = NULL,
  cleanedMSIData = NULL,
  ionImage = NULL
)
```

This creates hidden coupling because any module can potentially read or modify any field.

Refactoring direction:

- each module should receive only the reactive values it needs;
- modules should return explicit outputs;
- data flow should be directional and visible;
- ownership of each state should be clear.

Preferred pattern:

```r
upload_result <- mod_upload_data_server("upload_data")

preprocess_result <- mod_preprocess_server(
  "preprocess",
  raw_msi_data = upload_result$raw_msi_data
)

mod_view_data_server(
  "view_data",
  msi_data = preprocess_result$processed_msi_data
)
```

Long-term target:

```text
read data
    ↓
raw MSI reactive
    ↓
preprocessing
    ↓
processed MSI reactive
    ↓
visualization / segmentation / network / export
```

---

### 5. Clarify module boundaries

The project currently contains both page-level modules and small operation-level modules under the same `mod_*` naming convention.

Refactoring should distinguish:

- page modules;
- feature modules;
- core calculation functions;
- plotting functions;
- validation helpers;
- state or data-model helpers.

Because standard R packages normally keep source files flat under `R/`, use file-name prefixes rather than adding complex build machinery.

Suggested naming pattern:

```text
R/
  app-ui.R
  app-server.R
  app-run.R

  data-read-imzml.R
  data-validate-files.R
  data-upload.R

  preprocess-ui.R
  preprocess-server.R
  preprocess-core.R

  plot-msi-ui.R
  plot-msi-server.R
  plot-msi-core.R

  segmentation-ui.R
  segmentation-server.R
  segmentation-core.R
```

Do not reorganize all files at once. Apply the pattern gradually when each module is refactored.

---

### 6. Standardize naming

The current code mixes camelCase, abbreviations, and snake_case.

Examples to migrate gradually:

```text
mod_uploadData_ui      → mod_upload_data_ui
mod_readImzML_server   → mod_read_imzml_server
processedMSIData       → processed_msi_data
msiDataType            → msi_data_type
filePath               → file_paths
selectedMassRange      → selected_mass_range
```

For exported or externally used functions, preserve backward compatibility during a transition period:

```r
mod_uploadData_ui <- mod_upload_data_ui
```

Breaking renames should be reserved for a major version.

---

### 7. Audit dependencies by role

ShinyCardinal is scientific software, so it cannot follow the same dependency rule as a simple dashboard. Some packages are genuine scientific requirements.

Dependencies should be classified as:

```text
core scientific dependency
core application dependency
optional feature dependency
UI convenience dependency
development-only dependency
deployment-only dependency
```

Likely core dependencies:

- Cardinal
- shiny
- BiocParallel, if parallel processing is required
- MSbox, if its algorithms are essential

Dependencies requiring review:

- golem
- config
- shinydashboard
- shinycssloaders
- DT
- plotly
- shinyFiles
- visNetwork
- igraph
- sp

Important principle:

> Reduce coupling before removing packages. Do not rewrite the whole application merely to delete a dependency.

Create a dependency audit table before removal:

```text
Package | Used in | Core or optional | Possible replacement | Removal cost
```

Potential future actions:

- replace `shinycssloaders` with CSS if it is only used for spinners;
- consider making `plotly` optional if used only in a limited feature;
- consider making `visNetwork` optional if interactive network display is not essential to the core package;
- retain `igraph` if it provides core network calculations;
- review whether `sp` remains necessary with the current Cardinal data model;
- do not remove golem in the first refactoring stage.

---

## Specific issues already identified

### imzML/ibd validation bug

The current validation checks the imzML path twice. The second check should validate the ibd path.

Current pattern:

```r
need(file_paths$imzml_path != "", "imzML file missing!")
need(file_paths$imzml_path != "", "ibd file missing!")
```

Correct direction:

```r
need(length(file_paths$imzml_path) > 0L, "imzML file missing!")
need(length(file_paths$ibd_path) > 0L, "ibd file missing!")
```

### File patterns should be anchored

Prefer:

```r
pattern = "\\.imzML$"
pattern = "\\.ibd$"
```

rather than:

```r
pattern = ".imzML"
pattern = ".ibd"
```

because `.` is a wildcard in regular expressions.

### Typo in error message

Use `ibd`, not `idb`.

### File operations should be isolated

Temporary uploaded-file renaming, path resolution, and file matching should be moved into dedicated functions with explicit return values.

---

## Recommended refactoring phases

### Phase 0 — Preparation

- Create a dedicated refactoring branch.
- Do not refactor directly on `main`.
- Record the current application version and expected behavior.
- Prepare small representative MSI test datasets.
- Add or update architecture and coding-style documentation.

### Phase 1 — Low-risk cleanup

Goal: improve code without changing behavior or UI.

Tasks:

- fix obvious validation bugs and typos;
- standardize formatting in files being touched;
- extract pure helper functions from modules;
- move heavy operations out of `render*()`;
- add tests for file matching, input validation, option conversion, and other pure functions;
- preserve all current module interfaces where possible.

Recommended first targets:

1. `mod_readImzML.R`
2. `mod_preprocessMSI.R`
3. `mod_plotMSI.R`

### Phase 2 — Data-flow cleanup

Goal: make state ownership and module communication explicit.

Tasks:

- reduce passing of the entire global state object;
- define module inputs and outputs explicitly;
- distinguish raw, processed, cleaned, and derived MSI objects;
- prevent unrelated modules from modifying shared objects;
- remove duplicated reactive filtering and computation;
- document the data-flow graph.

### Phase 3 — Dependency audit

Goal: reduce optional and UI-only dependencies safely.

Tasks:

- create the dependency audit table;
- identify packages used in only one file or one feature;
- move suitable packages from `Imports` to `Suggests` if appropriate;
- add graceful feature checks for optional packages;
- remove convenience dependencies only when the replacement is simpler overall.

### Phase 4 — Naming and file organization

Goal: make the codebase visually and structurally consistent.

Tasks:

- adopt snake_case for new internal functions and objects;
- introduce compatibility aliases for renamed public functions;
- group files using consistent prefixes;
- reduce duplicate comments generated by templates;
- keep comments focused on design reasons rather than obvious operations.

### Phase 5 — UI and design language

Goal: modernize appearance without disturbing scientific behavior.

Tasks:

- apply a consistent design language;
- simplify panels, colors, spacing, buttons, and result areas;
- evaluate whether `shinydashboard` should remain;
- do not mix UI redesign with scientific refactoring in the same pull request.

### Phase 6 — Major-version cleanup

Only after earlier phases are stable:

- remove deprecated function aliases;
- make breaking naming changes;
- simplify global state further;
- reconsider golem or other structural dependencies if there is a strong reason;
- release as a major version.

---

## Rules for each refactoring pull request

Each PR should:

- address one clear concern;
- preserve scientific output unless explicitly documented;
- avoid simultaneous architecture, UI, and algorithm changes;
- include before/after behavior notes;
- include tests for extracted pure functions where practical;
- avoid adding dependencies unless total complexity is clearly reduced;
- keep backward compatibility when feasible;
- remain small enough to review confidently.

Suggested PR size:

- one module or one architectural concern at a time;
- avoid repository-wide renaming and logic changes in the same PR.

---

## Review checklist

### Functions

- Does each function have one clear responsibility?
- Can core logic run without Shiny?
- Are inputs and outputs explicit?
- Does the function rely unnecessarily on global state?
- Is the function short enough to understand in one reading?

### Modules

- Does the module receive only the values it needs?
- Does it return explicit reactive outputs?
- Does it modify state owned by another module?
- Is scientific computation separated from UI coordination?

### Reactive design

- Is heavy work triggered intentionally?
- Is expensive computation hidden inside a renderer?
- Is the same computation repeated in multiple outputs?
- Is state ownership clear?

### Dependencies

- Is the package scientifically essential?
- Is it used broadly or only for one convenience feature?
- Can the feature degrade gracefully without it?
- Does removal actually reduce total complexity?

### Naming and readability

- Are names clear and consistent?
- Does the code use snake_case for new internal functions?
- Do comments explain why rather than repeat what?
- Will the code still be understandable in several years?

### Safety

- Is behavior unchanged unless explicitly intended?
- Are file operations validated?
- Are user-facing errors clear?
- Are representative MSI datasets available for regression testing?

---

## First practical refactoring target

Start with `mod_readImzML.R` because it has a clear boundary and currently combines many responsibilities.

Proposed first decomposition:

```r
find_msi_files()
prepare_uploaded_files()
match_msi_pairs()
validate_msi_pairs()
build_read_msi_options()
read_msi_dataset()
format_msi_info()
```

Then reduce the module server to:

```r
mod_read_imzml_server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    loaded_data <- eventReactive(input$load_data, {
      files <- resolve_msi_files(...)
      validate_msi_pairs(files)
      read_msi_dataset(files, build_read_msi_options(input))
    })

    output$msi_data_info <- renderPrint({
      format_msi_info(loaded_data())
    })

    list(
      msi_data = loaded_data,
      msi_data_type = reactive(input$msi_data_type)
    )
  })
}
```

This first step will establish the pattern for the rest of the project.

---

## Final principle

> Shiny modules should coordinate the application. Ordinary R functions should perform the scientific work.

The long-term objective is a ShinyCardinal codebase that is:

- scientifically reliable;
- modular;
- function-oriented;
- easy to test;
- explicit in its data flow;
- conservative in dependencies;
- readable and maintainable for many years.
