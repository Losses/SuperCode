# SuperCode

**SuperCode** is an interactive Jamovi module designed to simplify the generation, understanding, and application of coding schemes on categorical variables. 

**SuperCode** provides an intuitive, transposed preview of the coding matrix along with clear semantic descriptions of the mathematical comparisons executed by each contrast (e.g. `Level B - Level A` instead of just coding numbers). It also allows you to append the computed contrast columns directly to your jamovi dataset for downstream analyses.

## Development

Since the module is written as a jamovi R package, you can compile and install it directly using `jmvtools` inside your R environment:

```R
# Install jmvtools if you haven't already
install.packages('jmvtools', repos='https://jamovi.org/cran')

# Inside the project root directory, run:
jmvtools::install()
```

If you are running jamovi/R under Nix, load the environment containing RStudio/jamovi and execute `jmvtools::install()` from the R console.


## Usage

1. Open **jamovi** and load your dataset.
2. Select **SuperCode** from the analysis menu.
3. Drag your categorical/factor variables into the **Variables** box.
4. Customize the contrast settings for each variable under the options list:
   - Choose the coding scheme (e.g., Dummy, Helmert, Forward Difference).
   - Select the desired reference level (if supported by the coding scheme).
   - Enable/disable standardization.
5. Inspect the **Coding Matrix Preview** table to see exactly what comparisons are being set up.
6. Click the **Add Columns** button to append the coded columns back into your jamovi spreadsheet for regression or general linear modeling.


## Supported Coding Schemes & Descriptions

| Coding Scheme | Parameter Row | Contrast / Comparison Task |
|---|---|---|
| **Dummy (Treatment)** | `dummy j` | $Level_{j+1} - Level_1$ |
| **Simple** | `simple j` | $Level_{j+1} - Level_1$ |
| **Deviation (Sum)** | `deviation j` | $Level_j - Mean(Level_1, \dots, Level_k)$ |
| **Orthogonal Polynomial** | `poly j` | Orthogonal polynomial trend of degree $j$ (linear, quadratic, etc.) |
| **Helmert** | `helmert j` | $Level_j - Mean(Level_{j+1}, \dots, Level_k)$ |
| **Reverse Helmert** | `revhelmert j` | $Level_{j+1} - Mean(Level_1, \dots, Level_j)$ |
| **Forward Difference** | `forward j` | $Level_j - Level_{j+1}$ |
| **Backward Difference** | `backward j` | $Level_{j+1} - Level_j$ |

## License

This project is licensed under the GPL (>= 3) License.
