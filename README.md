# Shiny App for Analyzing Measurement Data

This Shiny app allows users to upload and analyze measurement data, visualize interactions between different groups, and calculate area under the curve (AUC) statistics. The app provides interactive plots and summary statistics based on user inputs.

## Features

- **File Upload**: Upload CSV files containing measurement data.
- **Dynamic UI Elements**: Automatically update UI elements based on uploaded data.
- **Interaction Plots**: Generate interaction plots for treated and control groups.
- **AUC Calculation**: Calculate and visualize AUC for individual and grouped data.
- **Summary Statistics**: Display summary statistics for different groups and time points.

## Getting Started

### Prerequisites

- R (version 4.0 or higher)
- RStudio (optional but recommended)
- The following R packages:
  - `shiny`
  - `ggplot2`
  - `dplyr`
  - `knitr`

### Installation

1. **Clone the repository:**
    ```sh
    git clone https://github.com/yourusername/shiny-measurement-analysis.git
    cd shiny-measurement-analysis
    ```

2. **Install the required R packages:**
    ```r
    install.packages(c("shiny", "ggplot2", "dplyr", "knitr"))
    ```

### Running the App

1. **Open RStudio (or your preferred R environment).**
2. **Open the `app.R` file located in the repository.**
3. **Run the app by clicking the 'Run App' button in RStudio or using the following command:**
    ```r
    shiny::runApp('path_to_your_app_directory')
    ```

### Usage

1. **Upload Data**: Use the file input widget to upload a CSV file. The CSV file should contain the following columns:
    - `ID`: Unique identifier for each subject.
    - `Time`: Time points at which measurements were taken.
    - `Visit`: Different visit instances.
    - `Measurement`: The measurement values.
    - `treatment`: Group identifier (e.g., 0 for control, 1 for treated).

2. **Select Treatment Group**: Choose between 'Treated', 'Control', 'Pooled', or 'Individual' to filter the data.

3. **Generate Plots**: Use the provided controls to generate interaction plots and AUC plots.

4. **View Summary Statistics**: Display summary statistics for the selected group and variables.

## Project Structure

- `app.R`: Main application file containing UI and server logic.
- `data/`: Directory for storing sample data files (if any).
- `README.md`: This file, providing an overview and usage instructions.

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request or open an Issue if you have any suggestions or bug reports.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Acknowledgements

- This project uses the [Shiny](https://shiny.rstudio.com/) framework for building the interactive web application.
- Special thanks to the authors of the R packages used in this project.
