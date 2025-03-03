# survivalTB
Survival analysis with interval data, bootstrap and interactive visualization.

## Installation

This package depends on **interval**, which in turn requires **Icens**. However, **Icens** has been archived on CRAN, causing automated installations to fail. To install **survivalTB** successfully, please follow these steps:

1. **Install Icens Manually**  
   - Visit the [CRAN archive for Icens](https://cran.r-project.org/src/contrib/Archive/Icens/).  
   - Download the most recent `.tar.gz` file.  
   - In R, install it manually:
     ```r
     install.packages("path/to/Icens_x.y.z.tar.gz", repos = NULL, type = "source")
     ```

2. **Install interval**  
   ```r
   install.packages("interval")
   ```

3. **Proceed with installing survivalTB**  
