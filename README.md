# survivalTB
Survival analysis with interval data, bootstrap and interactive visualization.

## Installation

This package depends on **interval**, which in turn requires **Icens**. However, **Icens** has been archived on CRAN, causing automated installations to fail. To install **survivalTB** successfully, please follow these steps:

1. **Install Icens Manually**
   
   This package depends on **interval**, which in turn requires **Icens**. However, **Icens** has been archived on CRAN, causing automated installations to fail. You can:
   - **Download from the CRAN archive** [CRAN archive for Icens](https://cran.r-project.org/src/contrib/Archive/Icens/).
   - **Or install from Bioconductor** [Icens on Bioconductor](https://www.bioconductor.org/packages/release/bioc/html/Icens.html)

   Once downloaded,  
   - install manually in R:
     ```r
     install.packages("path/to/Icens_x.y.z.tar.gz", repos = NULL, type = "source")
     ```

3. **Install interval**  
   ```r
   install.packages("interval")
   ```

4. **Proceed with installing survivalTB**  
a) Using remotes (Recommended)
```r
install.packages("remotes")
remotes::install_github("martaaaa/survivalTB")
```

b) From Source (ZIP or Folder)
* Download the ZIP of this repo (e.g., survivalTB.zip) from GitHub.
* Unzip it.
In R, run:
```r
install.packages("path/to/survivalTB", repos = NULL, type = "source")
```
----------

To load the package:
```r
library(survivalTB)
```

Note: If you encounter any errors, confirm that:
* Icens was successfully installed.
* interval is installed from CRAN.
* You’re installing survivalTB from the correct path or GitHub URL.
