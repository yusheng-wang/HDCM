# HDCM
Codes for the paper: “Efficient and Effective Calibration of Numerical Model Outputs Using Hierarchical Dynamic Models” by Y. Chen, X. Chang, B. Zhang, and H. Huang. 

There are two parts to our codes: 1) our two algorithms, the VB and the EnKs, are written into the stBase package in the R statistical environment; 2) a project entitled ``HDCMc'' in the Rstudio environment was built to reproduce all the results (e.g., figures and tables) in this work. 

The stBase package depends on the gpuR package that allows our codes to run on the GPU platform and hence further speeds up the calibration procedure. The source codes and its installation instructions for gpuR can now be found in a GitHub repository https://github.com/cdeterman/gpuR. One can also download the gpuR package here: https://cran.r-project.org/src/contrib/Archive/gpuR/.
