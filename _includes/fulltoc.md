PART I: **BASICS**  

1\. **Introduction**  
Point patterns  
Statistical methodology for point patterns  
About this book

2\. **Software Essentials**  
Introduction to R  
Packages for R   
Introduction to spatstat   
Getting started with spatstat   
FAQ

3\. **Collecting and Handling Point Pattern Data** ([download pdf](sample-chapters/chapter03.pdf))  
Surveys and experiments  
Data handling  
Entering point pattern data into spatstat  
Data errors and quirks  
Windows in spatstat  
Pixel images in spatstat  
Line segment patterns  
Collections of objects  
Interactive data entry in spatstat  
Reading GIS file formats  
FAQ

4\. **Inspecting and Exploring Data**  
Plotting  
Manipulating point patterns and windows  
Exploring images  
Using line segment patterns  
Tessellations  
FAQ

5\. **Point Process Methods**  
Motivation  
Basic definitions   
Complete spatial randomness  
Inhomogeneous Poisson process  
A menagerie of models  
Fundamental issues  
Goals of analysis  

PART II: **EXPLORATORY DATA ANALYSIS**  

6\. **Intensity**  
Introduction  
Estimating homogeneous intensity  
Technical definition 
Quadrat counting  
Smoothing estimation of intensity function  
Investigating dependence of intensity on a covariate  
Formal tests of (non-)dependence on a covariate  
Hot spots, clusters, and local features  
Kernel smoothing of marks  
FAQ

7\. **Correlation** ([download pdf](sample-chapters/chapter07.pdf))  
Introduction  
Manual methods  
The *K*-function  
Edge corrections for the *K*-function  
Function objects in spatstat  
The pair correlation function  
Standard errors and confidence intervals  
Testing whether a pattern is completely random  
Detecting anisotropy  
Adjusting for inhomogeneity  
Local indicators of spatial association  
Third- and higher-order summary statistics  
Theory  
FAQ

8\. **Spacing**  
Introduction  
Basic methods  
Nearest-neighbour function *G*and empty-space function *F*  
Confidence intervals and simulation envelopes  
Empty-space hazard  
*J*-function  
Inhomogeneous *F*-, *G*- and *J*-functions  
Anisotropy and the nearest-neighbour orientation  
Empty-space distance for a spatial pattern  
Distance from a point pattern to another spatial pattern  
Theory for edge corrections  
Palm distribution  
FAQ

PART III: **STATISTICAL INFERENCE**  

9\. **Poisson Models** ([download pdf](sample-chapters/chapter09.pdf))  
Introduction  
Poisson point process models  
Fitting Poisson models in spatstat  
Statistical inference for Poisson models  
Alternative fitting methods  
More flexible models  
Theory  
Coarse quadrature approximation  
Fine pixel approximation  
Conditional logistic regression  
Approximate Bayesian inference  
Non-loglinear models  
Local likelihood  
FAQ

10\. **Hypothesis Tests and Simulation Envelopes**  
Introduction  
Concepts and terminology  
Testing for a covariate effect in a parametric model  
Quadrat counting tests  
Tests based on the cumulative distribution function  
Monte Carlo tests  
Monte Carlo tests based on summary functions  
Envelopes in spatstat  
Other presentations of envelope tests  
Dao-Genton test and envelopes  
Power of tests based on summary functions  
FAQ

11\. **Model Validation**  
Overview of validation techniques  
Relative intensity  
Residuals for Poisson processes  
Partial residual plots  
Added variable plots  
Validating the independence assumption  
Leverage and influence  
Theory for leverage and influence  
FAQ

12\. **Cluster and Cox Models**   
Introduction  
Cox processes  
Cluster processes  
Fitting Cox and cluster models to data  
Locally fitted models  
Theory*  
*FAQ

13\. **Gibbs Models**  
Introduction  
Conditional intensity  
Key concepts  
Statistical insights  
Fitting Gibbs models to data  
Pairwise interaction models  
Higher-order interactions  
Hybrids of Gibbs models  
Simulation  
Goodness-of-fit and validation for fitted Gibbs models  
Locally fitted models  
Theory: Gibbs processes  
Theory: Fitting Gibbs models  
Determinantal point processes  
FAQ

14\. **Patterns of Several Types of Points**  
Introduction  
Methodological issues  
Handling multitype point pattern data  
Exploratory analysis of intensity  
Multitype Poisson models  
Correlation and spacing  
Tests of randomness and independence  
Multitype Gibbs models  
Hierarchical interactions  
Multitype Cox and cluster processes  
Other multitype processes  
Theory  
FAQ

PART IV: **ADDITIONAL STRUCTURE**  

15\. **Higher-Dimensional Spaces and Marks**  
Introduction  
Point patterns with numerical or multidimensional marks  
Three-dimensional point patterns  
Point patterns with any kinds of marks and coordinates  
FAQ

16\. **Replicated Point Patterns and Designed Experiments**  
Introduction  
Methodology  
Lists of objects  
Hyperframes  
Computing with hyperframes  
Replicated point pattern datasets in spatstat  
Exploratory data analysis  
Analysing summary functions from replicated patterns  
Poisson models  
Gibbs models  
Model validation  
Theory  
FAQ

17\. **Point Patterns on a Linear Network**  
Introduction  
Network geometry  
Data handling  
Intensity  
Poisson models  
Intensity on a tree  
Pair correlation function  
*K*-function  
FAQ
