# COVID-19 Analysis 
Hello everyone!\
In this repository, we try to tackle three major problems resulting from the COVID-19 pandemic:\
1)Identify if age or biological sex impacts viral load as measured by qPCR.\
2)Investigate the impacts of seasonal change on viral load as measured by qPCR.\
3)Determine the effects of seasonal change on SARS-CoV-2 infection.\
The inspiration of these questions were based off this paper: https://www.medrxiv.org/content/10.1101/2020.10.01.20205096v2.full \
NOTE: Data will not be added to this repository as confidential. Please email for inquiries. 


# Pipeline
<img width="765" alt="Screen Shot 2022-04-19 at 3 45 16 AM" src="https://user-images.githubusercontent.com/101582963/163952130-306716ab-e23a-4962-b4c0-6803d3433b93.png">

# Scripts:
1)COVID GAM.R is a script used to create a Generalized Additive Models (GAM) between Positive Case Counts and Months. \
2)Covid Dot Plot.R is the R-Script used to create Dot Plots to observe the relation between Gender and Age with respect to CT Value.\
3)PythonScriptforCovid.py is a Python Script used to create Violin and Violin Swarm Plots to graph the same relation found in Script 2).\
https://seaborn.pydata.org/tutorial/categorical.html : Was used as a guideline to create these plots\
4)grpassign-pt1.R is a script used to create:
<pre>
a)Seasons of Viral Load and Gender via Boxplot for 2020 and 2021
b)Seasons of Viral Load and Age Bin via Boxplot for 2020 and 2021
c)Season of Viral Load and Age distributed in 10 year intervals via Boxplot
d)Viral Load and Age via Scatter Plot with Linear Fit
</pre>
5)lm group.R script is exploratory demographic analysis of the data
