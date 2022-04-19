#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
#Dot Plot 
"""
import csv
import pandas as pd
COVID19=pd.read_csv("Shedding_output.csv")
openfile=csv.reader(COVID19)
print(COVID19)

#Column Names:
print(COVID19.columns)
COVID19_1=COVID19.dropna()
COVID19_2020=COVID19_1[COVID19_1['CollectDate'].str.contains("2020")]
COVID19_2021=COVID19_1[COVID19_1['CollectDate'].str.contains("2021")]
#'DOB', 'Sex', 'COVID', 'CollectDate', 'CTValue', 'Lineage', 'UniqID','Age', 'Age.bin'

#Cleveland Dot Plot - Violin for Gender All Years
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="ticks", color_codes=True)
sns.catplot(x="Sex",y="CTValue",kind="violin",data=COVID19_1)

#Swarm + Violin for Gender All Years
g=sns.catplot(x="Sex",y="CTValue",kind="violin",inner=None,data=COVID19_1)
sns.swarmplot(x="Sex",y="CTValue",color="k",size=2,data=COVID19_1,ax=g.ax)


#Cleveland Dot Plot - Violin for Gender 2020
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="ticks", color_codes=True)
sns.catplot(x="Sex",y="CTValue",kind="violin",data=COVID19_2020)

#Swarm + Violin for Gender for 2020
gh=sns.catplot(x="Sex",y="CTValue",kind="violin",inner=None,data=COVID19_2020)
sns.swarmplot(x="Sex",y="CTValue",color="k",size=2,data=COVID19_2020,ax=gh.ax)


#Cleveland Dot Plot - Violin for Gender 2021
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="ticks", color_codes=True)
sns.catplot(x="Sex",y="CTValue",kind="violin",data=COVID19_2021)

#Swarm + Violin for Gender for 2020
ghi=sns.catplot(x="Sex",y="CTValue",kind="violin",inner=None,data=COVID19_2021)
sns.swarmplot(x="Sex",y="CTValue",color="k",size=2,data=COVID19_2021,ax=ghi.ax)

#Cleveland Dot Plot - Violin for Gender 2021
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="ticks", color_codes=True)
sns.catplot(x="Sex",y="CTValue",kind="violin",data=COVID19_2021)

#Swarm + Violin for Gender for 2020
ghi=sns.catplot(x="Sex",y="CTValue",kind="violin",inner=None,data=COVID19_2021)
sns.swarmplot(x="Sex",y="CTValue",color="k",size=2,data=COVID19_2021,ax=ghi.ax)


#Cleveland Dot Plot - Violin for Age 
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="ticks", color_codes=True)
sns.catplot(x="Age.bin",y="CTValue",kind="violin",data=COVID19_1)

#Swarm + Violin for Age for 2021
ghij=sns.catplot(x="Age.bin",y="CTValue",kind="violin",inner=None,data=COVID19_1)
sns.swarmplot(x="Age.bin",y="CTValue",color="k",size=2,data=COVID19_1,ax=ghij.ax)

#Cleveland Dot Plot - Violin for Age 
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="ticks", color_codes=True)
sns.catplot(x="Age.bin",y="CTValue",kind="violin",data=COVID19_2020)

#Swarm + Violin for Age for 2021
ghijk=sns.catplot(x="Age.bin",y="CTValue",kind="violin",inner=None,data=COVID19_2020)
sns.swarmplot(x="Age.bin",y="CTValue",color="k",size=2,data=COVID19_2020,ax=ghijk.ax)

#Cleveland Dot Plot - Violin for Age 
import seaborn as sns
import matplotlib.pyplot as plt
sns.set_theme(style="ticks", color_codes=True)
sns.catplot(x="Age.bin",y="CTValue",kind="violin",data=COVID19_2021)

#Swarm + Violin for Age for 2021
ghijkl=sns.catplot(x="Age.bin",y="CTValue",kind="violin",inner=None,data=COVID19_2021)
sns.swarmplot(x="Age.bin",y="CTValue",color="k",size=2,data=COVID19_2021,ax=ghijkl.ax)
