# Machine Learning for Economic Analysis

Material for the course by [Damian Kozbur](https://www.econ.uzh.ch/en/people/faculty/kozbur.html) @UZH. The primary reference for introductory machine learning concepts and econometrics is Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani, “*An Introduction to Statistical Learning with Applications in R*”, freely available [here](https://faculty.marshall.usc.edu/gareth-james/ISL/ISLR%20Seventh%20Printing.pdf). For an advanced analysis of the topics, the recommended book is "*The Elements of Statistical Learning*" by Trevor Hastie, Robert Tibshirani, Jerome Friedman freely available [here](https://web.stanford.edu/~hastie/ElemStatLearn/).

The exercise sessions are entirely coded in [Python](https://www.python.org/downloads/) on Jupyter Notebooks. Recommended free resources are the documentation of the Python library [scikit-learn](https://scikit-learn.org/) and Bruce Hansen's [Econometrics book](https://www.ssc.wisc.edu/~bhansen/econometrics/). 

The code is publicly available on Github: https://github.com/matteocourthoud/Machine-Learning-for-Economic-Analysis



### Pre-requisites

Students should be familiar with the following concepts:

-  Matrix Algebra
   - Econometrics, appendix A.1-A.10
-  Conditional Expectation and Projection
 -  Econometrics, chapter 2.1-2.25
-  Large Sample Asymptotics
 -  Econometrics, chapter 6.1-6.5
-  Python basics
   - [Quant-Econ Tutorial](https://python.quantecon.org/index_learning_python.html)



### Exercise Sessions

1. [OLS Regression](https://matteocourthoud.github.io/course/ml-econ/1-regression/)
   - ISLR, chapter 3
   - ESL, chapter 3
   - Econometrics, chapters 3 and 4

2. [Instrumental variables](https://matteocourthoud.github.io/course/ml-econ/2-iv/)
   - Econometrics, chapter 12.1-12.12

3. [Nonparametric regression](https://matteocourthoud.github.io/course/ml-econ/3-nonparametric/)
   - ISLR, chapter 7
   - ESL, chapter 5
   - Econometrics, chapters 19 and 20

4. [Cross-validation](https://matteocourthoud.github.io/course/ml-econ/4-crossvalidation/)
   - ISLR, chapter 5
   - ESL, chapter 7

5. [Lasso and forward regression](https://matteocourthoud.github.io/course/ml-econ/5-lasso/)
   - ISLR, chapter 6
   - ESL, chapters 3 and 18
   - Econometrics, chapter 29.2-29.5

6. [Convexity and optimization](https://matteocourthoud.github.io/course/ml-econ/6-convexity/)

7. [Trees and forests](https://matteocourthoud.github.io/course/ml-econ/7-trees/)
   - ISLR, chapter 8
   - ESL, chapters 9, 10, 15, 16
   - Econometrics, chapter 29.6-29.9

8. [Neural Networks](https://matteocourthoud.github.io/course/ml-econ/8-neuralnets/)
   - ESL, chapter 11

9. [Post-double selection](https://matteocourthoud.github.io/course/ml-econ/9-postdoubleselection/)

  	- Econometrics, chapter 3.18
  	- Belloni, Chen, Chernozhukov, Hansen (2012)
  	- Belloni, Chernozhukov, Hansen (2014)
  	- Chernozhukov, Chetverikov, Demirer, Duflo, Hansen, Newey, Robins (2018)

10. [Unsupervised learning](https://matteocourthoud.github.io/course/ml-econ/10-unsupervised/)

  	- ISLR, chapter 10
  	- ESL, chapter 14




### Readings

- Athey, S., & Imbens, G. W. (n.d.). *Machine Learning Methods Economists Should Know About*. 62.
- Belloni, A., Chen, H., Chernozhukov, V., & Hansen, C. B. (2012). Sparse Models and Methods for Optimal Instruments With an Application to Eminent Domain. *Econometrica*, *80*(6), 2369–2429. https://doi.org/10.3982/ECTA9626
- Belloni, A., Chernozhukov, V., & Hansen, C. (2014). Inference on Treatment Effects after Selection among High-Dimensional Controls. *The Review of Economic Studies*, *81*(2), 608–650. https://doi.org/10.1093/restud/rdt044
- Chernozhukov, V., Chetverikov, D., Demirer, M., Duflo, E., Hansen, C., Newey, W., & Robins, J. (2018). Double/debiased machine learning for treatment and structural parameters. *The Econometrics Journal*, *21*(1), C1–C68. https://doi.org/10.1111/ectj.12097
- Franks, A., Miller, A., Bornn, L., & Goldsberry, K. (2015). Characterizing the spatial structure of defensive skill in professional basketball. *The Annals of Applied Statistics*, *9*(1), 94–121. https://doi.org/10.1214/14-AOAS799
- Gentzkow, M., Shapiro, J. M., & Taddy, M. (2019). Measuring Group Differences in High‐Dimensional Choices: Method and Application to Congressional Speech. *Econometrica*, *87*(4), 1307–1340. https://doi.org/10.3982/ECTA16566
- Kleinberg, J., Lakkaraju, H., Leskovec, J., Ludwig, J., & Mullainathan, S. (2017). Human Decisions and Machine Predictions. *The Quarterly Journal of Economics*. https://doi.org/10.1093/qje/qjx032
- Kleinberg, J., Ludwig, J., Mullainathan, S., & Obermeyer, Z. (2015). Prediction Policy Problems. *American Economic Review*, *105*(5), 491–495. https://doi.org/10.1257/aer.p20151023
- Mullainathan, S., & Spiess, J. (2017). Machine Learning: An Applied Econometric Approach. *Journal of Economic Perspectives*, *31*(2), 87–106. https://doi.org/10.1257/jep.31.2.87
- Wager, S., & Athey, S. (2018). Estimation and Inference of Heterogeneous Treatment Effects using Random Forests. *Journal of the American Statistical Association*, *113*(523), 1228–1242. https://doi.org/10.1080/01621459.2017.1319839



### Thanks

These exercise sessions heavily borrow from

- [Jordi Warmenhoven's](https://github.com/JWarmenhoven) git repo [ISLR-python](https://github.com/JWarmenhoven/ISLR-python)
- [Quant-Econ](https://quantecon.org/python-lectures/) website
- Prof. [Damian Kozbur](https://www.econ.uzh.ch/en/people/faculty/kozbur.html) past UZH [PhD Econometrics Class](https://matteocourthoud.github.io/econometrics/)
- Clark Science Center [Machine Learning couse](http://www.science.smith.edu/~jcrouser/SDS293/)
- UC Berkeley [Convex Optimization and Approximation class](https://ee227c.github.io/) by [Moritz Hardt](http://mrtz.org/)
- [Morvan Zhou](https://github.com/MorvanZhou/) and [Yunjey Choi](https://github.com/yunjey/) [pytorch](https://github.com/MorvanZhou/PyTorch-Tutorial) [tutorials](https://github.com/yunjey/pytorch-tutorial)
- [Daniel Godoy](https://medium.com/@dvgodoy) excellent article on Pytorch in [Medium's towardsdatascience](https://towardsdatascience.com/understanding-pytorch-with-an-example-a-step-by-step-tutorial-81fc5f8c4e8e)



### Contacts

If you have any issue or suggestion for the course, please feel free to [pull edits](https://github.com/matteocourthoud/Machine-Learning-for-Economic-Analysis-2020/pulls) or contact me [via mail](mailto:matteo.courthoud@uzh.ch). All feedback is greatly appreciated!