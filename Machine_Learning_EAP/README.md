## This is a repo for a project "Prediciting the Cross-Section of Expected Stock Returns using Machine Learning".

The objective of this project is to achieve higher predictive accuracy in estimating expected returns of the cross-section of public US firms. I use modern machine learning approaches and show that they outperform commonly used in academia linear regression by the factor of 5-6. 

The repo contains the code, which build predictive models. For now, this is only the code for v1 of the project. v1 used semi-random train/test split, which did not fully account for time dimension. Hence, the results suffered from possible forward-looking bias. v2 utilizes time-based split with rolling train/validation/test split. Currently, v2 is fully hosted and VC-ed at Kaggle and is pushed to https://github.com/MykolaPinchuk/KaggleProjects/blob/main/kp-103-mleap.ipynb. 
