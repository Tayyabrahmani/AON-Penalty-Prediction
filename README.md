# Apollo Choice Modelling for Penalty Shot Prediction
This repository contains code for penalty shot prediction using Apollo choice modeling and a web-based R Shiny app for the user interface.

## Overview
Penalty shot prediction is an essential aspect of soccer analytics. In this repository, we utilize the Apollo choice modeling package to create a model that predicts the outcome of penalty shots based on various factors. The model takes into account player information, shot locations, goalkeeper behavior, and other relevant features to estimate the probability of a successful penalty shot.

## Contents
1. **SixAlt.xlsx**: Contains the dataset used for modeling penalty shots. The dataset includes information on player IDs, shot locations, shot outcomes among others
2. **Model_fixed_effects.R**: This script includes code to build the Apollo choice model using the penalty shot dataset.
forecast_function.R: Contains the forecast function in R
3. **utils_func.R**: Contains certain utility functions useful in the script
4. **Prediction_app/app.R**: This script contains the code to create the interactive R Shiny app.

## How to Use
1. Clone the repository to your local machine using the following command:
```
  git clone https://github.com/your-username/penalty-shot-prediction.git
```
2. Open RStudio or your preferred R development environment.
3. Install the required R packages if not already installed:
 ```
   install.packages(c("apollo", "shiny", "tidyverse", "fastDummies", "readxl", "dplyr", "shinyjs", "shinythemes"))
 ```
4. Run the **Model_fixed_effects.R** to build the Apollo choice model using the provided dataset.
5. After the model is built, run the **app.R** to launch the R Shiny app.
6. The Shiny app will provide an interactive interface where you can input player information and shot locations to predict the outcome of a penalty shot.

## Where to Find the App
The R Shiny app can also be accessed online at [this link](https://tayyabrahmani.shinyapps.io/Penalty_Prediction/).

## Contributing
We welcome contributions to improve the penalty shot prediction model and the Shiny app. Feel free to submit issues, open pull requests, or suggest enhancements.

## Credits
This project was developed by Tayyab, Nallaperumal, Kanthi and Diksha. If you have any questions or need further information, please feel free to contact me.

Enjoy predicting penalty shots like never before with Apollo choice modeling and the R Shiny app!
