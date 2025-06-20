🍺 Beer Sales Forecasting
A time series analysis and forecasting project using Holt-Winters exponential smoothing and ARIMA models to predict quarterly beer sales for the next 2 years. Includes complete modeling workflow, seasonal decomposition, stationarity checks, model validation, and  diagnostics.

👩‍💻 Author
Ajuna P John
Assignment Type: Time Series Forecasting

🎯 Objective
To model and forecast beer sales for the next 8 quarters using historical quarterly data over 18 years. The analysis includes:

Trend and seasonality identification

Data stationarization

Holt-Winters and ARIMA model development

Forecast evaluation and comparison

📂 Dataset Overview
Data: beer.csv (loaded as beer_timeseries)

Variable: OzBeer (Beer sales quantity)

Observations: 72 (quarterly data over 18 years)

Assumed Period: Q1 2003 – Q4 2020

📊 Time Series Components Identified
Trend: Clear upward trend in beer sales over time

Seasonality: Strong quarterly pattern (sales peak in Q4)

Cycle: Minor cyclic variation

Additive Model Assumed due to constant seasonal variation over time

🧪 Data Preparation and Stationarity
Used ADF Test to confirm non-stationarity

Applied:

Seasonal adjustment via decomposition

Differencing to achieve stationarity

Final model confirmed stationary with:

ADF p-value < 0.05

Random, normally distributed residuals

📈 Forecasting Models
1️⃣ Holt-Winters Additive Model
Model: ETS(A,A,A)

Parameters:

α (level) = 0.044

β (trend) = 0.044

γ (seasonal) = 0.0002

MAPE (Test): 2.43%

Residuals: Independently distributed (Box-Pierce p = 0.32)

2️⃣ ARIMA Model
Model: ARIMA(0,1,2)(0,1,1)[4]

Found using auto.arima()

MAPE (Test): 0.37%

Residuals: Normal and uncorrelated (Shapiro-Wilk p = 0.99, Box-Pierce p = 0.82)

📈 Forecast Visualization
Both models effectively captured:

Seasonal peaks in Q4 (Oct–Dec)

Dips in Q2 and Q3 (Apr–Sep)

Reliable prediction intervals (80% & 95%)

🔍 Best Model: ARIMA, due to lower AIC and MAPE

💡 Key Insights
Beer sales are highly seasonal, peaking in warm months (Q4 in Australia).

ARIMA outperformed Holt-Winters in forecast accuracy and residual diagnostics.

The seasonal pattern remains consistent over years, suggesting strong climate influence on sales behavior.

🧰 Tech Stack
Language: R

Libraries: forecast, tseries, astsa, fpp2, TSA, ggplot2, stats
