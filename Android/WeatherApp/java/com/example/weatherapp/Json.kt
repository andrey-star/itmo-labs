package com.example.weatherapp

import com.squareup.moshi.Json

data class DailyForecasts(
    @Json(name = "DailyForecasts") val dailyForecasts: List<DailyForecast>
)

data class DailyForecast(
    @Json(name = "Temperature") val temperature: Temperature
)

data class Temperature(
    @Json(name = "Minimum") val min: Minimum,
    @Json(name = "Maximum") val max: Maximum
)

data class Minimum(
    @Json(name = "Value") val value: Double
)

data class Maximum(
    @Json(name = "Value") val value: Double
)


