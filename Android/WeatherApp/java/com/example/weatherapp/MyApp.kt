package com.example.weatherapp

import android.app.Application

class MyApp : Application() {
    lateinit var weatherApi: WeatherApi
        private set

    override fun onCreate() {
        super.onCreate()
        val api = createWeatherApi()
        weatherApi = api
        app = this
    }

    companion object {
        lateinit var app: MyApp
            private set
    }
}