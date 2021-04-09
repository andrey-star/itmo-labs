package com.example.weatherapp

import com.squareup.moshi.Moshi
import com.squareup.moshi.kotlin.reflect.KotlinJsonAdapterFactory
import okhttp3.OkHttpClient
import retrofit2.Call
import retrofit2.Retrofit
import retrofit2.converter.moshi.MoshiConverterFactory
import retrofit2.http.*

interface WeatherApi {
    @GET("forecasts/v1/daily/5day/{location}")
    fun getRepos(@Path("location") location: String,
                 @Query("apikey") apikey: String,
                 @Query("metric") metric: Boolean = true): Call<DailyForecasts>
}

fun createWeatherApi(): WeatherApi {
    val client = OkHttpClient()
    val moshi = Moshi.Builder()
        .add(KotlinJsonAdapterFactory())
        .build()
    val retrofit = Retrofit.Builder()
        .client(client)
        .addConverterFactory(MoshiConverterFactory.create(moshi))
        .baseUrl("http://dataservice.accuweather.com/")
        .build()
    val api = retrofit.create(WeatherApi::class.java)
    return api
}