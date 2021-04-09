package com.example.weatherapp

import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import android.widget.Switch
import android.view.View
import androidx.appcompat.app.AppCompatDelegate
import kotlinx.android.synthetic.main.activity_main.*
import retrofit2.Call
import retrofit2.Callback
import retrofit2.Response
private const val LOG_TAG = "Weather API"

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)

        val sw: Switch = findViewById(R.id.dayNightSwitch)
        sw.setOnCheckedChangeListener { _, isChecked ->
            if (isChecked) {
                delegate.setLocalNightMode(AppCompatDelegate.MODE_NIGHT_YES)
                nightModeImage.visibility = View.INVISIBLE
                lightModeImage.visibility = View.VISIBLE
            } else {
                delegate.setLocalNightMode(AppCompatDelegate.MODE_NIGHT_NO)
                lightModeImage.visibility = View.INVISIBLE
                nightModeImage.visibility = View.VISIBLE
            }
        }
        run()
    }

    private var call: Call<DailyForecasts>? = null

    private fun run() {
        call = MyApp.app.weatherApi.getRepos("294021", BuildConfig.ApiKey)
        call?.enqueue(object : Callback<DailyForecasts> {
            override fun onFailure(call: Call<DailyForecasts>, t: Throwable) {
                Log.e(LOG_TAG, "Failed with", t)
            }

            override fun onResponse(
                call: Call<DailyForecasts>,
                response: Response<DailyForecasts>
            ) {
                Log.d(LOG_TAG, response.raw().request.url.toString())
                val body = response.body()
                Log.d(LOG_TAG, "Finished with ${response.code()}, body: $body")
                if (body != null) {
                    todayTemp.text = getString(
                        R.string.temp_template,
                        body.dailyForecasts[0].temperature.min.value
                    )
                    textView9.text = getString(
                        R.string.temp_template,
                        body.dailyForecasts[1].temperature.min.value
                    )
                    textView12.text = getString(
                        R.string.temp_template,
                        body.dailyForecasts[2].temperature.min.value
                    )
                    textView16.text = getString(
                        R.string.temp_template,
                        body.dailyForecasts[3].temperature.min.value
                    )
                    textView14.text = getString(
                        R.string.temp_template,
                        body.dailyForecasts[4].temperature.min.value
                    )
                }
            }
        })
    }
}
