package com.example.imagesapp

import android.content.Intent
import android.os.AsyncTask
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import android.widget.Toast
import androidx.recyclerview.widget.LinearLayoutManager
import kotlinx.android.synthetic.main.activity_main.*
import org.json.simple.JSONArray
import org.json.simple.JSONObject
import org.json.simple.parser.JSONParser
import java.io.IOException
import java.io.InputStreamReader
import java.lang.ref.WeakReference
import java.net.URL

class MainActivity : AppCompatActivity() {

    companion object {
        private const val TOKEN = "0c1f7e490c1f7e490c1f7e491e0c47880600c1f0c1f7e4957cfcfd44851d9514e791f82"
        private const val QUERY = "nature"
        private const val COUNT = 100
    }

    private var imagesList: List<Image>? = null

    private class ImagePreviewLoader(activity: MainActivity) :
        AsyncTask<String, Void, List<Image>>() {

        private val activityRef = WeakReference(activity)

        override fun doInBackground(vararg params: String): List<Image> {
            val imagesList = mutableListOf<Image>()
            val url =
                "https://api.vk.com/method/photos.search?q=${params[0]}&access_token=${params[1]}&v=5.102&count=${COUNT}"
            Log.i("connect", "Connecting to $url")
            try {
                InputStreamReader(
                    URL(url)
                        .openConnection()
                        .getInputStream()
                ).use {
                    val parser = JSONParser()
                    val root = parser.parse(it.readText()) as JSONObject
                    val response = root["response"] as JSONObject
                    val items = response["items"] as JSONArray
                    for (item in items) {
                        item as JSONObject
                        val text = item["text"] as String
                        val sizes = item["sizes"] as JSONArray
                        val size = sizes.last() as JSONObject
                        val imageUrl = size["url"] as String
                        imagesList.add(Image(text, imageUrl))
                    }
                }
            } catch (e: IOException) {
                Log.e("connect", "Connection failed: ${e.message}", e)
                e.printStackTrace()
            }
            return imagesList
        }

        override fun onPostExecute(result: List<Image>) {
            val activity = activityRef.get()
            activity?.onLoadCompleted(result)
        }
    }

    internal fun onLoadCompleted(result: List<Image>) {
        imagesList = result
        val viewManager = LinearLayoutManager(this@MainActivity)
        val myAdapter = ImageAdapter(result) {
            val intent = Intent(this@MainActivity, ImageActivity::class.java)
            intent.putExtra("url", it.url)
            startActivity(intent)
        }
        if (myAdapter.itemCount == 0) {
            Toast.makeText(this, "No results", Toast.LENGTH_LONG).show()
        }
        my_recycler_view.apply {
            layoutManager = viewManager
            adapter = myAdapter
        }
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
    }

    override fun onResume() {
        super.onResume()
        if (imagesList == null) {
            ImagePreviewLoader(this).execute(QUERY, TOKEN)
        }
    }

}
