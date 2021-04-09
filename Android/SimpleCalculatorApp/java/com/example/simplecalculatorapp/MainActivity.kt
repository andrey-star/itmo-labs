package com.example.simplecalculatorapp

import android.graphics.Color
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.view.View
import android.widget.Button
import kotlinx.android.synthetic.main.activity_main.*
import net.objecthunter.exp4j.ExpressionBuilder
import java.lang.Exception

class MainActivity : AppCompatActivity() {

    private val errorColor = Color.RED
    private val regularColor = Color.GRAY

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
    }

    fun buttonClicked(view: View) {
        result.append((view as Button).text)
        result.setTextColor(regularColor)
    }

    fun evaluate(view: View) {
        try {
            result.text = ExpressionBuilder(result.text.toString()).build().evaluate().toString()
            result.setTextColor(regularColor)
        } catch (e: Exception) {
            result.setTextColor(errorColor)
        }

    }

    fun erase(view: View) {
        result.text = result.text.subSequence(0, result.text.length - 1)
        result.setTextColor(regularColor)
        if (result.text.isEmpty()) {
            result.text = "0"
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putString("res", result.text.toString())
        outState.putString("color", if (result.currentTextColor == errorColor) "red" else "gray")
        super.onSaveInstanceState(outState)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        result.text = savedInstanceState.getString("res")
        result.setTextColor(if (savedInstanceState.getString("color") == "red") errorColor else regularColor)
    }

}