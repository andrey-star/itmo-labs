package com.example.animationapp

import android.animation.Animator
import android.animation.ValueAnimator
import android.content.Context
import android.util.AttributeSet
import android.view.animation.LinearInterpolator
import android.widget.TextView


class AnimatedTextView @JvmOverloads constructor(
    context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0
) : TextView(context, attrs, defStyleAttr) {

    private val textAlphaAnimator = ValueAnimator.ofFloat(1f, 0.2f, 1f).apply {
        repeatCount = ValueAnimator.INFINITE
        addUpdateListener {
            alpha = it.animatedValue as Float
        }
        interpolator = LinearInterpolator()
        duration = 1000L
        start()
    }

    private var animator: Animator? = null

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()

        animator?.cancel()
        animator = textAlphaAnimator
    }

    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()

        animator?.cancel()
        animator = null
    }

}