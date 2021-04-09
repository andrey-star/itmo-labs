package com.example.animationapp

import android.animation.Animator
import android.animation.AnimatorSet
import android.animation.ValueAnimator
import android.content.Context
import android.content.res.TypedArray
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.RectF
import android.util.AttributeSet
import android.util.Log
import android.util.TypedValue
import android.view.View
import android.view.animation.PathInterpolator
import androidx.core.animation.doOnEnd
import androidx.core.animation.doOnRepeat
import kotlin.math.hypot
import kotlin.math.max
import kotlin.math.min

class LoaderDrawAnimationView @JvmOverloads constructor(
    context: Context, attrs: AttributeSet? = null, defStyleAttr: Int = 0
) : View(context, attrs, defStyleAttr) {

    private var circleRadius = dp(2.5f)

    private var circleScaleFactor = 1.3f
    private var paint = Paint(Paint.ANTI_ALIAS_FLAG).apply {
        color = 0xFFe1e3e6.toInt()
    }
    private val betweenCircles = dp(10f)

    private val rectLongSide = dp(22f)
    private var rectShortSide = dp(6f)
    private val rectHypot = hypot(rectLongSide, rectShortSide)
    private val rectF = RectF()

    private var elementsMargin = dp(16f)
    private val topMargin = dp(16f)
    private val leftMargin = dp(2f)
    private val desiredWidth =
        leftMargin * 2 + rectHypot + elementsMargin + rectShortSide + betweenCircles + rectShortSide * circleScaleFactor
    private val desiredHeight = 2 * topMargin + max(
        rectLongSide,
        rectShortSide + rectShortSide * circleScaleFactor + betweenCircles
    )
    private var circleTurn = 0

    private var animationDelay: Long
    private var animationLength: Long

    init {
        val a: TypedArray = context.obtainStyledAttributes(
            attrs, R.styleable.LoaderDrawAnimationView, defStyleAttr, 0
        )
        try {
            rectShortSide = dp(a.getFloat(R.styleable.LoaderDrawAnimationView_rectShortSide, 6f))
            circleRadius = dp(a.getFloat(R.styleable.LoaderDrawAnimationView_circleRadius, 2.5f))
            elementsMargin = dp(a.getFloat(R.styleable.LoaderDrawAnimationView_elementsMargin, 16f))
            animationDelay =
                a.getInt(R.styleable.LoaderDrawAnimationView_animationDelay, 1000).toLong()
            animationLength =
                a.getInt(R.styleable.LoaderDrawAnimationView_animationLength, 300).toLong()
            circleScaleFactor =
                a.getFloat(R.styleable.LoaderDrawAnimationView_circleScaleFactor, 1.3f)
            paint.color = a.getColor(R.styleable.LoaderDrawAnimationView_color, 0xFFe1e3e6.toInt())
        } finally {
            a.recycle()
        }
    }

    private var circleScale: Float = 1f
        set(value) {
            field = value
            invalidate()
        }

    private var rectRotation: Float = 0f
        set(value) {
            field = value
            invalidate()
        }


    private var animator: Animator? = null

    override fun onAttachedToWindow() {
        super.onAttachedToWindow()

        animator?.cancel()
        animator = AnimatorSet().apply {
            val rectRotateAnimator = ValueAnimator.ofFloat(0.0F, 180F).apply {
                addUpdateListener {
                    rectRotation = it.animatedValue as Float
                }
                startDelay = animationDelay
                duration = animationLength
            }
            val circleScaleAnimator = ValueAnimator.ofFloat(1f, circleScaleFactor).apply {
                repeatCount = 4
                this.doOnRepeat {
                    circleTurn++
                }
                addUpdateListener {
                    circleScale = it.animatedValue as Float
                }
                this.doOnEnd {
                    circleTurn = 0
                }
                duration = animationLength
            }

            interpolator = PathInterpolator(0.25F, 0.1F, 0.25F, 1F)
            playSequentially(rectRotateAnimator, circleScaleAnimator)
            doOnEnd {
                start()
            }
            start()
        }
    }

    override fun onMeasure(widthMeasureSpec: Int, heightMeasureSpec: Int) {
        setMeasuredDimension(
            getSize(widthMeasureSpec, desiredWidth.toInt()),
            getSize(heightMeasureSpec, desiredHeight.toInt())
        )
    }

    private fun drawDot(canvas: Canvas, translateX: Float, translateY: Float, curCircleTurn: Int) {
        val save = canvas.save()
        canvas.translate(translateX, translateY)
        if ((circleTurn == curCircleTurn && circleScale != circleScaleFactor)
            || (circleTurn == curCircleTurn + 1 && circleScale == circleScaleFactor)
        ) {
            canvas.scale(circleScale, circleScale, circleRadius, circleRadius)
        } else if (circleTurn == curCircleTurn + 1) {
            canvas.scale(
                1 + circleScaleFactor - circleScale,
                1 + circleScaleFactor - circleScale,
                circleRadius,
                circleRadius
            )
        }
        canvas.drawRoundRect(
            0f,
            0f,
            rectShortSide,
            rectShortSide,
            circleRadius,
            circleRadius,
            paint
        )
        canvas.restoreToCount(save)
    }

    private fun drawRect(canvas: Canvas, rotationX: Float, rotationY: Float, rectF: RectF) {
        val save = canvas.save()
        canvas.rotate(rectRotation, rotationX, rotationY)
        canvas.drawRoundRect(
            rectF, circleRadius, circleRadius, paint
        )
        canvas.restoreToCount(save)
    }

    override fun onDraw(canvas: Canvas) {
        super.onDraw(canvas)

        // Plus
        val horTop = topMargin + (rectLongSide - rectShortSide) / 2
        rectF.set(leftMargin, horTop, leftMargin + rectLongSide, horTop + rectShortSide)
        drawRect(
            canvas, leftMargin + rectLongSide / 2, horTop + rectShortSide / 2, rectF
        )

        val verLeft = leftMargin + (rectLongSide - rectShortSide) / 2
        rectF.set(
            verLeft, topMargin, verLeft + rectShortSide,
            topMargin + rectLongSide
        )
        drawRect(
            canvas, verLeft + rectShortSide / 2, topMargin + rectLongSide / 2, rectF
        )

        // Dots
        val dotsStart = leftMargin + elementsMargin + rectHypot
        drawDot(
            canvas,
            dotsStart + (betweenCircles + rectShortSide) / 2,
            topMargin, 0
        )
        drawDot(
            canvas,
            dotsStart + rectShortSide + betweenCircles,
            (rectShortSide + betweenCircles) / 2 + topMargin,
            1
        )
        drawDot(
            canvas,
            dotsStart + (betweenCircles + rectShortSide) / 2,
            rectShortSide + betweenCircles + topMargin,
            2
        )
        drawDot(canvas, dotsStart, (rectShortSide + betweenCircles) / 2 + topMargin, 3)

    }


    override fun onDetachedFromWindow() {
        super.onDetachedFromWindow()

        animator?.cancel()
        animator = null
    }


    private fun getSize(measureSpec: Int, desired: Int): Int {
        val mode = MeasureSpec.getMode(measureSpec)
        val size = MeasureSpec.getSize(measureSpec)
        return when (mode) {
            MeasureSpec.AT_MOST -> min(size, desired)
            MeasureSpec.EXACTLY -> size
            MeasureSpec.UNSPECIFIED -> desired
            else -> desired
        }
    }

    private fun dp(dp: Float): Float {
        return TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dp, resources.displayMetrics)
    }
}