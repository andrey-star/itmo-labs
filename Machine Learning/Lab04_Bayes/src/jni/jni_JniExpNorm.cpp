#include <iostream>
#include "jni_JniExpNorm.h"
#include <cmath>

/*
 * Class:     jni_JniExpNorm
 * Method:    expNormA
 * Signature: (DD)D
 */
JNIEXPORT jdouble JNICALL Java_jni_JniExpNorm_expNormA
  (JNIEnv *, jobject, jdouble a, jdouble b) {
  long double aa = a;
  long double bb = b;
  aa = std::exp(aa);
  bb = std::exp(bb);
  long double sum = aa + bb;
  aa = aa / sum;
  bb = bb / sum;
  return aa;
}

/*
 * Class:     jni_JniExpNorm
 * Method:    expNormB
 * Signature: (DD)D
 */
JNIEXPORT jdouble JNICALL Java_jni_JniExpNorm_expNormB
  (JNIEnv *, jobject, jdouble a, jdouble b) {
  long double aa = a;
  long double bb = b;
  aa = std::exp(aa);
  bb = std::exp(bb);
  long double sum = aa + bb;
  aa = aa / sum;
  bb = bb / sum;
  return bb;
}

