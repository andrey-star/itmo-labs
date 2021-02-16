package jni;

public class JniExpNorm {
	
	static {
		System.load("C:\\Users\\fastr\\IdeaProjects\\MachineLearning\\Lab04_Bayes\\src\\jni\\jniExpNorm.so");
	}
	
	public native double expNormA(double a, double b);
	public native double expNormB(double a, double b);
	
}
