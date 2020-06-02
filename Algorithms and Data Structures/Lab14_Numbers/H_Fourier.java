import java.io.*;
import java.math.BigInteger;
import java.util.Random;

public class H_Fourier {
	
	private static void test() throws FileNotFoundException {
		Random random = new Random(4);
		BigInteger a = new BigInteger(8000, random);
		BigInteger b = new BigInteger(8000, random);
		PrintWriter out = new PrintWriter(new File("test.in"));
		out.println(a);
		out.println(b);
		out.close();
	}
	
	public static void main(String[] args) throws IOException {
//		test();
//		System.exit(0);
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String a = in.readLine();
		String b = in.readLine();
		int[] arr = new int[a.length()];
		int[] brr = new int[b.length()];
		int aShift = a.charAt(0) == '-' ? 1 : 0;
		for (int i = 0; i < a.length() - aShift; i++) {
			arr[i] = a.charAt(i + aShift) - '0';
		}
		int bShift = b.charAt(0) == '-' ? 1 : 0;
		for (int i = 0; i < b.length() - bShift; i++) {
			brr[i] = b.charAt(i + bShift) - '0';
		}
		solve(arr, brr, (aShift ^ bShift) == 1);
		
	}
	
	private static void solve(int[] a, int[] b, boolean negative) {
		PrintWriter out = new PrintWriter(System.out);
		int maxLength = Math.max(a.length, b.length);
		int n = 1;
		while (n < maxLength) {
			n *= 2;
		}
		n *= 2;
		Complex[] aa = complexArray(n);
		Complex[] bb = complexArray(n);
		for (int i = 0; i < a.length; i++) {
			aa[i] = new Complex(a[i], 0);
		}
		for (int i = 0; i < b.length; i++) {
			bb[i] = new Complex(b[i], 0);
		}
		Complex w = new Complex(Math.cos(2 * Math.PI / n), Math.sin(2 * Math.PI / n));
		Complex[] ya = fft(aa, w);
		Complex[] yb = fft(bb, w);
		Complex[] yc = new Complex[ya.length];
		for (int i = 0; i < ya.length; i++) {
			yc[i] = ya[i].multiply(yb[i]);
		}
		long[] prod = reverseFft(yc, w);
		out.println((negative ? "-" : "") + convert(prod));
		out.close();
	}
	
	private static String convert(long[] a) {
		a = trim(a);
		long carry = 0;
		StringBuilder res = new StringBuilder();
		for (int i = a.length - 1; i >= 0; i--) {
			a[i] += carry;
			res.append(a[i] % 10);
			carry = a[i] / 10;
		}
		if (carry != 0) {
			res.append(carry);
		}
		final String s = res.reverse().toString();
		return s.isEmpty() ? "0" : s;
	}
	
	private static long[] trim(long[] a) {
		int i = a.length - 1;
		for (; i >= 0; i--) {
			if (a[i] != 0) {
				break;
			}
		}
		if (i == -1) {
			return new long[]{};
		}
		long[] b = new long[i + 1];
		if (i + 1 >= 0) {
			System.arraycopy(a, 0, b, 0, i + 1);
		}
		return b;
	}
	
	
	private static Complex[] complexArray(int n) {
		Complex[] a = new Complex[n];
		for (int i = 0; i < n; i++) {
			a[i] = new Complex(0, 0);
		}
		return a;
	}
	
	private static Complex[] fft(Complex[] a, Complex w) {
		int n = a.length;
		if (n == 1) {
			Complex[] res = new Complex[n];
			res[0] = a[0];
			return res;
		}
		Complex[] a0 = new Complex[n / 2];
		Complex[] a1 = new Complex[n / 2];
		for (int i = 0; i < n / 2; i++) {
			a0[i] = a[2 * i];
			a1[i] = a[2 * i + 1];
		}
		Complex wSquared = w.multiply(w);
		Complex[] y0 = fft(a0, wSquared);
		Complex[] y1 = fft(a1, wSquared);
		Complex[] y = new Complex[n];
		Complex wi = new Complex(1, 0);
		for (int i = 0; i < n; i++) {
			y[i] = y0[i % (n / 2)].add(wi.multiply(y1[i % (n / 2)]));
			wi = wi.multiply(w);
		}
		return y;
	}
	
	private static long[] reverseFft(Complex[] a, Complex w) {
		int n = a.length;
		Complex[] b = fft(a, w);
		for (int i = 1; i < n / 2; i++) {
			Complex temp = b[i];
			b[i] = b[n - i];
			b[n - i] = temp;
		}
		long[] c = new long[n];
		for (int i = 0; i < n; i++) {
			c[i] = Math.round(b[i].r / n);
		}
		return c;
	}
	
	private static class Complex {
		double r, i;
		
		public Complex(double r, double i) {
			this.r = r;
			this.i = i;
		}
		
		public Complex multiply(Complex other) {
			return new Complex(r * other.r - i * other.i, r * other.i + i * other.r);
		}
		
		public Complex add(Complex other) {
			return new Complex(r + other.r, i + other.i);
		}
		
		@Override
		public String toString() {
			return r + " + i * " + i;
		}
		
	}
}