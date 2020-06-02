import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.math.BigInteger;

public class J_RsaFactoring {
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" +");
		int k = Integer.parseInt(line[1]);
		String nHex = in.readLine();
		solve(k, nHex);
	}
	
	private static void solve(int k, String nHex) {
		PrintWriter out = new PrintWriter(System.out);
		BigInteger n = new BigInteger(nHex, 16);
		if (k == 2) {
			BigInteger[] factors = getFactors2(n);
			out.println(factors[0].toString(16));
			out.println(factors[1].toString(16));
		} else {
			BigInteger[][] factors = getFactors4(n);
			BigInteger p1q1 = factors[0][0];
			BigInteger p2q2 = factors[0][1];
			BigInteger p1q2 = factors[1][0];
			BigInteger p2q1 = factors[1][1];
			out.println(p1q1.gcd(p1q2).toString(16));
			out.println(p2q1.gcd(p2q2).toString(16));
			out.println(p1q1.gcd(p2q1).toString(16));
			out.println(p1q2.gcd(p2q2).toString(16));
		}
		out.close();
	}
	
	private static BigInteger[] getFactors2(BigInteger n) {
		BigInteger m = sqrt(n);
		while (true) {
			if (m.multiply(m).compareTo(n) < 0) {
				m = m.add(BigInteger.ONE);
			}
			BigInteger d = m.multiply(m).subtract(n);
			BigInteger dRoot = sqrt(d);
			if (dRoot.multiply(dRoot).equals(d)) {
				return new BigInteger[]{m.subtract(dRoot), m.add(dRoot)};
			}
			m = m.add(BigInteger.ONE);
		}
	}
	
	private static BigInteger[][] getFactors4(BigInteger n) {
		BigInteger[] firstPair = null;
		BigInteger m = sqrt(n);
		while (true) {
			if (m.multiply(m).compareTo(n) < 0) {
				m = m.add(BigInteger.ONE);
			}
			BigInteger d = m.multiply(m).subtract(n);
			BigInteger dRoot = sqrt(d);
			if (dRoot.multiply(dRoot).equals(d)) {
				if (firstPair == null) {
					firstPair = new BigInteger[]{m.subtract(dRoot), m.add(dRoot)};
				} else {
					return new BigInteger[][]{firstPair, {m.subtract(dRoot), m.add(dRoot)}};
				}
			}
			m = m.add(BigInteger.ONE);
		}
	}
	
	public static BigInteger sqrt(BigInteger x) {
		BigInteger div = BigInteger.ZERO.setBit(x.bitLength() / 2);
		BigInteger div2 = div;
		while (true) {
			BigInteger y = div.add(x.divide(div)).shiftRight(1);
			if (y.equals(div) || y.equals(div2)) {
				return y;
			}
			div2 = div;
			div = y;
		}
	}
	
}