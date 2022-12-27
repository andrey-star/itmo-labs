import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Scanner;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Ba2b extends AbstractTask {
	
	public Ba2b() {
		super("ba2b");
	}
	
	public static void main(String[] args) throws IOException {
		new Ba2b().test();
	}
	
	public void test(Path test) throws FileNotFoundException {
		System.out.println(test.toFile());
		Scanner in = new Scanner(new InputStreamReader(new FileInputStream(test.toFile())));
		int k = in.nextInt();
		List<String> dna = new ArrayList<>();
		while (in.hasNext()) {
			dna.add(in.next());
		}
		System.out.println(solve(k, dna));
	}
	
	private String solve(int k, List<String> dna) {
		return kmers(k, dna).min(Comparator.comparing(Kmer::getDist)).get().value;
	}
	
	private Stream<Kmer> kmers(int k, List<String> dna) {
		return IntStream.range(0, (int) Math.pow(4, k)).mapToObj(i -> {
			String fourPadded = ("0".repeat(k) + Integer.toString(i, 4));
			String four = fourPadded.substring(fourPadded.length() - k);
			String kmer = four.replace("0", "A")
					.replace("1", "C")
					.replace("2", "G")
					.replace("3", "T");
			return new Kmer(kmer, dna);
		});
	}
	
	private static class Kmer {
		String value;
		int dist;
		
		public Kmer(String value, List<String> dna) {
			this.value = value;
			this.dist = dist(value, dna);
		}
		
		public int dist(String kmer, List<String> dna) {
			return dna.stream().mapToInt(dnai -> dist(kmer, dnai)).sum();
		}
		
		public int dist(String kmer, String text) {
			int min = Integer.MAX_VALUE;
			for (int i = 0; i <= text.length() - kmer.length(); i++) {
				int ham = hamming(kmer, text.substring(i, i + kmer.length()));
				if (ham < min) {
					min = ham;
				}
			}
			return min;
		}
		
		public int hamming(String a, String b) {
			int res = 0;
			for (int i = 0; i < a.length(); i++) {
				if (a.charAt(i) != b.charAt(i)) {
					res++;
				}
			}
			return res;
		}
		
		public int getDist() {
			return dist;
		}
	}
	
}
