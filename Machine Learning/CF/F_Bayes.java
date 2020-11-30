import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.*;
import java.util.function.BiFunction;

public class F_Bayes {
	
	private int k, n;
	private double alpha;
	private double[] lambda;
	private BufferedReader in;
	
	public static void main(String[] args) throws IOException {
		new F_Bayes().run();
	}
	
	private int getN() throws IOException {
		return Integer.parseInt(in.readLine());
	}
	
	private void readDataset(int n, List<Set<String>> x, List<Integer> y) throws IOException {
		for (int i = 0; i < n; i++) {
			String[] line = in.readLine().trim().split(" +");
			if (y != null) {
				int c = Integer.parseInt(line[0]) - 1;
				y.add(c);
			}
			int l = Integer.parseInt(line[y == null ? 0 : 1]);
			x.add(new HashSet<>());
			for (int j = 0; j < l; j++) {
				String word = line[j + (y == null ? 1 : 2)];
				x.get(i).add(word);
			}
		}
	}
	
	private void run() throws IOException {
		in = new BufferedReader(new InputStreamReader(System.in));
		k = getN();
		lambda = new double[k];
		String[] line = in.readLine().trim().split(" +");
		for (int i = 0; i < k; i++) {
			lambda[i] = Integer.parseInt(line[i]);
		}
		
		alpha = getN();
		
		n = getN();
		// max 1 word appearance for one object
		List<Set<String>> x_train = new ArrayList<>();
		List<Integer> y_train = new ArrayList<>();
		readDataset(n, x_train, y_train);
		
		int m = getN();
		List<Set<String>> x_test = new ArrayList<>();
		readDataset(m, x_test, null);
		in.close();
		
		Model model = train(x_train, y_train);
		for (Set<String> x : x_test) {
			double[] predict = predict(x, model);
			for (double v : predict) {
				System.out.printf(Locale.US, "%.10f ", v);
			}
			System.out.println();
		}
	}
	
	private double[] predict(Set<String> x, Model model) {
		double[] classProb = new double[k];
		for (int c = 0; c < k; c++) {
			if (model.objectsOfClass.get(c) == 0) {
				continue;
			}
			double curClassProb = Math.log(lambda[c] * model.classProb.get(c));
			for (String word : model.words) {
				double wordProb = model.getWordProb(word, c);
				curClassProb += Math.log(x.contains(word) ? wordProb : 1 - wordProb);
			}
			classProb[c] = curClassProb;
		}
		double totalScore = 0;
		for (int c = 0; c < k; c++) {
			if (model.objectsOfClass.get(c) != 0) {
				classProb[c] = Math.exp(classProb[c]);
				totalScore += classProb[c];
			}
		}
		for (int c = 0; c < k; c++) {
			classProb[c] /= totalScore;
		}
		return classProb;
	}
	
	private Model train(List<Set<String>> xs, List<Integer> ys) {
		List<Map<String, Integer>> wordFreqByClass = new ArrayList<>();
		List<Integer> objectsOfClass = new ArrayList<>();
		for (int c = 0; c < k; c++) {
			wordFreqByClass.add(new HashMap<>());
			objectsOfClass.add(0);
		}
		Set<String> words = new HashSet<>();
		for (int i = 0; i < xs.size(); i++) {
			Set<String> x = xs.get(i);
			words.addAll(x);
			int y = ys.get(i);
			for (String word : x) {
				Map<String, Integer> classFreq = wordFreqByClass.get(y);
				classFreq.putIfAbsent(word, 0);
				classFreq.put(word, classFreq.get(word) + 1);
			}
			objectsOfClass.set(y, objectsOfClass.get(y) + 1);
		}
		
		List<Double> classProb = new ArrayList<>();
		for (int c = 0; c < k; c++) {
			classProb.add(1.0 * objectsOfClass.get(c) / xs.size());
		}
		return new Model(classProb,
				(word, c) -> (wordFreqByClass.get(c).getOrDefault(word, 0) + alpha) /
						(objectsOfClass.get(c) + alpha * 2),
				objectsOfClass,
				words);
	}
	
	public static class Model {
		List<Double> classProb;
		BiFunction<String, Integer, Double> wordProb;
		List<Integer> objectsOfClass;
		Set<String> words;
		
		public Model(List<Double> classProb, BiFunction<String, Integer, Double> wordProb, List<Integer> objectsOfClass, Set<String> words) {
			this.classProb = classProb;
			this.wordProb = wordProb;
			this.objectsOfClass = objectsOfClass;
			this.words = words;
		}
		
		public double getWordProb(String word, int c) {
			return wordProb.apply(word, c);
		}
		
	}
	
}
