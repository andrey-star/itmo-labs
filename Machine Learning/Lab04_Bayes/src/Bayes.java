import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.*;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Bayes {
	
	private static List<List<Entry>> dataset;
	
	public static void main(String[] args) throws IOException {
		new Bayes().run();
	}
	
	private Entry readEntry(String filename) {
		try {
			BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream(filename)));
			String email = in.readLine().replace("Subject: ", "");
			in.readLine();
			String body = in.readLine();
			if (!body.isEmpty() && !email.isEmpty()) {
				email += " ";
			}
			email += body;
			in.close();
			
			List<Integer> x = Arrays.stream(email.split(" ")).map(Integer::parseInt).collect(Collectors.toList());
			int y = filename.contains("legit") ? 0 : 1;
			return new Entry(x, y, filename);
		} catch (IOException e) {
			throw new UncheckedIOException(e);
		}
	}
	
	private List<List<Entry>> readDataset() throws IOException {
		FileNameVisitor fnv = new FileNameVisitor();
		Files.walkFileTree(Path.of("dataset"), fnv);
		return fnv.parts.stream().map(partFiles ->
				partFiles.stream()
						.map(this::readEntry)
						.collect(Collectors.toList()))
				.collect(Collectors.toList());
	}
	
	private void run() throws IOException {
		dataset = readDataset();
//		roc();
		lambdaAccPlot();
//		stressNAlpha(); // n = 2, alpha = 1e-20, acc = 0.98165
//		NaiveBayes nb = new NaiveBayes(2, 1e-20, new double[]{1, 1}, 2);
//		List<Measurement> m = crossValidate(nb);
//		System.out.println(m);
//		stressLambda(2, 1e-10); // n,alpha ^, lambda = 1e80, acc = 0.97155
//		System.out.println(fullTrain(nb));
//		List<Measurement> measurements = crossValidate(new NaiveBayes(2, 1e-20, new double[]{1e80, 1}, 2));
//		System.out.println("Accuracy: " + Measurement.accuracy(measurements));
	}
	
	private void roc() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("res/pred.txt")));
		List<List<Double>> res = in.lines()
				.map(line -> line.split(","))
				.map(s -> List.of(Double.parseDouble(s[0]), Double.parseDouble(s[1])))
				.collect(Collectors.toList());
		in.close();
		res.sort(Comparator.comparing((List<Double> l) -> l.get(0)).reversed());
		for (List<Double> re : res) {
			System.out.println(re.get(0));
		}
		int p = (int) res.stream().filter(l -> l.get(1) == 1.0).count();
		int n = (int) res.stream().filter(l -> l.get(1) == 0.0).count();
		List<Double> xs = new ArrayList<>();
		List<Double> ys = new ArrayList<>();
		xs.add(0.0);
		ys.add(0.0);
		double curX = 0;
		double curY = 0;
		for (List<Double> re : res) {
			double expected = re.get(1);
			if (expected == 1.0) {
				curY += 1.0 / p;
			} else if (expected == 0.0) {
				curX += 1.0 / n;
			}
 			xs.add(curX);
			ys.add(curY);
		}
		try {
			Plotter.linePlot("ROC, acc = 0.98165", "fpr", "tpr", xs, ys);
		} catch (PythonExecutionException e) {
			e.printStackTrace();
		}
	}
	
	private void lambdaAccPlot() throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("res/alpha,acc.txt")));
		List<List<Double>> res = in.lines()
				.map(line -> line.split(","))
				.map(s -> List.of(Double.parseDouble(s[0]), Double.parseDouble(s[1])))
				.collect(Collectors.toList());
		in.close();
		res.sort(Comparator.comparing((List<Double> l) -> l.get(0)));
		List<Double> xs = res.stream().map(l -> Math.log(l.get(0))).collect(Collectors.toList());
		List<Double> ys = res.stream().map(l -> l.get(1)).collect(Collectors.toList());
		try {
			Plotter.linePlot("", "log(lambda)", "acc", xs, ys);
		} catch (PythonExecutionException e) {
			e.printStackTrace();
		}
	}
	
	private void stressNAlpha() {
		List<NaiveBayes> nbs = new ArrayList<>();
		for (int n : List.of(1, 2, 3)) {
			for (double alpha : IntStream.range(0, 10)
					.boxed()
					.map(i -> Math.pow(10, -i))
					.collect(Collectors.toList())) {
				NaiveBayes nb = new NaiveBayes(2, alpha, new double[]{1, 1}, n);
				nbs.add(nb);
			}
		}
		Map<NbParams, List<Measurement>> res = parallelCV(nbs);
		System.out.println("___________");
		double maxAcc = 0;
		NbParams bestParams = new NbParams();
		for (NbParams params : res.keySet()) {
			double acc = Measurement.accuracy(res.get(params));
			System.out.printf("n: %d, alpha: %s, lambda: %s, cv-acc: %.2f%%%n",
					params.ngramms, params.alpha, Arrays.toString(params.lambda), acc * 100);
			if (maxAcc < acc) {
				maxAcc = acc;
				bestParams = params;
			}
		}
		System.out.println("___________");
		System.out.println("Best: " + bestParams);
	}
	
	private void stressLambda(int ngramm, double alpha) {
		List<NaiveBayes> nbs = new ArrayList<>();
		for (double l = 1; l <= 1e85; l *= 10000) {
			NaiveBayes nb = new NaiveBayes(2, alpha, new double[]{l, 1}, ngramm);
			nbs.add(nb);
		}
		Map<NbParams, List<Measurement>> res = parallelCV(nbs);
		System.out.println("___________");
		for (NbParams params : res.keySet()) {
			List<Measurement> m = res.get(params);
			double acc = Measurement.accuracy(m);
//			System.out.println("fp: " + fp + ", acc: " + String.format("%.2f", acc) + ". " + params);
			System.out.println(params.lambda[0] + "," + acc);
		}
	}
	
	private Map<NbParams, List<Measurement>> parallelCV(List<NaiveBayes> nbs) {
		ExecutorService executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
		Map<NbParams, Future<List<Measurement>>> f = new HashMap<>();
		for (NaiveBayes nb : nbs) {
			f.put(new NbParams(nb.ngrammCount, nb.alpha, nb.lambda), executorService.submit(() -> {
				System.out.printf("Started n: %d, alpha: %s, lambda: %s%n", nb.ngrammCount, nb.alpha, Arrays.toString(nb.lambda));
				return crossValidate(nb);
			}));
		}
		
		try {
			executorService.shutdown();
			executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
			return f.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> {
				try {
					return e.getValue().get();
				} catch (InterruptedException | ExecutionException err) {
					throw new AssertionError();
				}
			}));
		} catch (InterruptedException e) {
			throw new AssertionError();
		}
	}
	
	private Map<NbParams, Measurement> parallelLambda(List<NaiveBayes> nbs, List<Entry> dataset) {
		ExecutorService executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
		Map<NbParams, Future<Measurement>> f = new HashMap<>();
		for (NaiveBayes nb : nbs) {
			NbParams params = new NbParams(nb.ngrammCount, nb.alpha, nb.lambda);
			f.put(params, executorService.submit(() -> {
				System.out.printf("Started n: %d, alpha: %s, lambda: %s%n", nb.ngrammCount, nb.alpha, Arrays.toString(nb.lambda));
				Measurement measure = measure(nb, nb.train(dataset), dataset);
				System.out.println(measure.fp + " " + measure.accuracy());
				if (measure.fp == 0) {
					System.err.println(params);
				}
				return measure;
			}));
		}
		
		try {
			executorService.shutdown();
			executorService.awaitTermination(Long.MAX_VALUE, TimeUnit.MILLISECONDS);
			return f.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, e -> {
				try {
					return e.getValue().get();
				} catch (InterruptedException | ExecutionException err) {
					throw new AssertionError();
				}
			}));
		} catch (InterruptedException e) {
			throw new AssertionError();
		}
	}
	
	private List<Measurement> crossValidate(NaiveBayes nb) {
		List<Measurement> res = new ArrayList<>();
		for (int testIndex = 0; testIndex < dataset.size(); testIndex++) {
			List<Entry> train = new ArrayList<>();
			List<Entry> test = new ArrayList<>();
			for (int j = 0; j < dataset.size(); j++) {
				if (j == testIndex) {
					test.addAll(dataset.get(j));
				} else {
					train.addAll(dataset.get(j));
				}
			}
			var model = nb.train(train);
			res.add(measure(nb, model, test));
		}
		return res;
	}
	
	private Measurement measure(NaiveBayes nb, NaiveBayes.Model model, List<Entry> entries) {
		Measurement m = new Measurement();
		int i = 0;
		for (Entry entry : entries) {
			double[] predict = nb.predict(entry.x, model);
			int expected = entry.y;
			int actual = predict[0] > predict[1] ? 0 : 1;
			if (expected == 1) {
				m.p++;
				if (actual == 1) {
					m.tp++;
				} else {
					m.fn++;
				}
			} else if (expected == 0) {
				m.n++;
				if (actual == 0) {
					m.tn++;
				} else {
					m.fp++;
				}
			} else {
				throw new AssertionError();
			}
		}
		
		return m;
	}
	
	private static class FileNameVisitor extends SimpleFileVisitor<Path> {
		
		private final List<Set<String>> parts = new ArrayList<>();
		
		@Override
		public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) {
			parts.get(parts.size() - 1).add(file.toString());
			return FileVisitResult.CONTINUE;
		}
		
		@Override
		public FileVisitResult preVisitDirectory(Path dir, BasicFileAttributes attrs) {
			if (dir.getFileName().toString().startsWith("part")) {
				parts.add(new HashSet<>());
			}
			return FileVisitResult.CONTINUE;
		}
	}
	
	private static class Measurement {
		int p, n, tp, tn, fp, fn;
		
		public Measurement() {
		}
		
		public Measurement(int p, int n, int tp, int tn, int fp, int fn) {
			this.p = p;
			this.n = n;
			this.tp = tp;
			this.tn = tn;
			this.fp = fp;
			this.fn = fn;
		}
		
		static double accuracy(List<Measurement> ms) {
			return ms.stream().map(Measurement::accuracy).reduce(0.0, Double::sum) / dataset.size();
		}
		
		double recall() {
			return 1.0 * tp / p;
		}
		
		double specificity() {
			return 1.0 * tn / n;
		}
		
		double precision() {
			return 1.0 * tp / (tp + fp);
		}
		
		double accuracy() {
			return 1.0 * (tp + tn) / (p + n);
		}
	}
	
	private static class NbParams {
		int ngramms;
		double alpha;
		double[] lambda;
		
		public NbParams() {
		}
		
		public NbParams(int ngramms, double alpha, double[] lambda) {
			this.ngramms = ngramms;
			this.alpha = alpha;
			this.lambda = lambda;
		}
		
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof NbParams)) return false;
			NbParams param = (NbParams) o;
			return ngramms == param.ngramms &&
					Double.compare(param.alpha, alpha) == 0 &&
					Arrays.equals(lambda, param.lambda);
		}
		
		@Override
		public int hashCode() {
			int result = Objects.hash(ngramms, alpha);
			result = 31 * result + Arrays.hashCode(lambda);
			return result;
		}
		
		@Override
		public String toString() {
			return "ngramms: " + ngramms + ", alpha: " + alpha + ", lambda: " + Arrays.toString(lambda);
		}
	}
	
}
