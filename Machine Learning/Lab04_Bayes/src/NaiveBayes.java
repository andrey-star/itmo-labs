import jni.JniExpNorm;

import java.util.*;
import java.util.function.BiFunction;

public class NaiveBayes {
	
	
	public final int k;
	public final int ngrammCount;
	public final double alpha;
	public final double[] lambda;
	
	public NaiveBayes(int k, double alpha, double[] lambda, int ngrammCount) {
		this.k = k;
		this.alpha = alpha;
		this.lambda = lambda;
		this.ngrammCount = ngrammCount;
	}
	
	public Model train(List<Entry> entries) {
		List<Map<Ngramm, Integer>> grammFreqByClass = new ArrayList<>();
		List<Integer> objectsOfClass = new ArrayList<>();
		for (int c = 0; c < k; c++) {
			grammFreqByClass.add(new HashMap<>());
			objectsOfClass.add(0);
		}
		Set<Ngramm> gramms = new HashSet<>();
		int i = 0;
		for (Entry entry : entries) {
			Set<Ngramm> x = getNgramms(entry.x);
			gramms.addAll(x);
			int y = entry.y;
			for (Ngramm ngramm : x) {
				Map<Ngramm, Integer> classFreq = grammFreqByClass.get(y);
				classFreq.putIfAbsent(ngramm, 0);
				classFreq.put(ngramm, classFreq.get(ngramm) + 1);
			}
			objectsOfClass.set(y, objectsOfClass.get(y) + 1);
		}
		
		List<Double> classProb = new ArrayList<>();
		for (int c = 0; c < k; c++) {
			classProb.add(1.0 * objectsOfClass.get(c) / entries.size());
		}
		return new Model(classProb,
				(word, c) -> (grammFreqByClass.get(c).getOrDefault(word, 0) + alpha) /
						(objectsOfClass.get(c) + alpha * 2),
				objectsOfClass,
				gramms);
	}
	
	public double[] predict(List<Integer> object, Model model) {
		Set<Ngramm> x = getNgramms(object);
		double[] classProb = new double[k];
		for (int c = 0; c < k; c++) {
			if (model.objectsOfClass.get(c) == 0) {
				continue;
			}
			double curClassProb = Math.log(lambda[c] * model.classProb.get(c));
			for (Ngramm ngramm : model.ngramms) {
				double ngrammProb = model.getNgrammProb(ngramm, c);
				curClassProb += Math.log(x.contains(ngramm) ? ngrammProb : 1 - ngrammProb);
			}
			classProb[c] = curClassProb;
		}
		if (k != 2) {
			throw new AssertionError("expected two classes");
		}
		JniExpNorm jne = new JniExpNorm();
		double[] doubles = {jne.expNormA(classProb[0], classProb[1]), jne.expNormB(classProb[0], classProb[1])};
		if (Double.isNaN(doubles[0]) || Double.isNaN(doubles[1])) {
			if (classProb[0] < classProb[1]) {
				return new double[]{0, 1};
			} else {
				return new double[]{1, 0};
			}
		}
		return doubles;
	}
	
	private Set<Ngramm> getNgramms(List<Integer> words) {
		Set<Ngramm> ngramms = new HashSet<>();
		for (int i = 0; i < words.size() - ngrammCount; i++) {
			ngramms.add(new Ngramm(words.subList(i, i + ngrammCount)));
		}
		return ngramms;
	}
	
	public static class Model {
		List<Double> classProb;
		BiFunction<Ngramm, Integer, Double> wordProb;
		List<Integer> objectsOfClass;
		Set<Ngramm> ngramms;
		
		public Model(List<Double> classProb, BiFunction<Ngramm, Integer, Double> wordProb, List<Integer> objectsOfClass, Set<Ngramm> ngramms) {
			this.classProb = classProb;
			this.wordProb = wordProb;
			this.objectsOfClass = objectsOfClass;
			this.ngramms = ngramms;
		}
		
		public double getNgrammProb(Ngramm word, int c) {
			return wordProb.apply(word, c);
		}
	}
	
	public static class Ngramm {
		private final List<Integer> ngramm;
		
		public Ngramm(List<Integer> ngramm) {
			this.ngramm = ngramm;
		}
		
		@Override
		public boolean equals(Object o) {
			if (this == o) return true;
			if (!(o instanceof Ngramm)) return false;
			Ngramm ngramm1 = (Ngramm) o;
			return Objects.equals(ngramm, ngramm1.ngramm);
		}
		
		@Override
		public int hashCode() {
			return Objects.hash(ngramm);
		}
	}
}
