import com.github.sh0nk.matplotlib4j.Plot;
import com.github.sh0nk.matplotlib4j.PythonExecutionException;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Plotter {
	
	public static void main(String[] args) throws IOException, PythonExecutionException {
		plotFromFile("res_all.csv");
	}
	
	public static void plotFromFile(String fileName) throws IOException, PythonExecutionException {
		BufferedReader in = new BufferedReader(new InputStreamReader(new FileInputStream("res/" + fileName)));
		String[][] data = in.lines().map(line -> line.split(",")).toArray(String[][]::new);
		String xLabel = data[0][0];
		String yLabel = data[0][1];
		
		data = Arrays.copyOfRange(data, 1, data.length);
		List<Double> xs = new ArrayList<>();
		List<Double> ys = new ArrayList<>();
		for (String[] point : data) {
			xs.add(Double.parseDouble(point[0]));
			ys.add(Double.parseDouble(point[1]));
		}
		linePlot(fileName, xLabel, yLabel, xs, ys);
	}
	
	public static void linePlot(String title, String xLabel, String yLabel, List<Double> xs, List<Double> ys) throws IOException, PythonExecutionException {
		Plot plt = Plot.create();
		plt.plot().add(xs, ys);
		plt.title(title);
		plt.xlabel(xLabel);
		plt.ylabel(yLabel);
		plt.show();
	}
	
	public static void scatterPlot(String xLabel, String yLabel, List<Double> xs, List<Double> ys, List<Boolean> color, double[] w) throws IOException, PythonExecutionException {
		double minX = xs.stream().min(Double::compare).orElse(0.0);
		double maxX = xs.stream().max(Double::compare).orElse(10.0);
		Plot plt = Plot.create();
		plt.plot()
				.add(IntStream.range(0, xs.size())
								.filter(color::get)
								.mapToObj(xs::get)
								.collect(Collectors.toList()),
						IntStream.range(0, ys.size())
								.filter(color::get)
								.mapToObj(ys::get)
								.collect(Collectors.toList()), "o")
				.color("red");
		plt.plot()
				.add(IntStream.range(0, xs.size())
								.filter(x -> !color.get(x))
								.mapToObj(xs::get)
								.collect(Collectors.toList()),
						IntStream.range(0, ys.size())
								.filter(x -> !color.get(x))
								.mapToObj(ys::get)
								.collect(Collectors.toList()), "o")
				.color("blue");
		
		plt.plot().add(Arrays.asList(minX, maxX), Arrays.asList(minX * -w[0] / w[1] -w[2] / w[1], maxX * -w[0] / w[1] + -w[2] / w[1]));
		plt.xlabel(xLabel);
		plt.ylabel(yLabel);
		plt.show();
	}
	
	public static void scatterPlot(String xLabel, String yLabel, List<Double> xs, List<Double> ys, double[] w) throws IOException, PythonExecutionException {
		Plot plt = Plot.create();
		plt.plot().add(xs, ys, "o");
		
		double min = xs.stream().min(Double::compareTo).orElse(0.0);
		double max = xs.stream().max(Double::compareTo).orElse(0.0);
		plt.plot().add(Arrays.asList(min, max), Arrays.asList(min * w[0] + w[1], max * w[0] + w[1]));
		plt.xlabel(xLabel);
		plt.ylabel(yLabel);
		plt.show();
	}
	
}
