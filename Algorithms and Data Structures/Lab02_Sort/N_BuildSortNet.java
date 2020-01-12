import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;

public class N_BuildSortNet {
	
	private static int nearest2Power(int n) {
		if (n == 1 || n == 2 || n == 4 || n == 8 || n == 16) {
			return n;
		}
		return (1 << Integer.toBinaryString(n).length());
	}
	
	private static void net(int n, int p) {
		StringBuilder sb = new StringBuilder();
		int comps = 0;
		int layers = 0;
		if (p / 2 > 0) {
			// first layer
			layers = 1;
			StringBuilder comp = new StringBuilder();
			int localComps = 0;
			for (int i = 0; i < p; i += 2) {
				if (i + 2 <= n) {
					comp.append(i + 1).append(" ").append(i + 2).append(" ");
					localComps++;
				}
			}
			comps += localComps;
			sb.append(localComps).append(" ").append(comp);
			
			if (p / 4 > 0) {
				// second layer
				comp = new StringBuilder();
				localComps = 0;
				for (int i = 0; i < p / 4; i++) {
					int shift = 4 * i;
					for (int j = 0; j < 2; j++) {
						if (shift + 4 - j <= n) {
							comp.append(shift + j + 1).append(" ").append(shift + 4 - j).append(" ");
							localComps++;
						}
					}
				}
				comps += localComps;
				sb.append("\n").append(localComps).append(" ").append(comp).append("\n");
				
				// third layer
				layers = 3;
				comp = new StringBuilder();
				localComps = 0;
				for (int i = 0; i < p; i += 2) {
					if (i + 2 <= n) {
						comp.append(i + 1).append(" ").append(i + 2).append(" ");
						localComps++;
					}
				}
				comps += localComps;
				sb.append(localComps).append(" ").append(comp);
				
				if (p / 8 > 0) {
					// fourth layer
					comp = new StringBuilder();
					localComps = 0;
					for (int i = 0; i < p / 8; i++) {
						int shift = 8 * i;
						for (int j = 0; j < 4; j++) {
							if (shift + 8 - j <= n) {
								comp.append(shift + j + 1).append(" ").append(shift + 8 - j).append(" ");
								localComps++;
							}
						}
					}
					comps += localComps;
					sb.append("\n").append(localComps).append(" ").append(comp).append("\n");
					
					// fifth layer
					comp = new StringBuilder();
					localComps = 0;
					for (int i = 0; i < p / 4; i++) {
						int shift = 4 * i;
						for (int j = 0; j < 2; j++) {
							if (shift + j + 3 <= n) {
								comp.append(shift + j + 1).append(" ").append(shift + j + 3).append(" ");
								localComps++;
							}
						}
					}
					comps += localComps;
					sb.append(localComps).append(" ").append(comp).append("\n");
					
					// sixth layer
					layers = 6;
					comp = new StringBuilder();
					localComps = 0;
					for (int i = 0; i < p; i += 2) {
						if (i + 2 <= n) {
							comp.append(i + 1).append(" ").append(i + 2).append(" ");
							localComps++;
						}
					}
					comps += localComps;
					sb.append(localComps).append(" ").append(comp);
					
					if (p / 16 > 0) {
						// seventh layer
						comp = new StringBuilder();
						localComps = 0;
						for (int i = 0; i < p / 16; i++) {
							int shift = 16 * i;
							for (int j = 0; j < 8; j++) {
								if (shift + 16 - j <= n) {
									comp.append(shift + j + 1).append(" ").append(shift + 16 - j).append(" ");
									localComps++;
								}
							}
						}
						comps += localComps;
						sb.append("\n").append(localComps).append(" ").append(comp).append("\n");
						
						// eighth layer
						comp = new StringBuilder();
						localComps = 0;
						for (int i = 0; i < p / 8; i++) {
							int shift = 8 * i;
							for (int j = 0; j < 4; j++) {
								if (shift + j + 5 <= n) {
									comp.append(shift + j + 1).append(" ").append(shift + j + 5).append(" ");
									localComps++;
								}
							}
						}
						comps += localComps;
						sb.append(localComps).append(" ").append(comp).append("\n");
						
						// ninth layer
						comp = new StringBuilder();
						localComps = 0;
						for (int i = 0; i < p / 4; i++) {
							int shift = 4 * i;
							for (int j = 0; j < 2; j++) {
								if (shift + j + 3 <= n) {
									comp.append(shift + j + 1).append(" ").append(shift + j + 3).append(" ");
									localComps++;
								}
							}
						}
						comps += localComps;
						sb.append(localComps).append(" ").append(comp).append("\n");
						
						// tenth layer
						layers = 10;
						comp = new StringBuilder();
						localComps = 0;
						for (int i = 0; i < p; i += 2) {
							if (i + 2 <= n) {
								comp.append(i + 1).append(" ").append(i + 2).append(" ");
								localComps++;
							}
						}
						comps += localComps;
						sb.append(localComps).append(" ").append(comp);
					}
				}
			}
		}
		String[] layersS = sb.toString().trim().split("\n");
		svg = new StringBuilder("<svg height=\"500\" width=\"500\">");
		int coef = 50;
		int shift = 10;
		int curShift = 10;
		for (int i = 0; i < layersS.length; i++) {
			int[] layer = Arrays.stream(layersS[i].split(" +")).skip(1).mapToInt(Integer::parseInt).toArray();
			for (int j = 0; j < layer.length; j += 2) {
				svg.append(" <line x1=\"").append(i * coef + curShift).append("\" y1=\"").append(layer[j] * coef).append("\" x2=\"").append(i * coef + curShift).append("\" y2=\"").append(layer[j + 1] * coef).append("\" style=\"stroke:rgb(255,0,0);stroke-width:4\" />");
				curShift += shift;
			}
			System.out.println(Arrays.toString(layer));
		}
		svg.append("</svg>");
		System.out.println(n + " " + comps + " " + layers);
		String s = sb.toString();
		if (!s.isEmpty()) {
			System.out.println(sb.toString());
		}
	}
	
	static StringBuilder svg;
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		int n = Integer.parseInt(in.readLine());
		in.close();
		net(n, nearest2Power(n));
		System.out.println(svg);
	}
	
}
