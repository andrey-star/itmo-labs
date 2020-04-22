import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Arrays;

public class B_Horn {
	
	private static int[] clausesToCheck;
	
	private static int uniqueArgs(int[] arg) { // Returns unique arg index, or -1 if absent
		int ans = -1;
		int notIncluded = 0; // How many arguments in a clause
		for (int i = 0; i < arg.length; i++) { // Iterate over arguments
			if (arg[i] != -1) { // If this arg is used
				notIncluded++;
				ans = i;
			}
			
			if (notIncluded > 1) { // If > 1 argument in a clause go to next
				return -1;
			}
		}
		return ans;
	}
	
	private static boolean iterate(int[][] arg, int[][] values) {
		int k = arg.length;
		int n = arg[0].length;
		int uniqueArgIndex = -1;
		int uniqueArgValue = -1;
		for (int i = 0; i < k; i++) { // Iterate over clauses
			if (clausesToCheck[i] == 1) {
				int checkUniqueArgIndex = uniqueArgs(arg[i]);
				if (checkUniqueArgIndex == -1) {
					continue;
				}
				// If did not continue a unique argument was found
				// Get its value
				uniqueArgIndex = checkUniqueArgIndex;
				uniqueArgValue = arg[i][checkUniqueArgIndex] == 1 ? 1 : 0;
				break;
			}
		}
		if (uniqueArgIndex != -1) {
			for (int i = 0; i < k; i++) { // Iterate over clauses and update values
				if (clausesToCheck[i] == 1) {
					if (arg[i][uniqueArgIndex] == 1) { // If arg without NOT
						if (uniqueArgValue == 1) { // If arg value in clause is 1
							clausesToCheck[i] = 0;
						} else {
							arg[i][uniqueArgIndex] = -1; // If arg value in clause is 0, don't check afterwards
						}
						values[i][uniqueArgIndex] = uniqueArgValue;
					} else if (arg[i][uniqueArgIndex] == 0) { // If arg with NOT
						if (uniqueArgValue == 0) { // If arg value in clause is 1
							clausesToCheck[i] = 0;
						} else {
							arg[i][uniqueArgIndex] = -1; // If arg value in clause is 0, don't check afterwards
						}
						values[i][uniqueArgIndex] = uniqueArgValue ^ 1;
					} else { // If arg doesn't appear in close, don't check afterwards
						values[i][uniqueArgIndex] = 1;
						arg[i][uniqueArgIndex] = -1;
					}
				}
			}
			for (int i = 0; i < k; i++) { // Iterate over clauses
				if (clausesToCheck[i] == 1) {
					boolean hasAllZeroClause = true;
					for (int j = 0; j < n; j++) { // Iterate over arguments
						if (arg[i][j] != -1 && values[i][j] != 0) {
							hasAllZeroClause = false;
							break;
						}
					}
					if (hasAllZeroClause) {
						return false;
					}
				}
			}
			for (int i = 0; i < k; i++) { // Iterate over clauses
				if (clausesToCheck[i] == 1) {
					boolean isEqualToOne = true;
					for (int j = 0; j < n; j++) { // Iterate over arguments
						if (arg[i][j] != -1 && values[i][j] != 1) { // Has clause equal to one
							isEqualToOne = false;
							break;
						}
					}
					if (isEqualToOne) {
						clausesToCheck[i] = 0;
					}
				}
			}
			return iterate(arg, values);
		} else {
			return true;
		}
	}
	
	public static void main(String[] args) throws IOException {
		BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
		String[] line = in.readLine().trim().split(" ");
		int n = Integer.parseInt(line[0]);
		int k = Integer.parseInt(line[1]);
		int[][] arg = new int[k][n];
		int[][] values = new int[k][n];
		clausesToCheck = new int[k];
		Arrays.fill(clausesToCheck, 1);
		for (int[] a : values) {
			Arrays.fill(a, -1);
		}
		for (int i = 0; i < k; i++) {
			line = in.readLine().trim().split(" ");
			for (int j = 0; j < n; j++) {
				arg[i][j] = Integer.parseInt(line[j]);
			}
		}
		System.out.println(iterate(arg, values) ? "NO" : "YES");
	}
	
}
