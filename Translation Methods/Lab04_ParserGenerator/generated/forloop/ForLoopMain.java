package forloop;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.Objects;

public class ForLoopMain {
	
	public static void main(String[] args) throws IOException {
		if (args == null || args.length != 1) {
			throw new IllegalArgumentException("Usage: ForLoopMain <input file>");
		}
		try {
			Path inputFile = Path.of(Objects.requireNonNull(args[0]));
			if (!Files.isRegularFile(inputFile)) {
				throw new IllegalArgumentException("Expected the input to be a file");
			}
			String inputFileName = inputFile.getFileName().toString().split("\\.")[0];
			var S = new ForLoopParser(new ForLoopLexer(Files.readString(inputFile))).S();
			System.out.println(S.val);
			var gv = new ForLoopGV();
			gv.generateImage(gv.getGraph(S._node), inputFile.getParent(), inputFileName);
			System.out.println("Tree generated at '%s\\%s.pdf'"
			.formatted(inputFile.getParent().toString(), inputFileName));
		} catch (InvalidPathException e) {
			System.err.println("Invalid input file path");
		}
	}
	
}