import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

public abstract class AbstractTask {
	
	private final String name;
	
	public AbstractTask(String name) {
		this.name = name;
	}
	
	public void test() throws IOException {
		List<Path> tests = Files.list(Path.of(name)).collect(Collectors.toList());
		for (Path test : tests) {
			test(test);
		}
	}
	
	protected abstract void test(Path test) throws FileNotFoundException;
	
	public void test(int test) throws IOException {
		List<Path> tests = Files.list(Path.of(name)).collect(Collectors.toList());
		test(tests.get(test));
	}
	
}
