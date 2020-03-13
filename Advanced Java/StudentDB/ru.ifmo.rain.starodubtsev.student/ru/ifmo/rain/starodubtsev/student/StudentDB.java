package ru.ifmo.rain.starodubtsev.student;

import info.kgeorgiy.java.advanced.student.*;

import java.util.*;
import java.util.function.*;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StudentDB implements AdvancedStudentGroupQuery {
	
	private final Comparator<Student> STUDENT_NAME_COMPARATOR = Comparator
			.comparing(Student::getLastName)
			.thenComparing(Student::getFirstName)
			.thenComparingInt(Student::getId);
	
	
	// StudentQuery //
	
	private <T, C extends Collection<T>, V> C mapToCollection(
			Collection<V> values,
			Function<V, T> mapper,
			Supplier<C> collectionFactory
	) {
		return values.stream().map(mapper).collect(Collectors.toCollection(collectionFactory));
	}
	
	private <T, V> List<T> mapToList(List<V> values, Function<V, T> mapper) {
		return mapToCollection(values, mapper, ArrayList::new);
	}
	
	private <T, V> Set<T> mapToSet(List<V> values, Function<V, T> mapper) {
		return mapToCollection(values, mapper, TreeSet::new);
	}
	
	private String fullName(Student student) {
		return student.getFirstName() + " " + student.getLastName();
	}
	
	@Override
	public List<String> getFullNames(List<Student> students) {
		return mapToList(students, this::fullName);
	}
	
	@Override
	public List<String> getFirstNames(List<Student> students) {
		return mapToList(students, Student::getFirstName);
	}
	
	@Override
	public List<String> getLastNames(List<Student> students) {
		return mapToList(students, Student::getLastName);
	}
	
	@Override
	public List<String> getGroups(List<Student> students) {
		return mapToList(students, Student::getGroup);
	}
	
	@Override
	public Set<String> getDistinctFirstNames(List<Student> students) {
		return mapToSet(students, Student::getFirstName);
	}
	
	@Override
	public String getMinStudentFirstName(List<Student> students) {
		return students.stream()
		               .min(Student::compareTo)
		               .map(Student::getFirstName)
		               .orElse("");
	}
	
	private <T> List<T> toSortedList(Stream<T> stream, Comparator<T> comparator) {
		return stream.sorted(comparator).collect(Collectors.toList());
	}
	
	@Override
	public List<Student> sortStudentsById(Collection<Student> students) {
		return toSortedList(students.stream(), Student::compareTo);
	}
	
	@Override
	public List<Student> sortStudentsByName(Collection<Student> students) {
		return toSortedList(students.stream(), STUDENT_NAME_COMPARATOR);
	}
	
	/**
	 * Returns a {@code List} of students, filtered by the provided {@code Predicate},
	 * and sorted by name.
	 */
	private List<Student> filterByPredicateAndSortByName(Collection<Student> students, Predicate<Student> predicate) {
		return toSortedList(students.stream().filter(predicate), STUDENT_NAME_COMPARATOR);
	}
	
	/**
	 * Returns a @{@code Predicate} capable of extracting a key based on the provided {@code keyExtractor},
	 * and checking if it is equal to {@code value}.
	 */
	private <T, K> Predicate<T> equalsPredicate(Function<T, K> keyExtractor, K value) {
		return student -> Objects.equals(keyExtractor.apply(student), value);
	}
	
	/**
	 * Returns a {@code List} of students, filtered by the provided {@code key},
	 * and sorted by name.
	 */
	private <K> List<Student> filterByKeyAndSortByName(Collection<Student> students, K key, Function<Student, K> keyExtractor) {
		return filterByPredicateAndSortByName(students, equalsPredicate(keyExtractor, key));
	}
	
	@Override
	public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
		return filterByKeyAndSortByName(students, name, Student::getFirstName);
	}
	
	@Override
	public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
		return filterByKeyAndSortByName(students, name, Student::getLastName);
	}
	
	@Override
	public List<Student> findStudentsByGroup(Collection<Student> students, String group) {
		return filterByKeyAndSortByName(students, group, Student::getGroup);
	}
	
	@Override
	public Map<String, String> findStudentNamesByGroup(Collection<Student> students, String group) {
		return students.stream()
		               .filter(equalsPredicate(Student::getGroup, group))
		               .collect(Collectors.toMap(Student::getLastName, Student::getFirstName,
				               BinaryOperator.minBy(String::compareTo)));
	}
	
	
	// StudentGroupQuery //
	
	/**
	 * Returns a {@code Stream} of entries, produced by the specified {@code Collector}.
	 */
	private <K, V> Stream<Map.Entry<K, V>>
	getEntriesFromStream(Stream<Student> studentStream, Collector<Student, ?, Map<K, V>> collector) {
		return studentStream.collect(collector).entrySet().stream();
	}
	
	
	/**
	 * Returns a {@code Stream} of entries with group name mapped to a {@code List} of associated students.
	 */
	private Stream<Map.Entry<String, List<Student>>>
	getGroupEntries(Stream<Student> studentStream) {
		return getEntriesFromStream(studentStream, Collectors.groupingBy(Student::getGroup));
	}
	
	/**
	 * Returns a {@code Stream} of entries with group name mapped to a {@code Set} of distinct student first names.
	 */
	private Stream<Map.Entry<String, Set<String>>>
	getGroupEntriesDistinct(Stream<Student> studentStream) {
		return getEntriesFromStream(studentStream, Collectors.groupingBy(Student::getGroup,
				Collectors.mapping(Student::getFirstName, Collectors.toSet())));
	}
	
	/**
	 * Returns a {@code List} of groups sorted by name
	 * with students within a group ordered by the provided {@code Comparator}.
	 */
	private List<Group> getSortedGroups(Collection<Student> students, Comparator<Student> studentComparator) {
		return toSortedList(getGroupEntries(students.stream().sorted(studentComparator))
				.map(e -> new Group(e.getKey(), e.getValue())), Comparator.comparing(Group::getName));
	}
	
	@Override
	public List<Group> getGroupsByName(Collection<Student> students) {
		return getSortedGroups(students, STUDENT_NAME_COMPARATOR);
	}
	
	@Override
	public List<Group> getGroupsById(Collection<Student> students) {
		return getSortedGroups(students, Student::compareTo);
	}
	
	/**
	 * Returns an {@code Optional} describing the maximum key in a map with {@code Collection} values,
	 * comparing by value first, then by key.
	 */
	private <K, V extends Collection<?>> Optional<K>
	maxKeyBy(Stream<Map.Entry<K, V>> entries, Comparator<V> byValue, Comparator<K> byKey) {
		return entries.max(Map.Entry.<K, V>comparingByValue(byValue)
				.thenComparing(Map.Entry.comparingByKey(byKey)))
		              .map(Map.Entry::getKey);
	}
	
	/**
	 * Returns the name of the largest group compared by associated {@code Collection},
	 * then by name.
	 * If no largest group is present, an empty {@code String} is returned.
	 */
	private <T, C extends Collection<T>> String getLargestGroupBy(
			Collection<Student> students,
			Function<Stream<Student>, Stream<Map.Entry<String, C>>> groupGetter,
			Comparator<C> comparator) {
		return maxKeyBy(groupGetter.apply(students.stream()), comparator,
				Collections.reverseOrder(String::compareTo))
				.orElse("");
	}
	
	@Override
	public String getLargestGroup(Collection<Student> students) {
		return getLargestGroupBy(students, this::getGroupEntries, Comparator.comparingInt(List::size));
	}
	
	@Override
	public String getLargestGroupFirstName(Collection<Student> students) {
		return getLargestGroupBy(students, this::getGroupEntriesDistinct, Comparator.comparingInt(Set::size));
	}
	
	
	// AdvancedStudentGroupQuery //
	
	@Override
	public String getMostPopularName(Collection<Student> students) {
		return maxKeyBy(getEntriesFromStream(students.stream(),
				Collectors.groupingBy(this::fullName, Collectors.mapping(Student::getGroup, Collectors.toSet()))),
				Comparator.comparingInt(Set::size),
				String::compareTo)
				.orElse("");
	}
	
	private List<String> indexedGet(Collection<Student> students, int[] indices, Function<Student, String> getter) {
		return Arrays.stream(indices).mapToObj(List.copyOf(students)::get).map(getter).collect(Collectors.toList());
	}
	
	@Override
	public List<String> getFirstNames(Collection<Student> students, int[] indices) {
		return indexedGet(students, indices, Student::getFirstName);
	}
	
	@Override
	public List<String> getLastNames(Collection<Student> students, int[] indices) {
		return indexedGet(students, indices, Student::getLastName);
	}
	
	@Override
	public List<String> getGroups(Collection<Student> students, int[] indices) {
		return indexedGet(students, indices, Student::getGroup);
	}
	
	@Override
	public List<String> getFullNames(Collection<Student> students, int[] indices) {
		return indexedGet(students, indices, this::fullName);
	}
	
}