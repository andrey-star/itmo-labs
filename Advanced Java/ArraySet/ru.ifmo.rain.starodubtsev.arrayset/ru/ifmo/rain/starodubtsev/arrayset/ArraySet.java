package ru.ifmo.rain.starodubtsev.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {
	
	public final List<E> elements;
	private final Comparator<? super E> comparator;
	
	public ArraySet() {
		this.elements = List.of();
		this.comparator = null;
	}
	
	public ArraySet(final Collection<? extends E> elements) {
		this.elements = List.copyOf(new TreeSet<>(elements));
		this.comparator = null;
	}
	
	public ArraySet(final Collection<? extends E> elements, final Comparator<? super E> comparator) {
		final Set<E> set = new TreeSet<>(comparator);
		set.addAll(elements);
		this.elements = List.copyOf(set);
		this.comparator = comparator;
	}
	
	private ArraySet(final List<E> sortedElements, final Comparator<? super E> comparator) {
		this.elements = sortedElements;
		this.comparator = comparator;
	}
	
	private UnsupportedOperationException uoe() {
		return new UnsupportedOperationException();
	}
	
	private E elementAt(final int index) {
		if (index < 0 || index >= elements.size()) {
			return null;
		}
		return elements.get(index);
	}
	
	private int getIndex(final E e) {
		return Collections.binarySearch(elements, e, comparator);
	}
	
	@Override
	public boolean contains(final Object o) {
		@SuppressWarnings("unchecked") final E e = (E) o;
		final int index = getIndex(e);
		return index >= 0;
	}
	
	private int lowerIndex(final E e, final boolean inclusive) {
		int index = getIndex(e);
		if (index < 0) {
			index = -(index + 1) - 1;
		} else if (!inclusive) {
			index--;
		}
		return index;
	}
	
	private int higherIndex(final E e, final boolean inclusive) {
		int index = getIndex(e);
		if (index < 0) {
			index = -(index + 1);
		} else if (!inclusive) {
			index++;
		}
		return index;
	}
	
	@Override
	public E lower(final E e) {
		return elementAt(lowerIndex(e, false));
	}
	
	@Override
	public E floor(final E e) {
		return elementAt(lowerIndex(e, true));
	}
	
	@Override
	public E ceiling(final E e) {
		return elementAt(higherIndex(e, true));
	}
	
	@Override
	public E higher(final E e) {
		return elementAt(higherIndex(e, false));
	}
	
	@Override
	public E pollFirst() {
		throw uoe();
	}
	
	@Override
	public E pollLast() {
		throw uoe();
	}
	
	@Override
	public Iterator<E> iterator() {
		return elements.iterator();
	}
	
	@Override
	public NavigableSet<E> descendingSet() {
		return new ArraySet<>(new DescendingList<>(elements), Collections.reverseOrder(comparator));
	}
	
	@Override
	public Iterator<E> descendingIterator() {
		return descendingSet().iterator();
	}
	
	private NavigableSet<E> subSetNoThrow(final E fromElement, final boolean fromInclusive, final E toElement, final boolean toInclusive) {
		final int fromIndex = higherIndex(fromElement, fromInclusive);
		final int toIndex = lowerIndex(toElement, toInclusive);
		if (fromIndex > toIndex) {
			return new ArraySet<>(Collections.emptyList(), comparator);
		}
		return new ArraySet<>(elements.subList(fromIndex, toIndex + 1), comparator);
	}
	
	@Override
	public NavigableSet<E> subSet(final E fromElement, final boolean fromInclusive, final E toElement, final boolean toInclusive) {
		if (compare(fromElement, toElement) > 0) {
			throw new IllegalArgumentException("fromKey > toKey");
		}
		return subSetNoThrow(fromElement, fromInclusive, toElement, toInclusive);
	}
	
	@Override
	public NavigableSet<E> headSet(final E toElement, final boolean inclusive) {
		if (isEmpty()) {
			return this;
		}
		return subSetNoThrow(first(), true, toElement, inclusive);
	}
	
	@Override
	public NavigableSet<E> tailSet(final E fromElement, final boolean inclusive) {
		if (isEmpty()) {
			return this;
		}
		return subSetNoThrow(fromElement, inclusive, last(), true);
	}
	
	@Override
	public Comparator<? super E> comparator() {
		if (comparator == Comparator.naturalOrder()) {
			return null;
		}
		return comparator;
	}
	
	@Override
	public SortedSet<E> subSet(final E fromElement, final E toElement) {
		return subSet(fromElement, true, toElement, false);
	}
	
	@Override
	public SortedSet<E> headSet(final E toElement) {
		return headSet(toElement, false);
	}
	
	@Override
	public SortedSet<E> tailSet(final E fromElement) {
		return tailSet(fromElement, true);
	}
	
	@Override
	public E first() {
		final E e = elementAt(0);
		if (e == null) {
			throw new NoSuchElementException();
		}
		return e;
	}
	
	@Override
	public E last() {
		final E e = elementAt(size() - 1);
		if (e == null) {
			throw new NoSuchElementException();
		}
		return e;
	}
	
	@Override
	public int size() {
		return elements.size();
	}
	
	@SuppressWarnings("unchecked")
	final int compare(final E e1, final E e2) {
		return comparator == null ? ((Comparable<? super E>) e1).compareTo(e2)
				: comparator.compare(e1, e2);
	}
	
	@Override
	public String toString() {
		return super.toString();
	}
	
	private static class DescendingList<E> extends AbstractList<E> {
		
		private final List<E> elements;
		private final boolean reversed;
		
		DescendingList(final List<E> elements) {
			if (elements.getClass() == DescendingList.class) { // to avoid stacking DescendingLists
				final DescendingList<E> els = (DescendingList<E>) elements;
				this.elements = els.elements;
				this.reversed = !els.reversed;
			} else {
				this.elements = elements;
				this.reversed = true;
			}
		}
		
		@Override
		public E get(final int index) {
			if (reversed) {
				return elements.get(size() - index - 1);
			}
			return elements.get(index);
		}
		
		@Override
		public int size() {
			return elements.size();
		}
	}
}
