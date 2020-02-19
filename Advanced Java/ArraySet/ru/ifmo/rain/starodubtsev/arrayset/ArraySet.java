package ru.ifmo.rain.starodubtsev.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {
	
	public final List<E> elements;
	private final Comparator<? super E> comparator;
	
	public ArraySet() {
		this.elements = List.of();
		this.comparator = null;
	}
	
	public ArraySet(Collection<? extends E> elements) {
		this.elements = List.copyOf(new TreeSet<>(elements));
		this.comparator = null;
	}
	
	public ArraySet(Collection<? extends E> elements, Comparator<? super E> comparator) {
		Set<E> set = new TreeSet<>(comparator);
		set.addAll(elements);
		this.elements = List.copyOf(set);
		this.comparator = comparator;
	}
	
	private ArraySet(List<E> sortedElements, Comparator<? super E> comparator) {
		this.elements = sortedElements;
		this.comparator = comparator;
	}
	
	private UnsupportedOperationException uoe() {
		return new UnsupportedOperationException();
	}
	
	private E elementAt(int index) {
		if (index < 0 || index >= elements.size()) {
			return null;
		}
		return elements.get(index);
	}
	
	private int getIndex(E e) {
		return Collections.binarySearch(elements, e, comparator);
	}
	
	@Override
	public boolean contains(Object o) {
		@SuppressWarnings("unchecked")
		E e = (E) o;
		int index = getIndex(e);
		return index >= 0;
	}
	
	private int lowerIndex(E e, boolean inclusive) {
		int index = getIndex(e);
		if (index < 0) {
			index = -(index + 1) - 1;
		} else if (!inclusive) {
			index--;
		}
		return index;
	}
	
	private int higherIndex(E e, boolean inclusive) {
		int index = getIndex(e);
		if (index < 0) {
			index = -(index + 1);
		} else if (!inclusive) {
			index++;
		}
		return index;
	}
	
	@Override
	public E lower(E e) {
		return elementAt(lowerIndex(e, false));
	}
	
	@Override
	public E floor(E e) {
		return elementAt(lowerIndex(e, true));
	}
	
	@Override
	public E ceiling(E e) {
		return elementAt(higherIndex(e, true));
	}
	
	@Override
	public E higher(E e) {
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
	
	private NavigableSet<E> subSetNoThrow(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
		int fromIndex = higherIndex(fromElement, fromInclusive);
		int toIndex = lowerIndex(toElement, toInclusive);
		if (fromIndex > toIndex) {
			return new ArraySet<>(Collections.emptyList(), comparator);
		}
		return new ArraySet<>(elements.subList(fromIndex, toIndex + 1), comparator);
	}
	
	@Override
	public NavigableSet<E> subSet(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
		if (compare(fromElement, toElement) > 0) {
			throw new IllegalArgumentException("fromKey > toKey");
		}
		return subSetNoThrow(fromElement, fromInclusive, toElement, toInclusive);
	}
	
	@Override
	public NavigableSet<E> headSet(E toElement, boolean inclusive) {
		if (isEmpty()) {
			return this;
		}
		return subSetNoThrow(first(), true, toElement, inclusive);
	}
	
	@Override
	public NavigableSet<E> tailSet(E fromElement, boolean inclusive) {
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
	public SortedSet<E> subSet(E fromElement, E toElement) {
		return subSet(fromElement, true, toElement, false);
	}
	
	@Override
	public SortedSet<E> headSet(E toElement) {
		return headSet(toElement, false);
	}
	
	@Override
	public SortedSet<E> tailSet(E fromElement) {
		return tailSet(fromElement, true);
	}
	
	@Override
	public E first() {
		E e = elementAt(0);
		if (e == null) {
			throw new NoSuchElementException();
		}
		return e;
	}
	
	@Override
	public E last() {
		E e = elementAt(size() - 1);
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
	final int compare(E e1, E e2) {
		return comparator == null ? ((Comparable<? super E>) e1).compareTo(e2)
				: comparator.compare(e1, e2);
	}
	// unnecessary
	private static class DescendingList<E> extends AbstractList<E> {
		
		private final List<E> elements;
		private boolean reversed;
		
		DescendingList(List<E> elements) {
			if (elements.getClass() == DescendingList.class) { // to avoid stacking DescendingLists
				DescendingList<E> els = (DescendingList<E>) elements;
				this.elements = els.elements;
				this.reversed = !els.reversed;
			} else {
				this.elements = elements;
				this.reversed = true;
			}
		}
		
		@Override
		public E get(int index) {
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

