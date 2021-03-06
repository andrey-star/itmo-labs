/**
 * <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-concurrent">Concurrent</a>
 * and <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-mapper">Mapper</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Andrey Starodubtsev
 */
module ru.ifmo.rain.starodubtsev.concurrent {
	requires info.kgeorgiy.java.advanced.concurrent;
	requires info.kgeorgiy.java.advanced.mapper;
	
	exports ru.ifmo.rain.starodubtsev.concurrent;
}