module ru.ifmo.rain.starodubtsev.bank {
	requires java.rmi;
	
	exports ru.ifmo.rain.starodubtsev.bank.main;
	exports ru.ifmo.rain.starodubtsev.bank.main.bank to java.rmi;
	
	requires org.junit.jupiter;
	requires org.junit.platform.launcher;
	exports ru.ifmo.rain.starodubtsev.bank.test;
	opens ru.ifmo.rain.starodubtsev.bank.test to org.junit.jupiter, org.junit.platform.commons;
}