package ru.ifmo.rain.starodubtsev.bank.test;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;
import ru.ifmo.rain.starodubtsev.bank.main.Server;
import ru.ifmo.rain.starodubtsev.bank.main.bank.*;

import java.net.MalformedURLException;
import java.nio.charset.StandardCharsets;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

public class RemoteBankTest {
	
	private static final Random RANDOM = new Random(4);
	
	private static Bank bank;
	
	@BeforeEach
	public void setup() throws RemoteException, NotBoundException, MalformedURLException {
		Server.main(null);
		bank = (Bank) Naming.lookup("//localhost/bank");
	}
	
	@Test
	public void testCreatePerson_new() throws RemoteException {
		createPersons(10);
	}
	
	@RepeatedTest(10)
	public void testCreatePerson_exists_noOverwrite() throws RemoteException {
		final long[] ids = RANDOM.longs().distinct().filter(l -> l > 0).limit(10).toArray();
		for (final long idLong : ids) {
			final String id = Long.toString(idLong);
			testCreatePerson_new(id, id, id);
			testCreatePerson_exists_noOverwrite(id, id, "Not" + id, "Not" + id, id);
		}
	}
	
	@Test
	public void testGetLocalPerson() throws RemoteException {
		final List<RemotePerson> remotePersons = createPersons(10);
		for (final RemotePerson remotePerson : remotePersons) {
			testGetLocalPerson_exists(new Credentials(remotePerson));
		}
	}
	
	@Test
	public void testGetPerson_absent_null() throws RemoteException {
		for (int i = 0; i < 10; i++) {
			testRemotePerson_absent(Integer.toString(RANDOM.nextInt()));
			testLocalPerson_absent(Integer.toString(RANDOM.nextInt()));
		}
	}
	
	@Test
	public void testCreatePerson_null_error() {
		assertNpe(() -> testCreatePerson_new(null, null, "0"));
		assertNpe(() -> testCreatePerson_new("notnull", null, "0"));
		assertNpe(() -> testCreatePerson_new(null, "notnull", "0"));
	}
	
	@Test
	public void testCreateAccount_newPerson() throws RemoteException {
		final List<RemotePerson> persons = createPersons(1);
		for (final RemotePerson person : persons) {
			testCreateAccount_new(person, "first_account");
		}
	}
	
	@Test
	public void testCreateMultipleAccounts_newPerson() throws RemoteException {
		final List<RemotePerson> persons = createPersons(10);
		for (final RemotePerson person : persons) {
			createAccounts(person, 10);
		}
	}
	
	@Test
	public void testCreateMultipleAccounts_accountExists() throws RemoteException {
		final List<RemotePerson> persons = createPersons(10);
		final List<List<String>> accounts = createAccounts(persons, 10);
		for (int i = 0; i < persons.size(); i++) {
			final RemotePerson person = persons.get(i);
			final LocalPerson localPerson = testGetLocalPerson_exists(new Credentials(person));
			for (final String subId : accounts.get(i)) {
				testCreateAccount_alreadyExists(person, subId);
				testCreateAccount_alreadyExists(localPerson, subId);
			}
		}
	}
	
	@Test
	public void testGetLocalPersonAccount_equalToRemoteAccount() throws RemoteException {
		final List<RemotePerson> remotePersons = createPersons(10);
		final List<List<String>> accounts = createAccounts(remotePersons, 10);
		for (int i = 0; i < remotePersons.size(); i++) {
			final RemotePerson remotePerson = remotePersons.get(i);
			final LocalPerson localPerson = testGetLocalPerson_exists(new Credentials(remotePerson));
			final List<String> remoteAccounts = accounts.get(i);
			for (final String subId : remoteAccounts) {
				final RemoteAccount remoteAccount = testGetAccount_exists(remotePerson, subId);
				final LocalAccount localAccount = testGetAccount_exists(localPerson, subId);
				assertEquals(remoteAccount.getId(), localAccount.getId());
				assertEquals(remoteAccount.getAmount(), localAccount.getAmount());
			}
		}
	}
	
	@Test
	public void testGetAccount_absent_null() throws RemoteException {
		final RemotePerson remoteBen = ben();
		final LocalPerson localBen = localBen();
		for (int i = 0; i < 10; i++) {
			testAccount_absent(remoteBen, randomString(5));
			testAccount_absent(localBen, randomString(6));
		}
	}
	
	@Test
	public void testAddMoney_localAccountMultipleInstances() throws RemoteException {
		final RemotePerson ben = ben();
		final String subId = "first_account";
		testCreateAccount_new(ben, subId);
		final List<LocalPerson> localPersonCopies = new ArrayList<>();
		for (int i = 0; i < 10; i++) {
			localPersonCopies.add(testGetLocalPerson_exists(new Credentials(ben)));
		}
		for (int i = 0; i < 10; i++) {
			final List<Long> values = new ArrayList<>();
			for (final LocalPerson personCopy : localPersonCopies) {
				values.add(testGetAccount_exists(personCopy, subId).getAmount());
			}
			final int modifiedPersonIndex = RANDOM.nextInt(localPersonCopies.size());
			final LocalPerson localModifiedPerson = localPersonCopies.get(modifiedPersonIndex);
			testAddMoney_singleThread(new Credentials(localModifiedPerson), subId, true);
			for (int j = 0; j < localPersonCopies.size(); j++) {
				if (j == modifiedPersonIndex) continue;
				assertEquals(values.get(j), testGetAccount_exists(localPersonCopies.get(j), subId).getAmount());
			}
		}
	}
	
	@Test
	public void testAddMoney_remoteAccount_singleThread() throws RemoteException, InterruptedException {
		testAddMoney(false, false);
	}
	
	@Test
	public void testAddMoney_localAccount_singleThread() throws RemoteException, InterruptedException {
		testAddMoney(true, false);
	}
	
	@Test
	public void testAddMoney_remoteAccount_concurrent() throws RemoteException, InterruptedException {
		testAddMoney(false, true);
	}
	
	@Test
	public void testAddMoney_localAccount_concurrent() throws RemoteException, InterruptedException {
		testAddMoney(true, true);
	}
	
	private void testAddMoney(final boolean testLocalAccount, final boolean concurrent) throws RemoteException, InterruptedException {
		final int personsAmount = 5;
		final int accountsAmount = 5;
		final int threads = 10;
		
		final List<RemotePerson> persons = createPersons(personsAmount);
		final List<List<String>> personAccounts = createAccounts(persons, accountsAmount);
		final ExecutorService executorService = Executors.newFixedThreadPool(threads);
		for (int i = 0; i < 50; i++) {
			final int index = RANDOM.nextInt(persons.size());
			RemotePerson remotePerson = persons.get(index);
			LocalPerson localPerson = testGetLocalPerson_exists(new Credentials(persons.get(index)));
			String subId = randomElement(personAccounts.get(index));
			if (concurrent) {
				final RemoteAccount remoteAccount = testGetAccount_exists(remotePerson, subId);
				final LocalAccount localAccount = testGetAccount_exists(localPerson, subId);
				final long balance = localAccount.getAmount();
				final AtomicLong totalAdd = new AtomicLong();
				final CountDownLatch latch = new CountDownLatch(threads);
				RemoteConsumer<Integer> moneyAdd = testLocalAccount ? localAccount::addFunds : remoteAccount::addFunds;
				Stream.generate(() -> addMoneyConcurrent(moneyAdd, totalAdd, latch))
						.limit(threads)
						.forEach(executorService::submit);
				latch.await();
				assertEquals(balance + totalAdd.get(), testLocalAccount ? localAccount.getAmount() : remoteAccount.getAmount());
			} else {
				Credentials credentials = testLocalAccount ? new Credentials(localPerson) : new Credentials(remotePerson);
				testAddMoney_singleThread(credentials, subId, testLocalAccount);
			}
		}
		waitFor(executorService);
		if (testLocalAccount) testAccountsEmpty(persons, personAccounts);
	}
	
	private Runnable addMoneyConcurrent(RemoteConsumer<Integer> moneyAdd, AtomicLong totalAdd, CountDownLatch latch) {
		return () -> {
			try {
				final int add = RANDOM.nextInt();
				moneyAdd.accept(add);
				totalAdd.addAndGet(add);
			} catch (RemoteException e) {
				e.printStackTrace();
			} finally {
				latch.countDown();
			}
		};
	}
	
	private void testAccountsEmpty(final List<RemotePerson> persons, final List<List<String>> personAccounts) throws RemoteException {
		for (int i = 0; i < persons.size(); i++) {
			final RemotePerson person = persons.get(i);
			for (final String subId : personAccounts.get(i)) {
				final RemoteAccount account = testGetAccount_exists(person, subId);
				assertEquals(0, account.getAmount());
			}
		}
	}
	
	private void testAddMoney_singleThread(final Credentials credentials, final String subId, final boolean local) throws RemoteException {
		final RemoteAccount remoteAccount = testGetAccount_exists(testGetRemotePerson_exists(credentials), subId);
		final long oldBalance = remoteAccount.getAmount();
		final int add = RANDOM.nextInt();
		if (local) {
			LocalAccount localAccount = testGetAccount_exists(testGetLocalPerson_exists(credentials), subId);
			localAccount.addFunds(add);
			assertEquals(oldBalance + add, localAccount.getAmount());
			RemoteAccount updatedRemoteAccount = testGetAccount_exists(testGetRemotePerson_exists(credentials), subId);
			assertEquals(oldBalance, updatedRemoteAccount.getAmount());
		} else {
			remoteAccount.addFunds(add);
			assertEquals(oldBalance + add, remoteAccount.getAmount());
		}
	}
	
	private void testCreatePerson_exists_noOverwrite(final String oldFirstName, final String oldLastName,
	                                                 final String newFirstName, final String newLastName,
	                                                 final String passportId) throws RemoteException {
		testGetPerson_exists(passportId, oldFirstName, oldLastName, bank::getRemotePerson);
		bank.createPerson(newFirstName, newLastName, passportId);
		testGetPerson_exists(passportId, oldFirstName, oldLastName, bank::getRemotePerson);
	}
	
	void testRemotePerson_absent(final String passportId) throws RemoteException {
		assertNull(bank.getRemotePerson(passportId));
	}
	
	private void testLocalPerson_absent(final String passportId) throws RemoteException {
		assertNull(bank.getLocalPerson(passportId));
	}
	
	private RemotePerson testGetRemotePerson_exists(final Credentials credentials) throws RemoteException {
		final RemotePerson person = bank.getRemotePerson(credentials.getPassportId());
		testPerson(new Credentials(person), credentials);
		return person;
	}
	
	private LocalPerson testGetLocalPerson_exists(final Credentials credentials) throws RemoteException {
		final LocalPerson person = bank.getLocalPerson(credentials.getPassportId());
		testPerson(new Credentials(person), credentials);
		return person;
	}
	
	private List<RemotePerson> createPersons(final int amount) throws RemoteException {
		final List<RemotePerson> persons = new ArrayList<>();
		for (int i = 1; i <= amount; i++) {
			String passportId = Integer.toString(i);
			persons.add(testCreatePerson_new(passportId, passportId, passportId));
		}
		return persons;
	}
	
	private RemotePerson testCreatePerson_new(final String firstName, final String lastName, final String passportId) throws RemoteException {
		assertNull(bank.getRemotePerson(passportId));
		bank.createPerson(firstName, lastName, passportId);
		return testGetPerson_exists(passportId, firstName, lastName, bank::getRemotePerson);
	}
	
	RemotePerson testGetPerson_exists(final String passportId, final String expectedFirstName, final String expectedLastName, final RemoteFunction<String, RemotePerson> personGetter) throws RemoteException {
		final RemotePerson person = personGetter.apply(passportId);
		testPerson(new Credentials(person), new Credentials(expectedFirstName, expectedLastName, passportId));
		return person;
	}
	
	private List<List<String>> createAccounts(final List<RemotePerson> persons, final int accountsAmount) throws RemoteException {
		final List<List<String>> personAccounts = new ArrayList<>();
		for (final RemotePerson person : persons) {
			personAccounts.add(createAccounts(person, accountsAmount));
		}
		return personAccounts;
	}
	
	List<String> createAccounts(final RemotePerson person, final int amount) throws RemoteException {
		final List<String> subIds = new ArrayList<>();
		for (int j = 1; j <= amount; j++) {
			final String subId = "account" + j;
			testCreateAccount_new(person, subId);
			subIds.add(subId);
		}
		return subIds;
	}
	
	private void testCreateAccount_alreadyExists(final RemotePerson person, final String subId) throws RemoteException {
		assertNotNull(person.getAccount(subId));
		assertEquals(person.createAccount(subId), testGetAccount_exists(person, subId));
	}
	
	private void testCreateAccount_alreadyExists(final LocalPerson person, final String subId) {
		assertNotNull(person.getAccount(subId));
		assertEquals(person.createAccount(subId), testGetAccount_exists(person, subId));
	}
	
	void testCreateAccount_new(final RemotePerson person, final String subId) throws RemoteException {
		assertNull(person.getAccount(subId));
		person.createAccount(subId);
		final RemoteAccount account = testGetAccount_exists(person, subId);
		assertEquals(0, account.getAmount());
	}
	
	RemoteAccount testGetAccount_exists(final RemotePerson person, final String subId) throws RemoteException {
		final RemoteAccount account = person.getAccount(subId);
		assertNotNull(account);
		assertEquals(getAccountId(person.getPassportId(), subId), account.getId());
		return account;
	}
	
	LocalAccount testGetAccount_exists(final LocalPerson person, final String subId) {
		final LocalAccount account = person.getAccount(subId);
		assertNotNull(account);
		assertEquals(getAccountId(person.getPassportId(), subId), account.getId());
		return account;
	}
	
	void testAccount_absent(final RemotePerson person, final String subId) throws RemoteException {
		assertNull(person.getAccount(subId));
	}
	
	void testAccount_absent(final LocalPerson person, final String subId) {
		assertNull(person.getAccount(subId));
	}
	
	private void testPerson(final Credentials actual, final Credentials expected) {
		assertNotNull(actual);
		assertEquals(expected.getFirstName(), actual.getFirstName());
		assertEquals(expected.getLastName(), actual.getLastName());
		assertEquals(expected.getPassportId(), actual.getPassportId());
	}
	
	private String getAccountId(final String passportId, final String subId) {
		return passportId + ":" + subId;
	}
	
	private RemotePerson ben() throws RemoteException {
		final String passportId = "4014552637";
		RemotePerson ben = bank.getRemotePerson(passportId);
		if (ben == null) {
			ben = testCreatePerson_new("Ben", "Dover", passportId);
		}
		return ben;
	}
	
	private LocalPerson localBen() throws RemoteException {
		final RemotePerson ben = ben();
		return testGetLocalPerson_exists(new Credentials(ben));
	}
	
	<E> E randomElement(final List<E> persons) {
		assertTrue(persons.size() > 0);
		return persons.get(RANDOM.nextInt(persons.size()));
	}
	
	String randomString(final int length) {
		final byte[] array = new byte[length];
		RANDOM.nextBytes(array);
		return new String(array, StandardCharsets.UTF_8);
	}
	
	private void waitFor(ExecutorService executorService) throws InterruptedException {
		executorService.shutdown();
		if (!executorService.awaitTermination(100, TimeUnit.SECONDS)) {
			fail("Executor service did not respond in time");
		}
	}
	
	private void assertNpe(Executable r) {
		assertThrows(NullPointerException.class, r);
	}
	
	
	@FunctionalInterface
	interface RemoteFunction<T, R> {
		R apply(T t) throws RemoteException;
	}
	
	@FunctionalInterface
	interface RemoteConsumer<T> {
		void accept(T t) throws RemoteException;
	}
	
	private static class Credentials {
		String firstName;
		String lastName;
		String passportId;
		
		public Credentials(String firstName, String lastName, String passportId) {
			this.firstName = firstName;
			this.lastName = lastName;
			this.passportId = passportId;
		}
		
		public Credentials(RemotePerson p) throws RemoteException {
			this(p.getFirstName(), p.getLastName(), p.getPassportId());
		}
		
		public Credentials(LocalPerson p) {
			this(p.getFirstName(), p.getLastName(), p.getPassportId());
		}
		
		public String getFirstName() {
			return firstName;
		}
		
		public String getLastName() {
			return lastName;
		}
		
		public String getPassportId() {
			return passportId;
		}
	}
}