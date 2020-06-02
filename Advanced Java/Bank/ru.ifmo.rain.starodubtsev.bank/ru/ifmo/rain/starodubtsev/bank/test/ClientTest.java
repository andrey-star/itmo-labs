package ru.ifmo.rain.starodubtsev.bank.test;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import ru.ifmo.rain.starodubtsev.bank.main.Client;
import ru.ifmo.rain.starodubtsev.bank.main.bank.RemoteAccount;
import ru.ifmo.rain.starodubtsev.bank.main.bank.Bank;
import ru.ifmo.rain.starodubtsev.bank.main.bank.RemotePerson;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.List;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.*;

class ClientTest {
	
	private static final Random RANDOM = new Random(4);
	private static RemoteBankTest rbt;
	private static Bank bank;
	
	@BeforeAll
	public static void init() {
		rbt = new RemoteBankTest();
	}
	
	@BeforeEach
	public void setUp() throws RemoteException, NotBoundException, MalformedURLException {
		rbt.setup();
		bank = (Bank) Naming.lookup("//localhost/bank");
	}
	
	@Test
	public void testPerson_new() throws RemoteException {
		for (int i = 0; i < 10; i++) {
			testPerson_new(ClientArgs.indexedClient(i));
		}
	}
	
	@Test
	public void testAccount_newAccountPersonExists() throws RemoteException {
		final ClientArgs ben = ClientArgs.ben();
		testPerson_new(ben);
		final RemotePerson person = rbt.testGetPerson_exists(ben.passportId, ben.firstName, ben.lastName, bank::getRemotePerson);
		for (int i = 1; i <= 10; i++) {
			ben.subId = "account" + i;
			ben.amount = RANDOM.nextInt();
			testAccountNew(ben, person);
		}
	}
	
	@Test
	public void testAccount_accountExistsPersonExists() throws RemoteException {
		final ClientArgs ben = ClientArgs.ben();
		testPerson_new(ben);
		final RemotePerson person = rbt.testGetPerson_exists(ben.passportId, ben.firstName, ben.lastName, bank::getRemotePerson);
		final List<String> accounts = rbt.createAccounts(person, 10);
		for (final String subId : accounts) {
			final RemoteAccount account = rbt.testGetAccount_exists(person, subId);
			account.addFunds(RANDOM.nextInt());
		}
		for (int i = 0; i < 15; i++) {
			final String subId = rbt.randomElement(accounts);
			final RemoteAccount account = rbt.testGetAccount_exists(person, subId);
			final long balance = account.getAmount();
			final int amount = RANDOM.nextInt();
			ben.subId = subId;
			ben.amount = amount;
			callClient(ben);
			final long expected = balance + amount;
			assertEquals(expected, account.getAmount());
			assertEquals(expected, rbt.testGetAccount_exists(person, subId).getAmount());
		}
	}
	
	@Test
	public void testPerson_existsValidCredentials() throws RemoteException {
		final ClientArgs ben = ClientArgs.ben();
		testPerson_new(ben);
		testAddMoney_personExists(ben, true);
	}
	
	@Test
	public void testPerson_existsInvalidName() throws RemoteException {
		final ClientArgs ben = ClientArgs.ben();
		testPerson_new(ben);
		ClientArgs invalidArgs = new ClientArgs(ben);
		invalidArgs.firstName = "Not" + ben.firstName;
		testAddMoney_personExists(invalidArgs, false);
		invalidArgs = new ClientArgs(ben);
		invalidArgs.lastName = "Not" + ben.lastName;
		testAddMoney_personExists(invalidArgs, false);
	}
	
	@Test
	public void test_InvalidArguments() {
		assertThrows(NullPointerException.class, () -> Client.main(null));
		callClient("Ben", null);
		callClient("Ben", "Dover", "0", null);
		assertNpe(null, "Dover", "0", "account", "0");
		assertNpe("Ben", null, "0", "account", "0");
		assertNpe("Ben", "Dover", null, "account", "0");
		assertNpe("Ben", "Dover", "0", null, "0");
		assertNfe("Ben", "Dover", "0", "account", "Nan");
	}
	
	private void testAccountNew(final ClientArgs clientArgs, final RemotePerson person) throws RemoteException {
		rbt.testAccount_absent(person, clientArgs.subId);
		callClient(clientArgs);
		final RemoteAccount account = rbt.testGetAccount_exists(person, clientArgs.subId);
		assertEquals(clientArgs.amount, account.getAmount());
	}
	
	private void testAddMoney_personExists(final ClientArgs args, final boolean validCredentials) throws RemoteException {
		final RemotePerson person;
		if (validCredentials) {
			person = rbt.testGetPerson_exists(args.passportId, args.firstName, args.lastName, bank::getRemotePerson);
		} else {
			person = bank.getRemotePerson(args.passportId);
		}
		assertNotNull(person);
		final RemoteAccount account = rbt.testGetAccount_exists(person, args.subId);
		final long balance = account.getAmount();
		callClient(args.firstName, args.lastName, args.passportId, args.subId, Long.toString(args.amount));
		final long expected = balance + (validCredentials ? args.amount : 0);
		assertEquals(expected, account.getAmount());
		assertEquals(expected, rbt.testGetAccount_exists(person, args.subId).getAmount());
	}
	
	private void testPerson_new(final ClientArgs clientArgs) throws RemoteException {
		rbt.testRemotePerson_absent(clientArgs.passportId);
		callClient(clientArgs);
		final RemotePerson person = rbt.testGetPerson_exists(clientArgs.passportId, clientArgs.firstName, clientArgs.lastName, bank::getRemotePerson);
		final RemoteAccount account = rbt.testGetAccount_exists(person, clientArgs.subId);
		assertEquals(clientArgs.amount, account.getAmount());
	}
	
	private void callClient(final ClientArgs client) {
		callClient(client.firstName, client.lastName, client.passportId, client.subId, Long.toString(client.amount));
	}
	
	private void callClient(final String... args) {
		Client.main(args);
	}
	
	private static class ClientArgs {
		String firstName;
		String lastName;
		String passportId;
		String subId;
		long amount;
		
		public ClientArgs(final String firstName, final String lastName, final String passportId, final String subId, final long amount) {
			this.firstName = firstName;
			this.lastName = lastName;
			this.passportId = passportId;
			this.subId = subId;
			this.amount = amount;
		}
		
		public ClientArgs(final ClientArgs args) {
			this.firstName = args.firstName;
			this.lastName = args.lastName;
			this.passportId = args.passportId;
			this.subId = args.subId;
			this.amount = args.amount;
		}
		
		private static ClientArgs indexedClient(final int i) {
			String id = Integer.toString(i);
			return new ClientArgs(id, id, id, "account" + i, RANDOM.nextInt());
		}
		
		private static ClientArgs ben() {
			return new ClientArgs("Ben", "Dover", "4014552637", "ben's_account", 75400);
		}
	}
	
	private void assertNpe(String... args) {
		assertThrows(NullPointerException.class, () -> callClient(args));
	}
	
	private void assertNfe(String... args) {
		assertThrows(NumberFormatException.class, () -> callClient(args));
	}
	
}