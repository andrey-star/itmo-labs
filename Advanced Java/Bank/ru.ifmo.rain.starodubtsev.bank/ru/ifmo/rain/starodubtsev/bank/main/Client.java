package ru.ifmo.rain.starodubtsev.bank.main;

import ru.ifmo.rain.starodubtsev.bank.main.bank.RemoteAccount;
import ru.ifmo.rain.starodubtsev.bank.main.bank.Bank;
import ru.ifmo.rain.starodubtsev.bank.main.bank.RemotePerson;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.util.Objects;

public class Client {
	
	private static final String USAGE = "Usage: Client <first name> <last name> <passport id> <account id> <amount>";
	
	public static void main(String[] args) {
		Objects.requireNonNull(args);
		if (args.length != 5) {
			System.out.println(USAGE);
			return;
		}
		String firstName = getArg(args, 0);
		String lastName = getArg(args, 1);
		String passportId = getArg(args, 2);
		String accountId = getArg(args, 3);
		long amount = getIntArg(args, 4);
		
		final Bank bank;
		try {
			bank = (Bank) Naming.lookup("//localhost/bank");
		} catch (final NotBoundException e) {
			System.out.println("Bank is not bound");
			return;
		} catch (final MalformedURLException e) {
			System.out.println("Bank URL is invalid");
			return;
		} catch (RemoteException e) {
			System.out.println("Could not find remote bank");
			return;
		}
		
		try {
			RemotePerson person = bank.getRemotePerson(passportId);
			if (person == null) {
				person = bank.createPerson(firstName, lastName, passportId);
			} else {
				if (!person.getFirstName().equals(firstName) || !person.getLastName()
						.equals(lastName)) {
					System.out.println("Invalid credentials for user with id " + passportId);
					return;
				}
			}
			RemoteAccount account = person.createAccount(accountId);
			System.out.println("Account id: " + account.getId());
			System.out.println("Money: " + account.getAmount());
			System.out.println(amount >= 0 ? "Adding money" : "Withdrawing money");
			account.addFunds(amount);
			System.out.println("Money: " + account.getAmount());
		} catch (RemoteException e) {
			System.out.println("An error occurred when communication with remote bank");
		}
	}
	
	private static String getArg(String[] args, int i) {
		return Objects.requireNonNull(args[i]);
	}
	
	private static int getIntArg(String[] args, int i) {
		return Integer.parseInt(getArg(args, i));
	}
}