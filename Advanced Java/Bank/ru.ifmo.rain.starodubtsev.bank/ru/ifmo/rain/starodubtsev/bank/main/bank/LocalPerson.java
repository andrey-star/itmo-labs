package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.rmi.RemoteException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class LocalPerson extends AbstractPerson {
	
	protected final Map<String, LocalAccount> accounts;
	
	LocalPerson(final RemotePerson person, Map<String, RemoteAccount> remoteAccounts) throws RemoteException {
		super(person.getFirstName(), person.getLastName(), person.getPassportId());
		accounts = new ConcurrentHashMap<>();
		for (final var account : remoteAccounts.entrySet()) {
			accounts.put(account.getKey(), new LocalAccount(account.getValue()));
		}
	}
	
	public LocalAccount createAccount(String subId) {
		return accounts.computeIfAbsent(subId, sId -> new LocalAccount(getAccountId(sId)));
	}
	
	public LocalAccount getAccount(String subId) {
		return accounts.get(subId);
	}
	
}
