package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.rmi.RemoteException;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class RemotePersonImpl extends AbstractPerson implements RemotePerson {
	
	private final RemoteBank remoteBank;
	private final Set<String> accountSubIds;
	
	RemotePersonImpl(final String firstName, final String lastName, final String passportId, final RemoteBank bank) {
		super(firstName, lastName, passportId);
		this.remoteBank = bank;
		this.accountSubIds = ConcurrentHashMap.newKeySet();
	}
	
	@Override
	public RemoteAccount createAccount(String subId) throws RemoteException {
		RemoteAccount account = remoteBank.createAccount(this, subId);
		accountSubIds.add(subId);
		return account;
	}
	
	@Override
	public RemoteAccount getAccount(String subId) {
		return remoteBank.getAccount(this, subId);
	}
	
	public Set<String> getAccountSubIds() {
		return accountSubIds;
	}
}
