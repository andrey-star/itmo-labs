package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.io.UncheckedIOException;
import java.rmi.Remote;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

public class RemoteBank implements Bank {
	
	private final int port;
	private final Map<String, RemotePerson> persons = new ConcurrentHashMap<>();
	private final Map<String, RemoteAccount> accounts = new ConcurrentHashMap<>();
	
	public RemoteBank(final int port) {
		this.port = port;
	}
	
	@Override
	public RemotePerson createPerson(final String firstName, final String lastName, final String passportId) throws RemoteException {
		Objects.requireNonNull(firstName);
		Objects.requireNonNull(lastName);
		return createRemote(persons, passportId, pId -> new RemotePersonImpl(firstName, lastName, pId, this));
	}
	
	@Override
	public RemotePerson getRemotePerson(final String passportId) {
		Logger.info("Retrieving remote person " + passportId);
		return persons.get(passportId);
	}
	
	@Override
	public LocalPerson getLocalPerson(final String passportId) throws RemoteException {
		Logger.info("Retrieving local person " + passportId);
		final RemotePersonImpl remotePerson = (RemotePersonImpl) getRemotePerson(passportId);
		if (remotePerson == null) {
			return null;
		}
		return new LocalPerson(remotePerson, getAccounts(remotePerson));
	}
	
	RemoteAccount createAccount(RemotePersonImpl person, String subId) throws RemoteException {
		return createRemote(accounts, person.getAccountId(subId), RemoteAccountImpl::new);
	}
	
	RemoteAccount getAccount(RemotePersonImpl person, String subId) {
		return accounts.get(person.getAccountId(subId));
	}
	
	private Map<String, RemoteAccount> getAccounts(RemotePersonImpl person) {
		return person.getAccountSubIds().stream()
				.collect(Collectors.toConcurrentMap(Function.identity(),
						subId -> accounts.get(person.getAccountId(subId))));
	}
	
	private <T extends Remote> T createRemote(Map<String, T> map, String key, Function<String, T> f) throws RemoteException {
		return tryRemote(() -> map.computeIfAbsent(key, k -> tryExport(f.apply(k))));
	}
	
	private <T> T tryRemote(RemoteCallable<T> exportTask) throws RemoteException {
		try {
			return exportTask.call();
		} catch (UncheckedIOException e) {
			throw (RemoteException) e.getCause();
		}
	}
	
	private <T extends Remote> T tryExport(T object) {
		try {
			UnicastRemoteObject.exportObject(object, port);
			return object;
		} catch (RemoteException e) {
			throw new UncheckedIOException(e);
		}
	}
	
	private interface RemoteCallable<T> {
		T call() throws RemoteException;
	}
	
}
