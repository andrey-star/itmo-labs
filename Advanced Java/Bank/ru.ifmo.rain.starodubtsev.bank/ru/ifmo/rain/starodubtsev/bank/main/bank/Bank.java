package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
	
	RemotePerson createPerson(String firstName, String lastName, String passportId) throws RemoteException;
	
	RemotePerson getRemotePerson(String passportId) throws RemoteException;
	
	LocalPerson getLocalPerson(String passportId) throws RemoteException;
	
}
