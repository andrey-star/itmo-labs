package ru.ifmo.rain.starodubtsev.bank.main.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface RemotePerson extends Remote {
	
	String getFirstName() throws RemoteException;
	
	String getLastName() throws RemoteException;
	
	String getPassportId() throws RemoteException;
	
	RemoteAccount getAccount(String subId) throws RemoteException;
	
	RemoteAccount createAccount(String subId) throws RemoteException;
	
}
